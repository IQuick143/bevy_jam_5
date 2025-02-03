use super::error::*;
use super::*;

use itertools::Itertools as _;

impl LevelBuilder {
	pub const PLACEHOLDER_LEVEL_NAME: &'static str = "NAME_MISSING";

	pub fn new() -> Self {
		Self {
			name: None,
			hint: None,
			vertices: Vec::new(),
			cycles: Vec::new(),
			declared_links: Vec::new(),
			declared_one_way_links: Vec::new(),
		}
	}

	pub fn set_level_name(&mut self, name: String) -> Result<(), LevelBuilderError> {
		if self.name.is_some() {
			return Err(LevelBuilderError::LevelNameAlreadySet);
		}
		self.name = Some(name);
		Ok(())
	}

	pub fn set_level_hint(&mut self, hint: String) -> Result<(), LevelBuilderError> {
		if self.hint.is_some() {
			return Err(LevelBuilderError::LevelHintAlreadySet);
		}
		self.hint = Some(hint);
		Ok(())
	}

	pub fn add_vertex(&mut self) -> Result<usize, LevelBuilderError> {
		self.vertices.push(IntermediateVertexData {
			position: IntermediateVertexPosition::Free,
			object: None,
			glyph: None,
			color_label_appearence: None,
		});
		Ok(self.vertices.len() - 1)
	}

	pub fn add_cycle(
		&mut self,
		turnability: CycleTurnability,
		vertex_indices: impl IntoIterator<Item = usize>,
	) -> Result<usize, LevelBuilderError> {
		let vertex_indices = vertex_indices.into_iter().collect::<Vec<_>>();
		if let Some(i) = vertex_indices.iter().duplicates().next() {
			return Err(LevelBuilderError::RepeatingVertexInCycle(*i));
		}
		if let Some(i) = vertex_indices
			.iter()
			.copied()
			.find(|&i| i >= self.vertices.len())
		{
			return Err(LevelBuilderError::VertexIndexOutOfRange(i));
		}
		// Add the cycle entry
		self.cycles.push(IntermediateCycleData {
			placement: None,
			center_sprite_position: None,
			vertex_indices,
			turnability,
			linked_cycle: IntermediateLinkStatus::None,
			outgoing_one_way_links: Vec::new(),
		});
		Ok(self.cycles.len() - 1)
	}

	pub fn set_object(
		&mut self,
		vertex_index: usize,
		object: ObjectData,
	) -> Result<(), LevelBuilderError> {
		if vertex_index >= self.vertices.len() {
			return Err(LevelBuilderError::VertexIndexOutOfRange(vertex_index));
		}
		self.vertices[vertex_index].object = Some(object);
		Ok(())
	}

	pub fn set_glyph(
		&mut self,
		vertex_index: usize,
		glyph: GlyphData,
	) -> Result<(), LevelBuilderError> {
		if vertex_index >= self.vertices.len() {
			return Err(LevelBuilderError::VertexIndexOutOfRange(vertex_index));
		}
		self.vertices[vertex_index].glyph = Some(glyph);
		Ok(())
	}

	/// Links two cycles. The link is both symmetric and transitive.
	/// ## Notes
	/// This function only has basic safety guarantee.
	/// If it fails, the two cycles may become partially linked.
	/// The builder as a whole should remain in a valid state.
	pub fn link_cycles(
		&mut self,
		source_cycle: usize,
		dest_cycle: usize,
		direction: LinkedCycleDirection,
	) -> Result<(), LevelBuilderError> {
		if source_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(source_cycle));
		}
		if dest_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(dest_cycle));
		}
		// Add the link symmetrically, in both directions
		self.add_cycle_link(source_cycle, dest_cycle, direction)?;
		self.declared_links.push(DeclaredLinkData {
			source_cycle,
			dest_cycle,
			direction,
		});
		Ok(())
	}

	/// Links two cycles by a one-way.
	pub fn one_way_link_cycles(
		&mut self,
		source_cycle: usize,
		dest_cycle: usize,
		direction: LinkedCycleDirection,
	) -> Result<(), LevelBuilderError> {
		if source_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(source_cycle));
		}
		if dest_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(dest_cycle));
		}
		// Add the link symmetrically, in both directions
		if source_cycle == dest_cycle {
			// TODO: consider a better error.
			return Err(LevelBuilderError::CycleLinkageConflict(
				source_cycle,
				dest_cycle,
			));
		}
		self.cycles[source_cycle]
			.outgoing_one_way_links
			.push(OneWayIntermediateData {
				target_cycle: dest_cycle,
				direction,
			});
		self.declared_one_way_links.push(DeclaredLinkData {
			source_cycle,
			dest_cycle,
			direction,
		});
		Ok(())
	}

	/// Checks that the level data is complete and assembles it
	pub fn build(mut self) -> Result<LevelData, LevelBuilderError> {
		let groups = self.compute_groups()?;
		let forbidden_group_pairs = self.compute_forbidden_groups()?;
		self.validate_before_build()?;
		self.build_layout();
		let cycles = self
			.cycles
			.into_iter()
			.map(Self::build_cycle_data)
			.collect();
		let vertices = self
			.vertices
			.into_iter()
			.map(Self::build_vertex_data)
			.collect();
		Ok(LevelData {
			name: self
				.name
				.unwrap_or_else(|| Self::PLACEHOLDER_LEVEL_NAME.to_owned()),
			hint: self.hint,
			vertices,
			cycles,
			declared_links: self.declared_links,
			declared_one_way_links: self.declared_one_way_links,
			groups,
			forbidden_group_pairs,
		})
	}

	/// Computes and creates the GroupData objects
	/// Also assigns all cycles into their groups
	fn compute_groups(&mut self) -> Result<Vec<GroupData>, LevelBuilderError> {
		let mut groups = Vec::new();
		for cycle in self.cycles.iter_mut() {
			if let IntermediateLinkStatus::None = cycle.linked_cycle {
				groups.push(GroupData {
					cycles: Vec::new(),
					linked_groups: Vec::new(),
				});
				cycle.linked_cycle = IntermediateLinkStatus::Group(
					groups.len() - 1,
					LinkedCycleDirection::Coincident,
				);
			}
		}
		// Technically we could've done this in the previous loop,
		// but I do not trust either of the two doofi working on
		// this codebase to not break the invariant that
		// a cycle always points to a lower cycle or a group,
		// so I will not assume it.
		for cycle in 0..self.cycles.len() {
			let (root_cycle, direction_1) = self.find_group_root(cycle);
			let IntermediateLinkStatus::Group(group, direction_2) =
				self.cycles[root_cycle].linked_cycle
			else {
				panic!("Some doofus done doofed up the for loop above this one. (or the one inside find_group_root)");
			};
			let direction = direction_1 * direction_2;
			groups[group]
				.cycles
				.push((cycle, direction_1 * direction_2));
			self.cycles[cycle].linked_cycle = IntermediateLinkStatus::Group(group, direction);
		}
		// A third pass for aggregating group one-ways
		for source_cycle in 0..self.cycles.len() {
			let IntermediateLinkStatus::Group(source_group, direction_1) =
				self.cycles[source_cycle].linked_cycle
			else {
				panic!("Some doofus done doofed up the for loop above this one.");
			};
			for link in self.cycles[source_cycle].outgoing_one_way_links.iter() {
				let IntermediateLinkStatus::Group(target_group, direction_2) =
					self.cycles[link.target_cycle].linked_cycle
				else {
					panic!("Some doofus done doofed up the for loop above this one.");
				};
				groups[source_group].linked_groups.push(OneWayLinkData {
					source_cycle_data: Some(source_cycle),
					target_cycle_data: Some(link.target_cycle),
					target_group,
					direction: link.direction * direction_1 * direction_2,
					multiplicity: 1,
				});
			}
		}
		// TOPOLOGICAL SORT
		// THIS BLOCK INVALIDATES OR CHANGES ALL GROUP INDICES BY RESHUFFLING THE `groups` VECTOR.
		// TODO: use a better structure for the links between cycles, to deduplicate mutliple equivalent dependencies
		{
			let group_target_indices = {
				let mut sorted_order = vec![0usize; groups.len()];
				let mut next_order_index = 0;
				let mut sorted_mark = vec![false; groups.len()];
				let mut currently_visited_mark = vec![false; groups.len()];
				let mut stack = Vec::new();

				for i in 0..groups.len() {
					// Node is already in the ordering, skip it
					if sorted_mark[i] {
						continue;
					}
					stack.push(i);
					while !stack.is_empty() {
						let node = *stack.last().unwrap();
						// Node is already in the ordering, skip it
						// This removes duplicates from the stack
						if sorted_mark[node] {
							stack.pop();
							continue;
						}
						currently_visited_mark[node] = true;
						let mut blocked = false;
						for dependency in groups[node].linked_groups.iter() {
							let dependency_node = dependency.target_group;
							if currently_visited_mark[dependency_node] {
								// TODO: Better errors, but I really could not be bothered.
								return Err(LevelBuilderError::OneWayLinkLoop);
							}
							// If this node depends on nodes that have not yet been put into the topological ordering
							// We need to first handle those and block this node
							if !sorted_mark[dependency_node] {
								blocked = true;
								stack.push(dependency_node);
							}
						}
						if !blocked {
							// Remove our node (it has to be at the top because node is the last element)
							// And we have not pushed to the stack
							stack.pop();
							// Place the node into the ordering giving it the next available index
							sorted_mark[node] = true;
							currently_visited_mark[node] = false;
							sorted_order[node] = next_order_index;
							next_order_index += 1;
						}
					}
				}
				// Reverse the ordering, so sources come before targets
				for index in sorted_order.iter_mut() {
					*index = groups.len() - 1 - *index;
				}
				sorted_order
			};
			#[cfg(debug_assertions)]
			{
				// Check that every group index occurs exactly once
				let mut counter = vec![0; groups.len()];
				for index in group_target_indices.iter() {
					counter[*index] += 1;
				}
				assert_eq!(
					counter,
					vec![1; groups.len()],
					"Index map is not a bijection"
				);
			}
			// Update ***EVERY*** group index.

			// Update cycle -> group links
			for source_cycle in 0..self.cycles.len() {
				let IntermediateLinkStatus::Group(source_group, direction) =
					self.cycles[source_cycle].linked_cycle
				else {
					panic!("Some doofus done doofed up the for loop above this one.");
				};
				self.cycles[source_cycle].linked_cycle =
					IntermediateLinkStatus::Group(group_target_indices[source_group], direction);
			}

			// Update group -> group links
			for group in groups.iter_mut() {
				for link in group.linked_groups.iter_mut() {
					link.target_group = group_target_indices[link.target_group];
				}
			}

			// Shuffle groups
			let n_groups = groups.len();
			let old_groups = std::mem::replace(
				&mut groups,
				vec![
					GroupData {
						cycles: Vec::new(),
						linked_groups: Vec::new()
					};
					n_groups
				],
			);
			for (old_index, group) in old_groups.into_iter().enumerate() {
				let _ = std::mem::replace(&mut groups[group_target_indices[old_index]], group);
			}
			#[cfg(debug_assertions)]
			{
				// Check that every group is linked only to groups of higher index
				for (source_group_id, group) in groups.iter().enumerate() {
					for link in group.linked_groups.iter() {
						assert!(
							link.target_group > source_group_id,
							"Group {} links to a lower (or equal) index group {}",
							source_group_id,
							link.target_group
						);
					}
				}
			}
		}

		// Throw away cycle data on links that don't need it
		for group in groups.iter_mut() {
			for link in group.linked_groups.iter_mut() {
				// TODO: take into account detectors
				link.source_cycle_data = None;
				link.target_cycle_data = None;
			}
		}

		// Deduplicate link data whenever possible
		for group in groups.iter_mut() {
			if group.linked_groups.is_empty() {
				continue;
			}
			group.linked_groups.sort_by(OneWayLinkData::compare);
			let old_links = std::mem::take(&mut group.linked_groups);
			// At least one link must exist because we checked for the vector being empty earlier
			group.linked_groups.push(old_links[0]);
			for link in old_links.into_iter().skip(1) {
				match OneWayLinkData::try_merge(&link, group.linked_groups.last().unwrap()) {
					Some(new_link) => *group.linked_groups.last_mut().unwrap() = new_link,
					None => group.linked_groups.push(link),
				}
			}
		}
		Ok(groups)
	}

	/// Computes which pairs of groups can not be rotated in sync
	fn compute_forbidden_groups(&self) -> Result<Vec<(usize, usize)>, LevelBuilderError> {
		let mut forbid = Vec::new();
		// TODO: Use a better algorithm, like come on O(n^4) ???
		for cycle_a in 0..self.cycles.len() {
			for cycle_b in (cycle_a + 1)..self.cycles.len() {
				'inner: for &vertex_a in self.cycles[cycle_a].vertex_indices.iter() {
					for &vertex_b in self.cycles[cycle_b].vertex_indices.iter() {
						if vertex_a == vertex_b {
							let IntermediateLinkStatus::Group(group_a, _) =
								self.cycles[cycle_a].linked_cycle
							else {
								panic!("Cycle in build phase doesn't have a link pointer, should've been resolved in [`compute_groups`]");
							};
							let IntermediateLinkStatus::Group(group_b, _) =
								self.cycles[cycle_b].linked_cycle
							else {
								panic!("Cycle in build phase doesn't have a link pointer, should've been resolved in [`compute_groups`]");
							};
							if group_a == group_b {
								return Err(LevelBuilderError::OverlappedLinkedCycles(
									OverlappedLinkedCyclesError {
										dest_cycle: cycle_a,
										source_cycle: cycle_b,
										shared_vertex: vertex_a,
									},
								));
							}
							if group_a < group_b {
								forbid.push((group_a, group_b));
							} else {
								forbid.push((group_b, group_a));
							}
							break 'inner;
						}
					}
				}
			}
		}
		forbid.sort_by(|a, b| match a.0.cmp(&b.0) {
			std::cmp::Ordering::Equal => a.1.cmp(&b.1),
			ord => ord,
		});
		Ok(forbid)
	}

	/// Asserts that a vertex data object is complete and assembles it
	/// ## Panics
	/// Panics if the vertex is partially placed
	fn build_vertex_data(intermediate: IntermediateVertexData) -> VertexData {
		let position = match intermediate.position {
			IntermediateVertexPosition::Fixed(pos) => pos,
			// This is technically possible, since a vertex can belong to no cycle
			IntermediateVertexPosition::Free => Vec2::ZERO,
			// Prevented by [`materialize_all_partial_vertex_placements`]
			IntermediateVertexPosition::Partial(_) => {
				panic!("Partially placed vertex in build phase, should have been materialized")
			}
		};
		VertexData {
			position,
			object: intermediate.object,
			glyph: intermediate.glyph,
		}
	}

	/// Asserts that a cycle data object is complete and assembles it
	/// ## Panics
	/// Panics if the cycle has not been placed
	fn build_cycle_data(intermediate: IntermediateCycleData) -> CycleData {
		let placement = intermediate
			.placement
			.expect("Unplaced cycle in build phase, should have been detected earlier");
		let center_sprite_position = intermediate.center_sprite_position.expect(
			"Unplaced cycle center sprite in build phase, should have been materialized earlier",
		);
		let center_sprite_appearence =
			CycleCenterSpriteAppearence(center_sprite_position.map(|p| p - placement.position));
		let IntermediateLinkStatus::Group(group, relative_direction) = intermediate.linked_cycle
		else {
			panic!("Cycle in build phase doesn't have a link pointer, should've been resolved in [`compute_groups`]");
		};
		CycleData {
			placement,
			center_sprite_appearence,
			vertex_indices: intermediate.vertex_indices,
			turnability: intermediate.turnability,
			group,
			orientation_within_group: relative_direction,
		}
	}

	/// Verifies that the level data is complete and ready to be built without an error
	fn validate_before_build(&self) -> Result<(), LevelBuilderError> {
		// All cycles must be placed
		for (i, cycle) in self.cycles.iter().enumerate() {
			if cycle.placement.is_none() {
				return Err(LevelBuilderError::UnplacedCycle(i));
			}
		}
		Ok(())
	}

	/// Finds the root cycle (cycle that has no link to another parent) that this cycle's links point to and
	/// what is the relative direction of rotation, going through the links to there.
	fn find_group_root(&self, mut cycle: usize) -> (usize, LinkedCycleDirection) {
		let mut relative_direction = LinkedCycleDirection::Coincident;
		while let IntermediateLinkStatus::Cycle(lower, direction) = self.cycles[cycle].linked_cycle
		{
			// Todo: Optimise by shortening the path as we traverse it.
			cycle = lower;
			relative_direction *= direction;
		}
		(cycle, relative_direction)
	}

	/// Links two cycles asymmetrically (the link only goes from source to destination)
	/// The link is transitive.
	fn add_cycle_link(
		&mut self,
		cycle_a: usize,
		cycle_b: usize,
		direction: LinkedCycleDirection,
	) -> Result<(), LevelBuilderError> {
		let (cycle_1, rel_dir_a) = self.find_group_root(cycle_a);
		let (cycle_2, rel_dir_b) = self.find_group_root(cycle_b);
		let link_direction = direction * rel_dir_a * rel_dir_b;

		// There is no link to be created if we converged to the same node twice
		// However we must check that the relative direction is correct, otherwise we would be requiring that cycles
		// in the group turn opposite to their own direction.
		if cycle_1 == cycle_2 {
			return if link_direction != LinkedCycleDirection::Coincident {
				Err(LevelBuilderError::CycleLinkageConflict(cycle_a, cycle_b))
			} else {
				Ok(())
			};
		}

		// For niceness we always link higher numbers to lower numbers.
		let (source_cycle, target_cycle) = if cycle_1 > cycle_2 {
			(cycle_1, cycle_2)
		} else {
			(cycle_2, cycle_1)
		};
		// We have to take into account all the swaps that occured between
		self.cycles[source_cycle].linked_cycle =
			IntermediateLinkStatus::Cycle(target_cycle, link_direction);
		Ok(())
	}
}
