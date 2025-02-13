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
			detectors: Vec::new(),
			declared_links: Vec::new(),
			declared_one_way_cycle_links: Vec::new(),
			declared_one_way_detector_links: Vec::new(),
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
		detectors: impl IntoIterator<Item = (usize, i32)>,
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
		// Max with 1 to avoid a possible panic in rem_euclid.
		let n_vertices = vertex_indices.len().max(1) as i32;
		let detectors = detectors
			.into_iter()
			.map(|(detector, position)| (detector, i32::rem_euclid(position, n_vertices) as usize))
			.collect::<Vec<_>>();
		// If there are detecotrs but no vertices, this cycle is invalid.
		if vertex_indices.is_empty() && !detectors.is_empty() {
			return Err(LevelBuilderError::DetectorOnEmptyCycle);
		}
		if let Some((index, _)) = detectors
			.iter()
			.find(|&(index, _)| *index >= self.detectors.len())
		{
			return Err(LevelBuilderError::DetectorIndexOutOfRange(*index));
		}
		// Add the cycle entry
		self.cycles.push(IntermediateCycleData {
			placement: None,
			center_sprite_position: None,
			vertex_indices,
			turnability,
			linked_cycle: IntermediateLinkStatus::None,
			outgoing_one_way_links: Vec::new(),
			placed_detectors: detectors,
		});
		Ok(self.cycles.len() - 1)
	}

	pub fn add_detector(&mut self) -> Result<usize, LevelBuilderError> {
		self.detectors
			.push(IntermediateDetectorData { links: Vec::new() });
		Ok(self.detectors.len() - 1)
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
		self.declared_one_way_cycle_links.push(DeclaredLinkData {
			source_cycle,
			dest_cycle,
			direction,
		});
		Ok(())
	}

	/// Links a detector to a cycle by a one-way.
	pub fn one_way_link_detector(
		&mut self,
		detector: usize,
		dest_cycle: usize,
		direction: LinkedCycleDirection,
	) -> Result<(), LevelBuilderError> {
		if dest_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(dest_cycle));
		}
		if let Some(detector) = self.detectors.get_mut(detector) {
			detector.links.push(OneWayIntermediateData {
				target_cycle: dest_cycle,
				direction,
			});
			Ok(())
		} else {
			Err(LevelBuilderError::DetectorIndexOutOfRange(detector))
		}
	}

	/// Checks that the level data is complete and assembles it
	pub fn build(mut self) -> Result<LevelData, LevelBuilderError> {
		let (groups, detectors, execution_order) = self.compute_groups_and_detectors()?;
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
			groups,
			detectors,
			declared_links: self.declared_links,
			declared_one_way_links: self.declared_one_way_cycle_links,
			forbidden_group_pairs,
			execution_order,
		})
	}

	/// Computes and creates the GroupData objects
	/// Computes the execution order of groups and detectors
	/// Also assigns all cycles and detectors into their groups
	fn compute_groups_and_detectors(
		&mut self,
	) -> Result<(Vec<GroupData>, Vec<DetectorData>, Vec<DetectorOrGroup>), LevelBuilderError> {
		let mut groups = Vec::new();
		for cycle in self.cycles.iter_mut() {
			if let IntermediateLinkStatus::None = cycle.linked_cycle {
				groups.push(GroupData {
					cycles: Vec::new(),
					linked_groups: Vec::new(),
					outgoing_detector_cycles: Vec::new(),
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
				unreachable!("Some doofus done doofed up the for loop above this one. (or the one inside find_group_root)");
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
				unreachable!("Some doofus done doofed up the for loop above this one.");
			};
			for link in self.cycles[source_cycle].outgoing_one_way_links.iter() {
				let IntermediateLinkStatus::Group(target_group, direction_2) =
					self.cycles[link.target_cycle].linked_cycle
				else {
					unreachable!("Some doofus done doofed up the for loop above this one.");
				};
				groups[source_group].linked_groups.push(OneWayLinkData {
					target_group,
					direction: link.direction * direction_1 * direction_2,
					multiplicity: 1,
				});
			}
		}
		// Finalise detector objects
		let detectors = {
			// Remove unused detectors
			let mut used = vec![false; self.detectors.len()];
			for cycle in self.cycles.iter() {
				for (detector, _) in cycle.placed_detectors.iter() {
					used[*detector] = true;
				}
			}
			let mut new_ids = vec![0; self.detectors.len()];
			let mut detectors = Vec::new();
			for detector in 0..self.detectors.len() {
				if used[detector] {
					new_ids[detector] = detectors.len();
					detectors.push(DetectorData {
						linked_groups: self.detectors[detector]
							.links
							.iter()
							.map(|link| {
								let IntermediateLinkStatus::Group(
									target_group,
									target_direction_in_group,
								) = self.cycles[link.target_cycle].linked_cycle
								else {
									unreachable!("Cycles should have groups by now");
								};
								OneWayLinkData {
									target_group,
									direction: link.direction * target_direction_in_group,
									multiplicity: 1, // TODO
								}
							})
							.collect(),
					});
				}
			}
			// Fix indices in cycles
			for cycle in self.cycles.iter_mut() {
				for (detector, _) in cycle.placed_detectors.iter_mut() {
					*detector = new_ids[*detector];
				}
			}
			detectors
		};
		// Place detectors into groups
		for (cycle_id, cycle) in self.cycles.iter().enumerate() {
			if !cycle.placed_detectors.is_empty() {
				let IntermediateLinkStatus::Group(group, _) = cycle.linked_cycle else {
					unreachable!("Cycles should have groups by now");
				};
				groups[group].outgoing_detector_cycles.push(cycle_id);
			}
		}
		// TOPOLOGICAL SORT
		// TODO: use a better structure for the links between cycles, to deduplicate mutliple equivalent dependencies
		let execution_order = {
			let n_groups = groups.len();
			let n_detectors = self.detectors.len();

			let get_merged_index = |id| match id {
				DetectorOrGroup::Group(i) => i,
				DetectorOrGroup::Detector(i) => i + n_groups,
			};

			let mut sorted_order = Vec::new();
			let mut sorted_mark = vec![false; n_groups + n_detectors];
			let mut currently_visited_mark = vec![false; n_groups + n_detectors];
			let mut stack = Vec::new();

			// This for-loop inserts all the groups, we don't really care about the detectors
			// They'll either be brought in as a dependency of some group, or can be ignored.
			// (Though that should raise eyebrows if it happens.)
			for new_group in 0..n_groups {
				// Node is already in the ordering, skip it
				if sorted_mark[new_group] {
					continue;
				}
				stack.push(DetectorOrGroup::Group(new_group));
				while let Some(&node) = stack.last() {
					let index = get_merged_index(node);
					// Node is already in the ordering, skip it
					// This removes duplicates from the stack
					if sorted_mark[index] {
						stack.pop();
						continue;
					}
					currently_visited_mark[index] = true;
					let mut blocked = false;
					// Process group dependencies of this node
					let mut dependency_callback = |dependency: DetectorOrGroup| {
						let index = get_merged_index(dependency);
						if currently_visited_mark[index] {
							// TODO: Better errors, but I really could not be bothered.
							return Err(LevelBuilderError::OneWayLinkLoop);
						}
						// If this node depends on nodes that have not yet been put into the topological ordering
						// We need to first handle those and block this node
						if !sorted_mark[index] {
							blocked = true;
							stack.push(dependency);
						}
						Ok(())
					};
					match node {
						DetectorOrGroup::Group(group) => {
							for link in groups[group].linked_groups.iter() {
								dependency_callback(DetectorOrGroup::Group(link.target_group))?
							}
							for detector_cycle in groups[group].outgoing_detector_cycles.iter() {
								for (detector, _) in
									self.cycles[*detector_cycle].placed_detectors.iter()
								{
									dependency_callback(DetectorOrGroup::Detector(*detector))?
								}
							}
						}
						DetectorOrGroup::Detector(detector) => {
							for link in detectors[detector].linked_groups.iter() {
								dependency_callback(DetectorOrGroup::Group(link.target_group))?
							}
						}
					}
					// If the node is not blocked, we can safely put it as the next in the topological ordering.
					if !blocked {
						// Remove our node (it has to be at the top because `node` is the last element)
						// And we have not pushed to the stack
						stack.pop();
						// Place the node into the ordering giving it the next available index
						sorted_mark[index] = true;
						currently_visited_mark[index] = false;
						sorted_order.push(node);
					}
				}
			}
			// Reverse the ordering, so sources come before targets
			sorted_order.reverse();
			sorted_order
		};
		#[cfg(any(debug_assertions, test))]
		{
			// Check that every group and (used) detector index occurs exactly once
			let mut group_counter = vec![0; groups.len()];
			let mut detector_counter = vec![0; detectors.len()];
			for step in execution_order.iter() {
				match step {
					DetectorOrGroup::Group(group) => group_counter[*group] += 1,
					DetectorOrGroup::Detector(detector) => detector_counter[*detector] += 1,
				}
			}
			assert_eq!(
				group_counter,
				vec![1; groups.len()],
				"Groups are not placed uniquely in execution order"
			);
			assert_eq!(
				detector_counter,
				vec![1; detectors.len()],
				"Detectors are not placed uniquely in execution order"
			);
		}
		#[cfg(any(debug_assertions, test))]
		{
			// Check that all links come from earlier to later objects in the execution order
			let mut group_appearances = vec![0; groups.len()];
			let mut detector_appearances = vec![0; detectors.len()];
			for (number, step) in execution_order.iter().enumerate() {
				match step {
					DetectorOrGroup::Group(group) => group_appearances[*group] = number,
					DetectorOrGroup::Detector(detector) => detector_appearances[*detector] = number,
				}
			}
			for (source_group_id, group) in groups.iter().enumerate() {
				for link in group.linked_groups.iter() {
					assert!(
						group_appearances[link.target_group] > group_appearances[source_group_id],
						"Group {} (with order {}) links to an earlier group {} (with order {})",
						source_group_id,
						group_appearances[source_group_id],
						link.target_group,
						group_appearances[link.target_group],
					);
				}
				for detector_cycle_id in group.outgoing_detector_cycles.iter().copied() {
					for &(detector_id, _) in self.cycles[detector_cycle_id].placed_detectors.iter()
					{
						assert!(
							detector_appearances[detector_id] > group_appearances[source_group_id],
							"Group {} (with order {}) contains an earlier detector {} (with order {})",
							source_group_id,
							group_appearances[source_group_id],
							detector_id,
							detector_appearances[detector_id],
						);
					}
				}
			}
			for (detector_id, detector) in detectors.iter().enumerate() {
				for link in detector.linked_groups.iter().copied() {
					assert!(
						detector_appearances[detector_id] < group_appearances[link.target_group],
						"Detector {} (with order {}) triggers an earlier group {} (with order {})",
						detector_id,
						detector_appearances[detector_id],
						link.target_group,
						group_appearances[link.target_group],
					);
				}
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
		Ok((groups, detectors, execution_order))
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
			panic!("Cycle in build phase doesn't have a link pointer, should've been resolved in [`compute_groups_and_detectors`]");
		};
		CycleData {
			placement,
			center_sprite_appearence,
			vertex_indices: intermediate.vertex_indices,
			detector_indices: intermediate.placed_detectors,
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
