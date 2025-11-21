use super::error::*;
use super::*;

use bevy::math::bounding::BoundingVolume;
use bevy::platform::collections::HashSet;
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
			explicit_bounding_box: default(),
			scale_override: None,
			initial_zoom: None,
			initial_camera_pos: default(),
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

	pub fn explicit_bounding_box(&mut self) -> &mut PartialBoundingBox {
		&mut self.explicit_bounding_box
	}

	pub fn set_initial_zoom(&mut self, zoom: f32) {
		self.initial_zoom = Some(zoom);
	}

	pub fn explicit_initial_camera_pos(&mut self) -> &mut PartialVec2 {
		&mut self.initial_camera_pos
	}

	pub fn add_vertex(&mut self) -> Result<usize, LevelBuilderError> {
		self.vertices.push(IntermediateVertexData {
			position: IntermediateVertexPosition::Free,
			hint_position: None,
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
		walls: impl IntoIterator<Item = i32>,
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
		let n_vertices = usize::max(vertex_indices.len(), 1) as i32;
		let detectors = detectors
			.into_iter()
			.map(|(detector, position)| (detector, i32::rem_euclid(position, n_vertices) as usize))
			.collect::<Vec<_>>();
		let walls = walls
			.into_iter()
			.map(|position| i32::rem_euclid(position, n_vertices) as usize)
			.collect::<Vec<_>>();
		// If there are detectors but no vertices, this cycle is invalid.
		if vertex_indices.is_empty() && !detectors.is_empty() {
			return Err(LevelBuilderError::DetectorOnEmptyCycle);
		}
		// If there are walls but no vertices, this cycle is invalid.
		if vertex_indices.is_empty() && !walls.is_empty() {
			return Err(LevelBuilderError::WallOnEmptyCycle);
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
			center_sprite_position: IntermediateCycleCenterSpritePosition::Unspecified,
			vertex_indices,
			turnability,
			linked_cycle: IntermediateLinkStatus::None,
			outgoing_one_way_links: Vec::new(),
			placed_detectors: detectors,
			walls,
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
		multiplicity: u64,
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
				multiplicity,
			});
		self.declared_one_way_cycle_links
			.push(DeclaredOneWayLinkData {
				source: source_cycle,
				dest_cycle,
				direction,
				multiplicity,
			});
		Ok(())
	}

	/// Links a detector to a cycle by a one-way.
	pub fn one_way_link_detector(
		&mut self,
		detector: usize,
		dest_cycle: usize,
		direction: LinkedCycleDirection,
		multiplicity: u64,
	) -> Result<(), LevelBuilderError> {
		if dest_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(dest_cycle));
		}
		if let Some(detector) = self.detectors.get_mut(detector) {
			detector.links.push(OneWayIntermediateData {
				target_cycle: dest_cycle,
				direction,
				multiplicity,
			});
			Ok(())
		} else {
			Err(LevelBuilderError::DetectorIndexOutOfRange(detector))
		}
	}

	/// Checks that the level data is complete and assembles it
	pub fn build(mut self) -> LevelBuildResult {
		let mut errors = LevelBuilderErrorLog::default();

		let (groups, detectors, execution_order) = self.compute_groups_and_detectors(&mut errors);
		let forbidden_group_pairs = self
			.compute_forbidden_groups(&groups)
			.map_err(|err| errors.push(LevelBuilderError::OverlappedLinkedCycles(err)))
			.unwrap_or_default();
		self.validate_before_build(&mut errors);
		errors.extend(
			self.solve_vertex_placements()
				.into_iter()
				.map(LevelBuilderError::VertexSolverError),
		);
		self.materialize_cycle_center_placements();
		self.apply_color_label_appearences_to_buttons();
		let bounding_box = self.fit_to_default_viewport();
		let initial_camera_pos = Vec2::new(
			self.initial_camera_pos.x.unwrap_or(bounding_box.center().x),
			self.initial_camera_pos.y.unwrap_or(bounding_box.center().y),
		);
		let vertices = self
			.vertices
			.into_iter()
			.map(Self::build_vertex_data)
			.collect::<Vec<_>>();
		let cycles = self
			.cycles
			.into_iter()
			.map(|c| Self::build_cycle_data(c, &vertices))
			.collect();
		let level = LevelData {
			is_valid: errors.is_ok(),
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
			bounding_box,
			initial_zoom: self.initial_zoom.unwrap_or(1.0),
			initial_camera_pos,
		};
		LevelBuildResult { level, errors }
	}

	/// Computes and creates the [`GroupData`] objects
	/// Computes the execution order of groups and detectors
	/// Also assigns all cycles and detectors into their groups
	fn compute_groups_and_detectors(
		&mut self,
		errors: &mut LevelBuilderErrorLog,
	) -> (Vec<GroupData>, Vec<DetectorData>, Vec<DetectorOrGroup>) {
		let mut groups = self.construct_cycle_groups();
		let detectors = self.construct_detectors();
		self.link_groups_via_detectors(&mut groups);
		match self.construct_execution_order(&groups, &detectors) {
			Ok(execution_order) => {
				#[cfg(any(debug_assertions, test))]
				self.debug_verify_execution_order(&groups, &detectors, &execution_order);
				(groups, detectors, execution_order)
			}
			Err(err) => {
				errors.push(LevelBuilderError::OneWayLinkLoop(err));
				(groups, detectors, Vec::new())
			}
		}
	}

	/// Collects all cycles into groups
	///
	/// Switches all cycles to [`IntermediateLinkStatus::Group`]
	/// and outputs the group list with the corresponding indices.
	/// [`GroupData::cycles`] and [`GroupData::linked_groups`] are filled in.
	/// [`GroupData::outgoing_detector_cycles`] are **not** filled in yet
	/// as detectors have not yet been constructed
	/// (call [`Self::link_groups_via_detectors`] after building detectors)
	/// (yes, [`Self::link_groups_via_detectors`] does not take detectors directly,
	/// but it depends on correct detector numbering that is
	/// modified by [`Self::construct_detectors`])
	fn construct_cycle_groups(&mut self) -> Vec<GroupData> {
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
					multiplicity: link.multiplicity,
				});
			}
		}
		groups
	}

	fn construct_detectors(&mut self) -> Vec<DetectorData> {
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
								multiplicity: link.multiplicity,
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
	}

	fn link_groups_via_detectors(&self, groups: &mut [GroupData]) {
		for (cycle_id, cycle) in self.cycles.iter().enumerate() {
			if !cycle.placed_detectors.is_empty() || !cycle.walls.is_empty() {
				let IntermediateLinkStatus::Group(group, _) = cycle.linked_cycle else {
					unreachable!("Cycles should have groups by now");
				};
				groups[group].outgoing_detector_cycles.push(cycle_id);
			}
		}
	}

	/// Sorts a level topologically by one-way links
	///
	/// TODO: use a better structure for the links between cycles,
	/// to deduplicate mutliple equivalent dependencies
	fn construct_execution_order(
		&self,
		groups: &[GroupData],
		detectors: &[DetectorData],
	) -> Result<Vec<DetectorOrGroup>, OneWayLinkLoopError> {
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
						return Err(OneWayLinkLoopError);
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
		Ok(sorted_order)
	}

	/// Asserts that [`Self::construct_execution_order`] produced
	/// a correct topological ordering
	#[cfg(any(debug_assertions, test))]
	fn debug_verify_execution_order(
		&self,
		groups: &[GroupData],
		detectors: &[DetectorData],
		execution_order: &[DetectorOrGroup],
	) {
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
				for &(detector_id, _) in self.cycles[detector_cycle_id].placed_detectors.iter() {
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

	/// Computes which pairs of groups can not be rotated in sync
	fn compute_forbidden_groups(
		&self,
		groups: &[GroupData],
	) -> Result<Vec<(usize, usize, HashSet<usize>)>, OverlappedLinkedCyclesError> {
		let mut forbid = Vec::new();
		// TODO: Use a better algorithm, like come on O(n^6) ???
		for group_a in 0..groups.len() {
			for group_b in group_a..groups.len() {
				let mut problems = HashSet::default();
				for &(cycle_a, _) in groups[group_a].cycles.iter() {
					for &(cycle_b, _) in groups[group_b].cycles.iter() {
						if cycle_a == cycle_b {
							debug_assert!(
								group_a == group_b,
								"Union-find should've partitioned cycles into disjoint groups"
							);
							continue;
						}
						'inner: for &vertex_a in self.cycles[cycle_a].vertex_indices.iter() {
							for &vertex_b in self.cycles[cycle_b].vertex_indices.iter() {
								if vertex_a == vertex_b {
									if group_a == group_b {
										return Err(OverlappedLinkedCyclesError {
											dest_cycle: cycle_a,
											source_cycle: cycle_b,
											shared_vertex: vertex_a,
										});
									}
									problems.insert(vertex_a);
									break 'inner;
								}
							}
						}
					}
				}
				if !problems.is_empty() {
					forbid.push((group_a, group_b, problems));
				}
			}
		}
		Ok(forbid)
	}

	/// Asserts that a vertex data object is complete and assembles it
	fn build_vertex_data(intermediate: IntermediateVertexData) -> VertexData {
		let position = intermediate.position.get_fixed().unwrap_or_default();
		VertexData {
			position,
			object: intermediate.object,
			glyph: intermediate.glyph,
		}
	}

	/// Asserts that a cycle data object is complete and assembles it
	fn build_cycle_data(
		intermediate: IntermediateCycleData,
		vertex_data: &[VertexData],
	) -> CycleData {
		let placement = intermediate.placement.unwrap_or_else(|| {
			debug_assert!(
				false,
				"Unplaced cycle in build phase, should have been detected earlier"
			);
			CyclePlacement {
				position: Vec2::ZERO,
				shape: CycleShape::Circle(1.0),
			}
		});
		let center_sprite_position = match intermediate.center_sprite_position {
			IntermediateCycleCenterSpritePosition::Placed(pos) => Some(pos),
			IntermediateCycleCenterSpritePosition::Disabled => None,
			IntermediateCycleCenterSpritePosition::Unspecified => {
				debug_assert!(false, "Unplaced cycle center sprite in build phase, should have been materialized earlier");
				None
			}
		};
		let center_sprite_appearence =
			CycleCenterSpriteAppearence(center_sprite_position.map(|p| p - placement.position));
		let (group, relative_direction) = match intermediate.linked_cycle {
			IntermediateLinkStatus::Group(group, relative_direction) => (group, relative_direction),
			_ => {
				debug_assert!(false, "Cycle built without a valid group assignment");
				(0, LinkedCycleDirection::Coincident)
			}
		};
		// This only works for circles
		// TODO: Make different versions for other cycle shapes
		let vertex_positions = intermediate
			.vertex_indices
			.iter()
			.map(|i| {
				let relative_vertex_position = vertex_data[*i].position - placement.position;
				let angle = Vec2::X.angle_to(relative_vertex_position);
				(angle / TAU).rem_euclid(1.0)
			})
			.collect();
		CycleData {
			placement,
			center_sprite_appearence,
			vertex_indices: intermediate.vertex_indices,
			vertex_positions,
			detector_indices: intermediate.placed_detectors,
			wall_indices: intermediate.walls,
			turnability: intermediate.turnability,
			group,
			orientation_within_group: relative_direction,
		}
	}

	/// Verifies that the level data is complete and ready to be built without an error
	///
	/// If some information is missing, an error is emited and the information is filled in
	fn validate_before_build(&mut self, errors: &mut LevelBuilderErrorLog) {
		// All cycles must be placed
		for (i, cycle) in self.cycles.iter_mut().enumerate() {
			if cycle.placement.is_none() {
				errors.push(LevelBuilderError::UnplacedCycle(i));
				// Fill in some placeholder value so we can return partial build
				cycle.placement = Some(CyclePlacement {
					position: Vec2::ZERO,
					shape: CycleShape::Circle(1.0),
				})
			}
		}
	}

	/// Finds the root cycle (cycle that has no link to another parent) that this cycle's links point to and
	/// what is the relative direction of rotation, going through the links to there.
	fn find_group_root(&self, mut cycle: usize) -> (usize, LinkedCycleDirection) {
		let mut relative_direction = LinkedCycleDirection::Coincident;
		while let IntermediateLinkStatus::Cycle(lower, direction) = self.cycles[cycle].linked_cycle
		{
			// TODO: Optimise by shortening the path as we traverse it.
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
