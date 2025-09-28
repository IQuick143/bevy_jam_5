use super::error::*;
use super::*;
use crate::graphics::{LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH};
use bevy::math::bounding::{Aabb2d, BoundingVolume};
use std::f32::consts::PI;

impl LevelBuilder {
	pub fn place_circle(
		&mut self,
		target_cycle: usize,
		center: Vec2,
		radius: f32,
	) -> Result<(), LevelBuilderError> {
		if target_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(target_cycle));
		}
		if self.cycles[target_cycle].placement.is_some() {
			return Err(LevelBuilderError::CycleAlreadyPlaced(target_cycle));
		}
		if radius.is_nan() || radius <= 0.0 {
			return Err(LevelBuilderError::CycleRadiusNotPositive(
				target_cycle,
				radius,
			));
		}
		self.cycles[target_cycle].placement = Some(CyclePlacement {
			position: center,
			shape: CycleShape::Circle(radius),
		});
		Ok(())
	}

	/// Explicitly sets the position of a cycle's center indicator
	/// or makes it invisible
	pub fn place_cycle_center(
		&mut self,
		target_cycle: usize,
		position: Option<Vec2>,
	) -> Result<(), LevelBuilderError> {
		if target_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(target_cycle));
		}
		if let Some(position) = position {
			self.cycles[target_cycle].center_sprite_position =
				IntermediateCycleCenterSpritePosition::Placed(position);
		} else {
			self.cycles[target_cycle].center_sprite_position =
				IntermediateCycleCenterSpritePosition::Disabled;
		}
		Ok(())
	}

	/// Assigns a fixed placement to a vertex
	/// ## Parameters
	/// - `target_vertex` - Index of the vertex to place
	/// - `position` - Actual position where the vertex should be placed
	pub fn place_vertex(
		&mut self,
		target_vertex: usize,
		position: Vec2,
	) -> Result<(), LevelBuilderError> {
		if target_vertex >= self.vertices.len() {
			return Err(LevelBuilderError::VertexIndexOutOfRange(target_vertex));
		}
		match self.vertices[target_vertex].position {
			IntermediateVertexPosition::Fixed(_) => {
				Err(LevelBuilderError::VertexAlreadyPlaced(target_vertex))
			}
			IntermediateVertexPosition::Free => {
				self.vertices[target_vertex].position = IntermediateVertexPosition::Fixed(position);
				Ok(())
			}
		}
	}

	/// Assigns a hint placement to a vertex, overriding any previous hint
	/// ## Parameters
	/// - `target_vertex` - Index of the vertex to add a hint for
	/// - `position` - Actual position near which the vertex should be placed
	///
	/// ## Notes
	/// Used during decisions in the layout solver.
	/// If the vertex has a finite set of placements to choose from,
	/// it will pick the closest one to the hint.
	///
	/// If the vertex is constrained to a cycle, it will use the closest point on the cycle
	/// (if the cycle shape supports that, shapeless cycles do not)
	///
	/// If the vertex has a fixed placement by other means, the hint is ignored
	pub fn add_vertex_hint(
		&mut self,
		target_vertex: usize,
		position: Vec2,
	) -> Result<(), LevelBuilderError> {
		if target_vertex >= self.vertices.len() {
			return Err(LevelBuilderError::VertexIndexOutOfRange(target_vertex));
		}
		self.vertices[target_vertex].hint_position = Some(position);
		Ok(())
	}

	pub fn set_color_label_appearences_for_cycle(
		&mut self,
		target_cycle: usize,
		position_set: CycleBoundColorLabelPositionSet,
		place_outside_cycle: bool,
		has_arrow_tip: bool,
	) -> Result<(), LevelBuilderError> {
		if target_cycle >= self.cycles.len() {
			return Err(LevelBuilderError::CycleIndexOutOfRange(target_cycle));
		}
		for target_vertex in &self.cycles[target_cycle].vertex_indices {
			let vertex = &mut self.vertices[*target_vertex];
			if matches!(vertex.glyph, Some(GlyphData::Button(Some(_)))) {
				vertex.color_label_appearence = Some(CycleBoundButtonColorLabelAppearence {
					owner_cycle: target_cycle,
					place_outside_cycle,
					has_arrow_tip,
					positions: position_set,
				})
			}
		}
		Ok(())
	}

	/// Sets the scale override to the level.
	/// Argument is a float converting epilang units to world units
	pub fn set_level_scale(&mut self, mut scale: f32) {
		if !scale.is_finite() {
			scale = 1.0;
		}
		self.scale_override = Some(scale.max(0.0));
	}

	/// Iterates through all vertices and applies their intermediate
	/// color label position to button objects on them if present.
	///
	/// This method expects all vertices to be materialized when it is called.
	pub(super) fn apply_color_label_appearences_to_buttons(&mut self) {
		for vertex in &mut self.vertices {
			if let Some(GlyphData::Button(Some((_, appearence)))) = &mut vertex.glyph {
				if let Some(p) = vertex.color_label_appearence {
					let Some(vertex_position) = vertex.position.get_fixed() else {
						log::warn!("Color label appearences cannot be applied before all vertices that belong to a cycle are placed");
						continue;
					};
					let Some(placement) = self.cycles[p.owner_cycle].placement else {
						log::warn!("Color label appearences cannot be applied before all cycles are placed");
						continue;
					};
					let owner_cycle_position = placement.position;
					let angle_from_owner =
						-Vec2::Y.angle_to(vertex_position - owner_cycle_position);
					// Flip target angle if we want the labels inside the cycle
					let target_angle = if p.place_outside_cycle {
						angle_from_owner
					} else {
						angle_from_owner + PI
					}
					.rem_euclid(2.0 * PI);
					let position = match p.positions {
						CycleBoundColorLabelPositionSet::LeftRight => {
							ButtonColorLabelPosition::AnglePlaced(match target_angle / PI {
								0.0..1.0 => PI * 0.5,
								1.0..=2.0 => PI * 1.5,
								// Theoretically unreachable but just in case (because floats) we use a default value
								_ => PI * 0.5,
							})
						}
						CycleBoundColorLabelPositionSet::AboveBelow => {
							ButtonColorLabelPosition::AnglePlaced(match target_angle / PI {
								0.0..0.5 | 1.5..=2.0 => 0.0,
								0.5..1.5 => PI,
								// Theoretically unreachable but just in case (because floats) we use a default value
								_ => 0.0,
							})
						}
						CycleBoundColorLabelPositionSet::CardinalDirections => {
							ButtonColorLabelPosition::AnglePlaced(match target_angle / PI {
								0.0..0.25 | 1.75..=2.0 => 0.0,
								0.25..0.75 => PI * 0.5,
								0.75..1.25 => PI,
								1.25..1.75 => PI * 1.5,
								// Theoretically unreachable but just in case (because floats) we use a default value
								_ => 0.0,
							})
						}
						CycleBoundColorLabelPositionSet::AllDirections => {
							ButtonColorLabelPosition::AnglePlaced(target_angle)
						}
						CycleBoundColorLabelPositionSet::AllDirectionsRotated => {
							ButtonColorLabelPosition::AngleRotated(target_angle)
						}
					};
					*appearence = ButtonColorLabelAppearence {
						position,
						has_arrow_tip: p.has_arrow_tip,
					};
				}
			}
		}
	}

	/// Iterates through all cycles and sets their center indicator placements
	/// to a default position for their placement shape if they have not
	/// been placed explicitly
	///
	/// All vertices that are a part of a cycle must have a fixed placement by now
	pub(super) fn materialize_cycle_center_placements(&mut self) {
		for cycle_data in &mut self.cycles {
			let Some(placement) = cycle_data.placement else {
				continue;
			};
			if cycle_data.center_sprite_position
				== IntermediateCycleCenterSpritePosition::Unspecified
			{
				// If center sprite has not been explicitly positioned
				// or disabled, put it in the cycle center
				cycle_data.center_sprite_position =
					IntermediateCycleCenterSpritePosition::Placed(placement.position);
			}
		}
	}

	/// Calculates the bounding box of all currently placed cycles and their center sprites
	fn get_content_bounding_box(&self) -> Aabb2d {
		let min = self
			.cycles
			.iter()
			.map(|cycle| {
				cycle
					.placement
					.map(|p| Self::get_bounding_box_for_cycle(p).min)
					.unwrap_or(Vec2::INFINITY)
					.min(
						cycle
							.center_sprite_position
							.placed()
							.unwrap_or(Vec2::INFINITY),
					)
			})
			.fold(Vec2::INFINITY, Vec2::min);
		let max = self
			.cycles
			.iter()
			.map(|cycle| {
				cycle
					.placement
					.map(|p| Self::get_bounding_box_for_cycle(p).max)
					.unwrap_or(Vec2::NEG_INFINITY)
					.max(
						cycle
							.center_sprite_position
							.placed()
							.unwrap_or(Vec2::NEG_INFINITY),
					)
			})
			.fold(Vec2::NEG_INFINITY, Vec2::max);
		Aabb2d { min, max }
	}

	/// Gets the true bounding box of the level, either computed or set explicitly by the caller
	fn get_bounding_box(&self) -> Aabb2d {
		let content = self.get_content_bounding_box();
		// Replace bounds with explicit ones if appropriate
		let min = Vec2::new(
			self.explicit_bounding_box.left.unwrap_or(content.min.x),
			self.explicit_bounding_box.top.unwrap_or(content.min.y),
		);
		let max = Vec2::new(
			self.explicit_bounding_box.right.unwrap_or(content.max.x),
			self.explicit_bounding_box.bottom.unwrap_or(content.max.y),
		);
		let center = (max + min) / 2.0;
		let mut half = (max - min) / 2.0;
		// Prevent zero-size bounding boxes
		if half.x <= 0.0 {
			half.x = 1.0;
			log::warn!("Level bounding box has zero width.");
		}
		if half.y <= 0.0 {
			half.y = 1.0;
			log::warn!("Level bounding box has zero height.");
		}
		Aabb2d::new(center, half)
	}

	/// Computes the bounding box for a given cycle shape
	fn get_bounding_box_for_cycle(placement: CyclePlacement) -> Aabb2d {
		match placement.shape {
			CycleShape::Circle(radius) => Aabb2d::new(placement.position, Vec2::splat(radius)),
		}
	}

	/// Resizes all currently placed objects to fit the default screen bounding box
	///
	/// ## Return Value
	/// Final bounding box that contains all objects in the level after the resize
	pub(super) fn fit_to_default_viewport(&mut self) -> Aabb2d {
		self.fit_to_viewport(Aabb2d::new(LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH / 2.0))
	}

	/// Resizes all currently placed objects to fit a bounding box
	///
	/// ## Return Value
	/// Final bounding box that contains all objects in the level after the resize.
	/// Fits inside and is centered on `viewport`
	fn fit_to_viewport(&mut self, viewport: Aabb2d) -> Aabb2d {
		let bounds = self.get_bounding_box();
		let scale = match self.scale_override {
			Some(scale) => scale,
			None => {
				let scale = viewport.half_size() / bounds.half_size();
				// Scaling must be equal in both directions
				scale.x.min(scale.y)
			}
		};
		let scale = if scale.is_finite() {
			scale.abs()
		} else {
			log::warn!(
				"Non-finite scale value in level bound computation, level likely has 0 size."
			);
			1.0
		};

		let viewport_center = viewport.center();
		let bounds_center = bounds.center();

		for vertex in &mut self.vertices {
			if let IntermediateVertexPosition::Fixed(p) = &mut vertex.position {
				*p = (*p - bounds_center) * scale + viewport_center;
			}
		}
		for cycle in &mut self.cycles {
			if let Some(p) = &mut cycle.placement {
				p.position = (p.position - bounds_center) * scale + viewport_center;
				match &mut p.shape {
					CycleShape::Circle(radius) => *radius *= scale,
				}
			}
			if let IntermediateCycleCenterSpritePosition::Placed(p) =
				&mut cycle.center_sprite_position
			{
				*p = (*p - bounds_center) * scale + viewport_center;
			}
		}

		// Transform explicitly set initial camera position to world coordinates
		if let Some(x) = &mut self.initial_camera_pos.x {
			*x = (*x - bounds_center.x) * scale + viewport_center.x;
		}
		if let Some(y) = &mut self.initial_camera_pos.y {
			*y = (*y - bounds_center.y) * scale + viewport_center.y;
		}

		Aabb2d::new(Vec2::ZERO, (bounds.half_size() * scale).max(Vec2::ONE))
	}
}
