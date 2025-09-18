use bevy::{math::bounding::Aabb2d, platform::collections::HashSet, prelude::*};
use std::f32::consts::TAU;

pub mod asset;
pub mod backend;
pub mod builder;
pub mod list;
pub mod list_asset;
#[cfg(test)]
mod test;

/// Complete description of a level
#[derive(Debug, Clone, Reflect, Asset)]
pub struct LevelData {
	/// Whether this level is correctly built
	pub is_valid: bool,
	/// Display name of the level
	pub name: String,
	/// Hint or comment that relates to the level, if any
	pub hint: Option<String>,
	/// Data for all vertices in the level
	pub vertices: Vec<VertexData>,
	/// Data for all cycles in the level
	pub cycles: Vec<CycleData>,
	/// Data for all groups of cycles in the level
	pub groups: Vec<GroupData>,
	/// Data for all abstract detector objects in the level (the ones doing the rotation propagation instead of the ones being rendered)
	pub detectors: Vec<DetectorData>,
	/// An order in which groups and detectors should be evaluated, sorted in topological order (previous elements need to be evaluated before later ones)
	pub execution_order: Vec<DetectorOrGroup>,
	/// List of group pairs, which cannot be turned at once
	/// Sorted in increasing lexicographic order of their index tuples
	/// The hashset contains offending vertices
	pub forbidden_group_pairs: Vec<(usize, usize, HashSet<usize>)>,
	/// Data for all cycle links that have been explicitly declared in the level file.
	/// Will be used for rendering the links
	pub declared_links: Vec<DeclaredLinkData>,
	/// Data for all one way links that have been explicitly declared in the level file.
	/// Will be used for rendering the links
	pub declared_one_way_links: Vec<DeclaredOneWayLinkData>,
	/// Bounding box
	pub bounding_box: Aabb2d,
	/// Camera zoom that should be set when entering the level
	pub initial_zoom: f32,
	/// Camera position that should be set when entering the level
	pub initial_camera_pos: Vec2,
}

/// Either the index of a detector or a group
#[derive(Debug, Reflect, Clone, Copy)]
pub enum DetectorOrGroup {
	Group(usize),
	Detector(usize),
}

/// Description of a single vertex
#[derive(Component, Debug, Clone, Copy, Reflect)]
pub struct VertexData {
	/// Position of the vertex on the screen
	pub position: Vec2,
	/// Object that lies on the vertex, if any
	pub object: Option<ObjectData>,
	/// Glyph that lies on the vertex, if any
	pub glyph: Option<GlyphData>,
}

/// Description of a single cycle
#[derive(Debug, Clone, Reflect)]
pub struct CycleData {
	/// Placement of the cycle
	pub placement: CyclePlacement,
	/// Settings of the cycle center sprite
	pub center_sprite_appearence: CycleCenterSpriteAppearence,
	/// Indices into [`LevelData::vertices`]
	/// that identify the vertices that lie on the cycle, in clockwise order
	pub vertex_indices: Vec<usize>,
	/// Distance to each vertex from the cycle's zero point
	///
	/// Values are in range [0, 1)
	///
	/// Position and interpretation of the cycle's zero point depends on its shape
	pub vertex_positions: Vec<f32>,
	/// Indices into [`LevelData::detectors`]
	/// that identify the detectors that lie on the cycle, and numerical offsets,
	/// that identify which vertex this detector comes after
	pub detector_indices: Vec<(usize, usize)>,
	/// When the cycle can be turned
	pub turnability: CycleTurnability,
	/// Group this cycle belongs to
	pub group: usize,
	/// The relative orientation of the cycle in regards to the group
	pub orientation_within_group: LinkedCycleDirection,
}

#[derive(Debug, Clone, Reflect)]
pub struct DetectorData {
	/// List of groups this detector points to.
	pub linked_groups: Vec<OneWayLinkData>,
}

/// Description of a group of cycles
#[derive(Debug, Clone, Reflect)]
pub struct GroupData {
	/// Indices into [`LevelData::cycles`]
	/// identifies the cycles that belong to the group
	/// All cycles that have to turn when this cycle is turned and in which direction relative to the nominal direction of the entire group
	pub cycles: Vec<(usize, LinkedCycleDirection)>,
	/// One Way Links to other groups that should get triggered by this one.
	pub linked_groups: Vec<OneWayLinkData>,
	/// List of cycle indices this group contains that have detectors on them.
	pub outgoing_detector_cycles: Vec<usize>,
}

/// Description of a declared (and visualized) cycle link
#[derive(Debug, Clone, Copy, Reflect)]
pub struct DeclaredLinkData {
	/// Cycle from which the link goes
	pub source_cycle: usize,
	/// Cycle to which the link goes
	pub dest_cycle: usize,
	/// Relative turning direction between the linked cycles
	pub direction: LinkedCycleDirection,
}

/// Description of a declared (and visualized) cycle link
#[derive(Debug, Clone, Copy, Reflect)]
pub struct DeclaredOneWayLinkData {
	/// Cycle/Detector id from which the link goes
	pub source: usize,
	/// Cycle to which the link goes
	pub dest_cycle: usize,
	/// Relative turning direction between the linked cycles
	pub direction: LinkedCycleDirection,
	/// How many copies of this link are present
	pub multiplicity: u64,
}

#[derive(Debug, Clone, Copy, Reflect, PartialEq, Eq)]
pub struct OneWayLinkData {
	/// The group this link goes to
	pub target_group: usize,
	/// Relative turning direction between the source (either a group or a detector) and target group
	pub direction: LinkedCycleDirection,
	/// How many copies of this link are present
	pub multiplicity: u64,
}

// This segment is unused because the relevant optimisation is not currently being done.
#[expect(dead_code)]
impl OneWayLinkData {
	pub fn try_merge(a: &OneWayLinkData, b: &OneWayLinkData) -> Option<OneWayLinkData> {
		let (direction, multiplicity) = if a.direction == b.direction {
			(a.direction, a.multiplicity + b.multiplicity)
		} else {
			match u64::cmp(&a.multiplicity, &b.multiplicity) {
				std::cmp::Ordering::Less => (b.direction, b.multiplicity - a.multiplicity),
				std::cmp::Ordering::Equal => (LinkedCycleDirection::Coincident, 0),
				std::cmp::Ordering::Greater => (a.direction, a.multiplicity - b.multiplicity),
			}
		};
		Some(OneWayLinkData {
			target_group: a.target_group,
			direction,
			multiplicity,
		})
	}

	/// A comparison function that sorts objects in a way as to place mergeable ones together
	pub fn compare(&self, other: &Self) -> std::cmp::Ordering {
		// First we compare unmergeable attributes
		match self.target_group.cmp(&other.target_group) {
			core::cmp::Ordering::Equal => {}
			ord => return ord,
		}
		// Then the ordering on the mergeable ones is arbitrary, so we leave it undefined
		// We do have to uphold that PartialOrd and PartialEq agree though.
		match (self.direction, other.direction) {
			(LinkedCycleDirection::Coincident, LinkedCycleDirection::Coincident) => {}
			(LinkedCycleDirection::Coincident, LinkedCycleDirection::Inverse) => {
				return std::cmp::Ordering::Greater
			}
			(LinkedCycleDirection::Inverse, LinkedCycleDirection::Coincident) => {
				return std::cmp::Ordering::Less
			}
			(LinkedCycleDirection::Inverse, LinkedCycleDirection::Inverse) => {}
		}
		self.multiplicity.cmp(&other.multiplicity)
	}
}

/// Computed placement of a cycle
#[derive(Component, Clone, Copy, PartialEq, Debug, Reflect)]
pub struct CyclePlacement {
	/// Position of the cycle center. This is the position of the cycle entity
	pub position: Vec2,
	/// Shape of the cycle's perimeter
	pub shape: CycleShape,
}

impl CyclePlacement {
	pub fn sample(&self, t: f32) -> Vec2 {
		match self.shape {
			CycleShape::Circle(radius) => self.position + radius * Vec2::from_angle(t * TAU),
		}
	}
}

/// Description of the shape of a cycle's perimeter
#[derive(Clone, Copy, PartialEq, Debug, Reflect)]
pub enum CycleShape {
	/// Circle defined by a radius, centered in the cycle's position
	Circle(f32),
}

impl CycleShape {
	/// Length of the cycle's perimeter, in world units
	pub fn length(&self) -> f32 {
		match self {
			Self::Circle(radius) => TAU * radius,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub enum ObjectType {
	Box,
	Player,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub enum GlyphType {
	Button,
	Flag,
}

#[derive(Component, Clone, Copy, PartialEq, Eq, Hash, Debug, Reflect)]
pub enum ObjectData {
	Box(Option<LogicalColor>),
	Player,
}

#[derive(Component, Clone, Copy, PartialEq, Debug, Reflect)]
pub enum GlyphData {
	Button(Option<(LogicalColor, ButtonColorLabelAppearence)>),
	Flag,
}

#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub enum ThingType {
	Object(ObjectType),
	Glyph(GlyphType),
}

/// Type describing any gameplay object
#[derive(Component, Debug, Clone, Copy, Reflect)]
pub enum ThingData {
	Object(ObjectData),
	Glyph(GlyphData),
}

/// Defines conditions under which a cycle may be turned
#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Default, Hash, Reflect)]
pub enum CycleTurnability {
	/// Cycle may be turned anytime
	#[default]
	Always,
	/// Cycle may be turned when an [`ObjectType::Player`] object lies on one of its vertices
	WithPlayer,
	/// Cycle may never be turned directly
	Never,
}

/// Relative direction of two cycles that are to turn together
#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Default, Reflect)]
pub enum LinkedCycleDirection {
	/// The cycles will turn in the same direction
	#[default]
	Coincident,
	/// The cycles will turn in opposite directions
	Inverse,
}

/// Logical color of a box or a button.
/// Colored buttons require a box of the same color
#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Hash, Reflect)]
pub struct LogicalColor {
	/// Index of the color.
	/// For pictogram colors, this is the ID of the pictogram.
	/// For numeric colors, this is the numeric value.
	pub color_index: usize,
	/// True to use a pictogram color, false to use a numeric color.
	///
	/// The pictogram or number is intended to be displayed inside the box
	/// and on a label next to the button.
	///
	/// Pictogram and numeric color with the same [`color_index`](Self::color_index)
	/// are entirely different colors.
	///
	/// The reason pictogram colors are identified by index (rather than using
	/// an enum to list all possible pictograms) is to allow for quickly expanding
	/// the pictogram list with minimal changes to the code.
	/// Out-of-range pictogram indices will not render properly, but they
	/// are not a hard error.
	pub is_pictogram: bool,
}

/// Describes how a button with logical color should be labeled.
///
/// Logical color on button takes the appearence of a label inside
/// or near the button. This structure describes the shape and
/// positioning of the label.
#[derive(Component, Clone, Copy, PartialEq, Debug, Default, Reflect)]
pub struct ButtonColorLabelAppearence {
	/// Position of the label
	pub position: ButtonColorLabelPosition,
	/// Whether the label should have an arrow/tag-like shape.
	/// Otherwise the label is square.
	///
	/// This has no effect if position is
	/// [`Inside`](ButtonColorLabelPosition::Inside).
	pub has_arrow_tip: bool,
}

/// Positioning of the label of a button with logical color
#[derive(Clone, Copy, PartialEq, Debug, Default, Reflect)]
pub enum ButtonColorLabelPosition {
	/// The label will be placed inside the button,
	/// and it will align perfectly with the matching label on the box.
	/// If a box is present, the label will by obscured by it.
	#[default]
	Inside,
	/// The label will be placed somewhere around the box.
	/// Specify the clock angle (0 = up, positive = clockwise)
	/// corresponding to the position of the label relative to the box.
	///
	/// For some angles, the arrow tip may end up pointing outside the box.
	AnglePlaced(f32),
	/// The label will be placed somewhere around the box
	/// and rotated so that one of its sides (ot its arrow tip, if present)
	/// is directed towards the center.
	/// Specify the clock angle (0 = up, positive = clockwise)
	/// corresponding to the position of the label relative to the box
	/// and its rotation angle.
	///
	/// The whole label is rotated, including the color sprite, so be careful with this
	/// in combination with rotationally symmetric sprites.
	AngleRotated(f32),
}

/// The rendering of a cycle's center sprite
///
/// Indicates the center sprite position relative to the cycle position,
/// or [`None`] if the center sprite is not to be drawn
#[derive(Component, Clone, Copy, PartialEq, Debug, Reflect)]
pub struct CycleCenterSpriteAppearence(pub Option<Vec2>);

impl LogicalColor {
	pub fn new(color_index: usize) -> Self {
		Self {
			color_index,
			is_pictogram: false,
		}
	}

	pub fn pictogram(color_index: usize) -> Self {
		Self {
			color_index,
			is_pictogram: true,
		}
	}
}

impl std::fmt::Display for CyclePlacement {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self.shape {
			CycleShape::Circle(r) => {
				write!(f, "[x={} y={} r={r}]", self.position.x, self.position.y)
			}
		}
	}
}

impl std::ops::Mul for LinkedCycleDirection {
	type Output = Self;
	fn mul(self, rhs: Self) -> Self::Output {
		if self == rhs {
			LinkedCycleDirection::Coincident
		} else {
			LinkedCycleDirection::Inverse
		}
	}
}

impl std::ops::MulAssign for LinkedCycleDirection {
	fn mul_assign(&mut self, rhs: Self) {
		*self = *self * rhs
	}
}

impl std::ops::Mul<i64> for LinkedCycleDirection {
	type Output = i64;

	fn mul(self, rhs: i64) -> Self::Output {
		match self {
			LinkedCycleDirection::Coincident => rhs,
			LinkedCycleDirection::Inverse => -rhs,
		}
	}
}

impl From<ObjectData> for ObjectType {
	fn from(value: ObjectData) -> Self {
		match value {
			ObjectData::Box(_) => Self::Box,
			ObjectData::Player => Self::Player,
		}
	}
}

impl From<GlyphData> for GlyphType {
	fn from(value: GlyphData) -> Self {
		match value {
			GlyphData::Button(_) => Self::Button,
			GlyphData::Flag => Self::Flag,
		}
	}
}

impl From<ThingData> for ThingType {
	fn from(value: ThingData) -> Self {
		match value {
			ThingData::Glyph(g) => Self::Glyph(g.into()),
			ThingData::Object(o) => Self::Object(o.into()),
		}
	}
}
