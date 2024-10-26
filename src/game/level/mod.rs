use bevy::prelude::*;

pub mod asset;
mod builder;
mod lex;
pub mod list;
pub mod list_asset;
pub mod parser;

/// Complete description of a level
#[derive(Debug, Clone, Reflect, Asset)]
pub struct LevelData {
	/// Name of the level
	pub name: String,
	/// Hint or comment that relates to the level, if any
	pub hint: Option<String>,
	/// Data for all vertices in the level
	pub vertices: Vec<VertexData>,
	/// Data for all cycles in the level
	pub cycles: Vec<CycleData>,
	/// Data for all groups of cycles in the level
	pub groups: Vec<GroupData>,
	/// List of group pairs, which cannot be turned at once
	pub forbidden_group_pairs: Vec<(usize, usize)>,
	/// Data for all cycle links that have been explicitly declared in the level file.
	/// Will be used for rendering the links
	pub declared_links: Vec<DeclaredLinkData>,
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
	///  Placement of the cycle
	pub placement: CyclePlacement,
	/// Indices into [`LevelData::vertices`]
	/// that identify the vertices that lie on the cycle, in clockwise order
	pub vertex_indices: Vec<usize>,
	/// When the cycle can be turned
	pub turnability: CycleTurnability,
	/// Group this cycle belongs to
	pub group: usize,
	/// The relative orientation of the cycle in regards to the group
	pub orientation_within_group: LinkedCycleDirection,
}

/// Description of a group of cycles
#[derive(Debug, Clone, Reflect)]
pub struct GroupData {
	/// Indices into [`LevelData::cycles`]
	/// identifies the cycles that belong to the group
	/// All cycles that have to turn when this cycle is turned and in which direction relative to the nominal direction of the entire group
	pub cycles: Vec<(usize, LinkedCycleDirection)>,
	// TODO: Links to other groups
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

/// Computed placement of a cycle
#[derive(Component, Clone, Copy, PartialEq, Debug, Reflect)]
pub struct CyclePlacement {
	/// Position of the center point of the cycle
	pub position: Vec2,
	/// Radius of the cycle
	pub radius: f32,
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

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Reflect)]
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
#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub enum CycleTurnability {
	/// Cycle may be turned anytime
	Always,
	/// Cycle may be turned when an [`ObjectType::Player`] object lies on one of its vertices
	WithPlayer,
	/// Cycle may never be turned directly
	Never,
}

/// Relative direction of two cycles that are to turn together
#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Reflect)]
pub enum LinkedCycleDirection {
	/// The cycles will turn in the same direction
	Coincident,
	/// The cycles will turn in opposite directions
	Inverse,
}

/// Logical color of a box or a button.
/// Colored buttons require a box of the same color
#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Reflect)]
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
		write!(
			f,
			"[x={} y={} r={}]",
			self.position.x, self.position.y, self.radius
		)
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
