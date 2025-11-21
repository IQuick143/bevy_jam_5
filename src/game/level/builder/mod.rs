//! Contains declarations and general helper structs for the [`LevelBuilder`]
//! Implementations of [`LevelBuilder`] are in layout and builder submodules.

/// Contains Error structs and variants for level construction logic.
pub mod error;
/// Contains the logic for laying out the geometry of a level inside a [`LevelBuilder`] impl.
mod layout;
/// Contains the logic for assembling and verifying level datastructures inside a [`LevelBuilder`] impl.
mod logic;
mod vertex_solver;

use super::*;

/// Helper object for constructing a valid [`LevelData`]
#[derive(Debug)]
pub struct LevelBuilder {
	/// Name of the level, if already set
	name: Option<String>,
	/// Hint or comment on the level, if already set
	hint: Option<String>,
	/// Placements of vertices, if they have been placed yet
	vertices: Vec<IntermediateVertexData>,
	/// Placements of cycles, if they have been placed yet
	cycles: Vec<IntermediateCycleData>,
	/// Detectors and their outgoing links
	detectors: Vec<IntermediateDetectorData>,
	/// Cycle links that have been explicitly added (no symmetry or transitivity)
	declared_links: Vec<DeclaredLinkData>,
	/// One way links between cycles that have been explicitly added (no transitivity)
	declared_one_way_cycle_links: Vec<DeclaredOneWayLinkData>,
	/// One way links from detectors that have been explicitly added (no transitivity)
	// TODO: Populate and use this
	#[expect(dead_code)]
	declared_one_way_detector_links: Vec<DeclaredOneWayLinkData>,
	/// Bounding box (or parts thereof) set by the caller
	explicit_bounding_box: PartialBoundingBox,
	/// A conversion factor from logical "epilang" units to level units.
	/// If `None` then this conversion is computed in order to fit the level into the usual bounding box.
	scale_override: Option<f32>,
	/// How zoom should be initialized when entering the level
	initial_zoom: Option<f32>,
	/// How camera position should be initialized when entering the level
	/// (in the builder's coordinate space)
	initial_camera_pos: PartialVec2,
}

/// Enumerates the possible sets of positions
/// for automatic cycle-bound placement of labels for colored buttons
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CycleBoundColorLabelPositionSet {
	/// The label can be placed to the left or right of the button
	LeftRight,
	/// The label can be placed above or below the button
	AboveBelow,
	/// The label can be placed in any cardinal direction from the button
	CardinalDirections,
	/// The label can be placed in any direction from the button,
	/// and it will be aligned laterally
	AllDirections,
	/// Thelabel can be placed in any direction from the button,
	/// and it will be rotated
	AllDirectionsRotated,
}

/// Bounding box whose bounds can be set (or not set) individually
#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct PartialBoundingBox {
	pub left: Option<f32>,
	pub top: Option<f32>,
	pub right: Option<f32>,
	pub bottom: Option<f32>,
}

/// Vector whose coordinates can be set (or not set) individually
#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct PartialVec2 {
	pub x: Option<f32>,
	pub y: Option<f32>,
}

#[derive(Clone, Copy, Debug)]
struct IntermediateVertexData {
	/// Position of the vertex, if it has been placed yet
	pub position: IntermediateVertexPosition,
	/// Position to use as a hint when placing the vertex, if any
	pub hint_position: Option<Vec2>,
	/// Object that lies on the vertex, if any
	pub object: Option<ObjectData>,
	/// Glyph that lies on the vertex, if any
	pub glyph: Option<GlyphData>,
	/// Appearence of the button color label,
	/// if it is bound to a parent cycle and set to be resolved at build time
	pub color_label_appearence: Option<CycleBoundButtonColorLabelAppearence>,
}

#[derive(Clone, Debug)]
struct IntermediateCycleData {
	/// Placement of the cycle, if it has been placed yet
	pub placement: Option<CyclePlacement>,
	/// Position of the cycle's center indicator, if it has been placed yet
	pub center_sprite_position: IntermediateCycleCenterSpritePosition,
	/// Indices into [`LevelData::vertices`]
	/// that identify the vertices that lie on the cycle, in clockwise order
	pub vertex_indices: Vec<usize>,
	/// When the cycle can be turned
	pub turnability: CycleTurnability,
	/// How other cycles turn together with this one
	pub linked_cycle: IntermediateLinkStatus,
	/// Outgoing oneways
	pub outgoing_one_way_links: Vec<OneWayIntermediateData>,
	/// Detectors, pairs of detector ID's and their offsets on this cycle. (The second, positional, index is the index of the vertex this detector comes *after*)
	pub placed_detectors: Vec<(usize, usize)>,
	/// Walls, values are their offsets on this cycle. (The positional index is the index of the vertex this wall comes *after*)
	pub walls: Vec<usize>,
}

/// Position of a cycle's center indicator specified by the user
#[derive(Clone, Copy, PartialEq, Debug, Default)]
enum IntermediateCycleCenterSpritePosition {
	/// User has not yet chosen a position for the center indicator
	#[default]
	Unspecified,
	/// User has requested that the center indicator not be drawn
	Disabled,
	/// User has placed the center indicator at a specific position
	///
	/// Unlike in the level description structures,
	/// **this is the absolute position in global coordinates**
	Placed(Vec2),
}

impl IntermediateCycleCenterSpritePosition {
	pub fn placed(self) -> Option<Vec2> {
		if let Self::Placed(p) = self {
			Some(p)
		} else {
			None
		}
	}
}

#[derive(Clone, Debug)]
struct IntermediateDetectorData {
	/// Outgoing links
	links: Vec<OneWayIntermediateData>,
}

/// Data about cycle linkages (bothway), used secondarily to compute the union of groups of linked cycles.
#[derive(Clone, Copy, Debug)]
enum IntermediateLinkStatus {
	/// No link has been declared
	None,
	/// A link to another cycle is known, but it is yet unknown what group these cycles are in.
	Cycle(usize, LinkedCycleDirection),
	/// The cycle is linked to a parent group.
	Group(usize, LinkedCycleDirection),
}

#[derive(Clone, Copy, Debug)]
struct OneWayIntermediateData {
	target_cycle: usize,
	direction: LinkedCycleDirection,
	multiplicity: u64,
}

/// Placement of a vertex while the layout is being built
#[derive(Clone, Copy, PartialEq, Debug)]
enum IntermediateVertexPosition {
	/// The vertex has not been placed yet
	Free,
	/// The vertex has a fixed position
	Fixed(Vec2),
}

impl IntermediateVertexPosition {
	fn get_fixed(&self) -> Option<Vec2> {
		match self {
			Self::Fixed(pos) => Some(*pos),
			_ => None,
		}
	}
}

/// Placeholder appearence for a button color label that will be
/// styled with regard to the cycle it is placed on
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct CycleBoundButtonColorLabelAppearence {
	/// Index of the cycle relative to which the label will be positioned
	owner_cycle: usize,
	/// Whether the labels should be oriented away from the cycle instead of towards it
	place_outside_cycle: bool,
	/// Whether the labels should be pentagon arrows instead of square
	has_arrow_tip: bool,
	/// Set of directions from which the final angle can be picked
	positions: CycleBoundColorLabelPositionSet,
}

/// The result of building a level
#[derive(Clone, Debug)]
pub struct LevelBuildResult {
	/// The level data
	///
	/// If hard errors are present, this will contain a best-effort
	/// subset of the level as described, with [`LevelData::is_valid`] set to false
	pub level: LevelData,
	/// Errors and warnings emited by the builder
	pub errors: error::LevelBuilderErrorLog,
}
