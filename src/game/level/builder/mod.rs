//! Contains declarations and general helper structs for the [`LevelBuilder`]
//! Implementations of [`LevelBuilder`] are in layout and builder submodules.

/// Contains Error structs and variants for level construction logic.
pub mod error;
/// Contains the logic for laying out the geometry of a level inside a [`LevelBuilder`] impl.
mod layout;
/// Contains the logic for assembling and verifying level datastructures inside a [`LevelBuilder`] impl.
mod logic;

use super::*;
use bevy::math::bounding::Aabb2d;

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
	/// Bounding box
	bounding_box: Option<Aabb2d>,
	/// A conversion factor from logical "epilang" units to level units.
	/// If `None` then this conversion is computed in order to fit the level into the usual bounding box.
	scale_override: Option<f32>,
	/// How zoom should be initialized when entering the level
	initial_zoom: Option<f32>,
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

#[derive(Clone, Copy, Debug)]
struct IntermediateVertexData {
	/// Position of the vertex, if it has been placed yet
	pub position: IntermediateVertexPosition,
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
	///
	/// Unlike in the level description structures,
	/// **this is the absolute position in global coordinates**
	///
	/// Value of `None` indicates the center has not been placed
	/// (default position will be used). Value of `Some(None)` indicates
	/// the center indicator has been explicitly toggled off.
	pub center_sprite_position: Option<Option<Vec2>>,
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
	/// Exactly one cycle that owns the vertex has been placed
	/// and the vertex may still be moved along it
	Partial(PartiallyBoundVertexPosition),
}

impl IntermediateVertexPosition {
	fn get_fixed(&self) -> Option<Vec2> {
		match self {
			Self::Fixed(pos) => Some(*pos),
			_ => None,
		}
	}
}

/// Placement of a vertex that belongs to a placed cycle,
/// but does not yet have a definite placement
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct PartiallyBoundVertexPosition {
	/// Index of the cycle that owns the vertex
	owner_cycle: usize,
	/// Index of the vertex within the owner cycle's vertex list
	index_in_owner: usize,
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

/// Container that holds one or two of something
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum OneTwo<T> {
	One(T),
	Two(T, T),
}
use OneTwo::*;

impl<T> OneTwo<T> {
	fn add_to_one(self, x: T) -> Result<Self, (T, T, T)> {
		match self {
			One(a) => Ok(Two(a, x)),
			Two(a, b) => Err((a, b, x)),
		}
	}

	fn first(self) -> T {
		match self {
			One(x) | Two(x, _) => x,
		}
	}
}

impl<T> IntoIterator for OneTwo<T> {
	type Item = T;
	type IntoIter = <Vec<T> as IntoIterator>::IntoIter;
	fn into_iter(self) -> Self::IntoIter {
		match self {
			One(a) => vec![a].into_iter(),
			Two(a, b) => vec![a, b].into_iter(),
		}
	}
}

/// Result type that allows both a value and an error.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ResultNonExclusive<T, E> {
	Ok(T),
	Partial(T, E),
	Err(E),
}

#[allow(dead_code)]
impl<T, E> ResultNonExclusive<T, E> {
	pub fn value(self) -> Option<T> {
		match self {
			ResultNonExclusive::Ok(value) => Some(value),
			ResultNonExclusive::Partial(value, _) => Some(value),
			ResultNonExclusive::Err(_) => None,
		}
	}

	pub fn error(self) -> Option<E> {
		match self {
			ResultNonExclusive::Ok(_) => None,
			ResultNonExclusive::Partial(_, err) => Some(err),
			ResultNonExclusive::Err(err) => Some(err),
		}
	}

	pub fn into<A, B>(self) -> ResultNonExclusive<A, B>
	where
		A: From<T>,
		B: From<E>,
	{
		match self {
			ResultNonExclusive::Ok(value) => ResultNonExclusive::Ok(value.into()),
			ResultNonExclusive::Partial(value, err) => {
				ResultNonExclusive::Partial(value.into(), err.into())
			}
			ResultNonExclusive::Err(err) => ResultNonExclusive::Err(err.into()),
		}
	}

	pub fn map_err<E2>(self, map: impl FnOnce(E) -> E2) -> ResultNonExclusive<T, E2> {
		match self {
			ResultNonExclusive::Ok(value) => ResultNonExclusive::Ok(value),
			ResultNonExclusive::Partial(value, err) => ResultNonExclusive::Partial(value, map(err)),
			ResultNonExclusive::Err(err) => ResultNonExclusive::Err(map(err)),
		}
	}
}

impl<T, E> From<(T, Option<E>)> for ResultNonExclusive<T, E> {
	fn from(value: (T, Option<E>)) -> Self {
		match value {
			(value, None) => ResultNonExclusive::Ok(value),
			(value, Some(err)) => ResultNonExclusive::Partial(value, err),
		}
	}
}

impl<T, E> From<E> for ResultNonExclusive<T, E> {
	fn from(err: E) -> Self {
		ResultNonExclusive::Err(err)
	}
}
