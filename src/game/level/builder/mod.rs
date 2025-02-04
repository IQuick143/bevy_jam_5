pub mod error;
/// Contains declarations and general helper structs for the [`LevelBuilder`]
/// Implementations of [`LevelBuilder`] are in layout and builder submodules.
mod layout;
mod logic;

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
	/// Cycle links that have been explicitly added (no symmetry or transitivity)
	declared_links: Vec<DeclaredLinkData>,
	/// One way links that have been explicitly added (no transitivity)
	declared_one_way_links: Vec<DeclaredLinkData>,
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
}

#[derive(Clone, Copy, Debug)]
enum IntermediateLinkStatus {
	None,
	Cycle(usize, LinkedCycleDirection),
	Group(usize, LinkedCycleDirection),
}

#[derive(Clone, Copy, Debug)]
struct OneWayIntermediateData {
	target_cycle: usize,
	direction: LinkedCycleDirection,
	// /// Information on whether this link interacts with detectors, if that information is known already
	// has_detectors: Option<bool>
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

impl IntermediateVertexPosition {
	fn get_fixed(&self) -> Option<Vec2> {
		match self {
			Self::Fixed(pos) => Some(*pos),
			_ => None,
		}
	}
}

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

fn intersect_circles(c1: Vec2, c2: Vec2, r1: f32, r2: f32) -> Option<OneTwo<Vec2>> {
	let d_sq = c1.distance_squared(c2);
	// Do not even try if the circles share a center point
	const CENTER_OVERLAP_THRESHOLD: f32 = 0.001;
	if d_sq < CENTER_OVERLAP_THRESHOLD {
		return None;
	}
	// First we find the projection of the intersections onto the line that connects center points
	// This is the distance of that point along that line, as a multiple of distance between the centers
	let rel_midpoint = (r1.powi(2) - r2.powi(2)) / 2.0 / d_sq + 0.5;
	let midpoint = c1.lerp(c2, rel_midpoint);
	// Now we try to construct the actual intersection points, they will lie at the intersections
	// of the normal at rel_midpoint with one of the circles
	let normal_offset_sq = r1.powi(2) - rel_midpoint.powi(2) * d_sq;
	// Round here, we want to cacth a single intersection with a margin of error
	const TANGENTIAL_INTERSECTION_THRESHOLD: f32 = 0.001;
	if normal_offset_sq.abs() < TANGENTIAL_INTERSECTION_THRESHOLD {
		Some(One(midpoint))
	} else if normal_offset_sq > 0.0 {
		let normal_offset = normal_offset_sq.sqrt();
		let normal_unit = (c2 - c1).normalize().perp();
		let offset = normal_offset * normal_unit;
		Some(Two(midpoint + offset, midpoint - offset))
	} else {
		None
	}
}

fn approx_eq(a: f32, b: f32) -> bool {
	// Threshold is intentionally chosen to be fairly large
	// so as to make up for rounding errors in arithmetics
	// and/or human inputs
	const THRESHOLD: f32 = 0.001;
	(a - b).abs() < THRESHOLD * a.max(b)
}
