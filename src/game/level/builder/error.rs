use super::*;

/// Error data for [`LevelBuilderError::CycleDoesNotContainVertex`]
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct CycleDoesNotContainVertexError {
	/// Index of the cycle whose attempted placement failed
	pub placed_cycle: usize,
	/// Placement that was requested for the cycle
	pub requested_placement: CyclePlacement,
	/// Index of the vertex with fixed position
	/// that would not lie on the cycle as placed
	pub failing_vertex: usize,
	/// Position of the failing vertex
	pub vertex_position: Vec2,
}

/// Error data for [`LevelBuilderError::CyclesDoNotIntersect`]
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct CyclesDoNotIntersectError {
	/// Index of the cycle whose attempted placement failed
	pub placed_cycle: usize,
	/// Placement that was requested for the cycle
	pub requested_placement: CyclePlacement,
	/// Index of the already-placed cycle that shared a vertex
	/// with the one being placed
	pub existing_cycle: usize,
	/// Placement of the already-placed cycle
	pub existing_placement: CyclePlacement,
	/// Index of the vertex that the cycles share
	/// that could not be placed because the cycles do not intersect
	pub failing_vertex: usize,
}

/// Error data for [`LevelBuilderError::CyclesDoNotIntersectTwice`]
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct CyclesDoNotIntersectTwiceError {
	/// Index of the cycle whose attempted placement failed
	pub placed_cycle: usize,
	/// Placement that was requested for the cycle
	pub requested_placement: CyclePlacement,
	/// Index of the already-placed cycle that shared
	/// two vertices with the one being placed
	pub existing_cycle: usize,
	/// Placement of the already-placed cycle
	pub existing_placement: CyclePlacement,
	/// Index of the vertex that has already been placed at the only
	/// intersection between the cycles
	pub existing_vertex: usize,
	/// Position of the intersection (and the already-placed vertex)
	pub vertex_position: Vec2,
	/// Index of the vertex that the cycles share
	/// that could not be placed because the cycles only intersect once
	pub failing_vertex: usize,
}

/// Error data for [`LevelBuilderError::TooManyVerticesInCycleIntersection`]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct TooManyVerticesInCycleIntersectionError {
	/// Index of the cycle whose attempted placement failed
	pub placed_cycle: usize,
	/// Index of the already-placed cycle that shared
	/// several vertices with the one being placed
	pub existing_cycle: usize,
	/// Indices of three of the vertices that are shared by the cycles
	pub shared_vertices: [usize; 3],
}

/// Error data for [`LevelBuilderError::OverlappedLinkedCycles`]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct OverlappedLinkedCyclesError {
	/// Index of the cycle where a (possibly transitive) link starts
	pub source_cycle: usize,
	/// Index of the cycle where a link ends
	pub dest_cycle: usize,
	/// Index of the vertex shared by the two cycles
	pub shared_vertex: usize,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum LevelBuilderError {
	/// [`set_level_name`](LevelBuilder::set_level_name)
	/// was called more than once
	LevelNameAlreadySet,
	/// [`set_level_hint`](LevelBuilder::set_level_hint)
	/// was called more than once
	LevelHintAlreadySet,
	/// An out-of-range index was used to reference a cycle
	CycleIndexOutOfRange(usize),
	/// An out-of-range index was used to reference a vertex
	VertexIndexOutOfRange(usize),
	/// An out-of-range index was used to reference a detector
	DetectorIndexOutOfRange(usize),
	/// A vertex shows up multiple times in the same cycle
	RepeatingVertexInCycle(usize),
	/// A detector was attempted on a cycle with no vertices
	DetectorOnEmptyCycle,
	/// A wall was attempted on a cycle with no vertices
	WallOnEmptyCycle,
	/// A cycle has been explicitly assigned negative radius
	CycleRadiusNotPositive(usize, f32),
	/// [`build`](LevelBuilder::build) was called
	/// while a cycle had not been placed yet
	UnplacedCycle(usize),
	/// [`place_cycle`](LevelBuilder::place_cycle) was called
	/// on a cycle that had already been placed
	CycleAlreadyPlaced(usize),
	/// A vertex or cycle positioning operation was called
	/// in a way that would cause a vertex to not lie on its cycle.
	/// ## Causes
	/// - [`place_cycle`](LevelBuilder::place_cycle) called on a cycle
	///   that contains a vertex that already has fixed placement
	///   and it would not lie on the vertex
	/// - [`place_vertex`](LevelBuilder::place_vertex) called on a vertex
	///   that lies on a placed cycle with a position that does not lie
	///   on the cycle
	CycleDoesNotContainVertex(CycleDoesNotContainVertexError),
	/// Cycles that share a vertex have been placed in a way that they do not intersect
	CyclesDoNotIntersect(CyclesDoNotIntersectError),
	/// Cycles that share two vertices have been placed in a way that they
	/// only intersect tangentially (only enough space for one shared vertex)
	CyclesDoNotIntersectTwice(CyclesDoNotIntersectTwiceError),
	/// Two cycles share more than two vertices
	TooManyVerticesInCycleIntersection(TooManyVerticesInCycleIntersectionError),
	/// A vertex positioning operation was called on a vertex that
	/// already has a placement too specific to perform the operation
	/// ## Causes
	/// - [`place_vertex`](LevelBuilder::place_vertex) or
	///   [`place_vertex_at_angle`](LevelBuilder::place_vertex_at_angle)
	///   called on a fully placed vertex
	VertexAlreadyPlaced(usize),
	/// [`place_vertex_at_angle`](LevelBuilder::place_vertex) was called
	/// on a vertex that already has not yet been partially placed
	VertexNotPartiallyPlaced(usize),
	/// A placement operation was called in a way that would place vertices
	/// around a cycle out of their rotation order
	/// ## Causes
	/// - [`place_cycle`](LevelBuilder::place_cycle) called on a cycle that
	///   shares vertices with a cycle that is already placed, and their
	///   shared vertices (which would gain fixed placement by this operation)
	///   would be out-of-order on one of them
	/// - [`place_vertex`](LevelBuilder::place_vertex) or
	///   [`place_vertex_at_angle`](LevelBuilder::place_vertex_at_angle)
	///   called on a vertex on a placed cycle in a way that would place
	///   the target vertex out of the correct order
	VertexOrderViolationOnCycle(usize),
	/// Two cycles are linked, but they share a vertex
	OverlappedLinkedCycles(OverlappedLinkedCyclesError),
	/// There is a loop in cycle links, and their directions are contradicting
	CycleLinkageConflict(usize, usize),
	/// There is a loop in one-way cycle links
	OneWayLinkLoop,
}

impl std::error::Error for LevelBuilderError {}

impl std::fmt::Display for LevelBuilderError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::LevelNameAlreadySet => write!(f, "Level name has been set multiple times."),
			Self::LevelHintAlreadySet => write!(f, "Level hint has been set multiple times."),
			Self::VertexIndexOutOfRange(i) => write!(f, "Vertex {i} has been referenced, but there are not that many vertices."),
			Self::DetectorIndexOutOfRange(i) => write!(f, "Detector {i} has been referenced, but there are not that many detectors."),
			Self::CycleIndexOutOfRange(i) => write!(f, "Cycle {i} has been referenced, but there are not that many cycles."),
			Self::RepeatingVertexInCycle(i) => write!(f, "Cannot create cycle that contains vertex {i} multiple times."),
			Self::DetectorOnEmptyCycle => write!(f, "Cannot create cycle that contains no vertices and nonzero amount of detectors."),
			Self::WallOnEmptyCycle => write!(f, "Cannot create cycle that contains no vertices and nonzero amount of detectors."),
			Self::CycleRadiusNotPositive(i, r) => write!(f, "Radius of cycle {i} is not positive ({r})"),
			Self::UnplacedCycle(i) => write!(f, "Cannot finish layout because cycle {i} has not yet been placed."),
			Self::CycleAlreadyPlaced(i) => write!(f, "Cannot place cycle {i} because it has already been placed."),
			Self::VertexAlreadyPlaced(i) => write!(f, "Cannot place vertex {i} because it has already been (possibly implicitly) placed."),
			Self::VertexNotPartiallyPlaced(i) => write!(f, "Cannot place vertex {i} because it does not lie on any placed cycle."),
			Self::CycleDoesNotContainVertex(e) => write!(
				f,
				"Placement is not valid because cycle {} placed at {} would not contain vertex {} placed at {}.",
				e.placed_cycle,
				e.requested_placement,
				e.failing_vertex,
				e.vertex_position
			),
			Self::CyclesDoNotIntersect(e) => write!(
				f,
				"Cycle {} cannot be placed at {} because it shares vertex {} with cycle {} at {} and the cycles would not intersect.",
				e.placed_cycle,
				e.requested_placement,
				e.failing_vertex,
				e.existing_cycle,
				e.existing_placement
			),
			Self::CyclesDoNotIntersectTwice(e) => write!(
				f,
				"Cycle {} cannot be placed at {} because it shares vertices {} and {} with cycle {} at {} and the cycles would only intersect once at {}.",
				e.placed_cycle,
				e.requested_placement,
				e.existing_vertex,
				e.failing_vertex,
				e.existing_cycle,
				e.existing_placement,
				e.vertex_position
			),
			Self::TooManyVerticesInCycleIntersection(e) => write!(
				f,
				"Cycles {} and {} cannot be placed because they sharemore than two vertices: {}, {}, {}.",
				e.placed_cycle,
				e.existing_cycle,
				e.shared_vertices[0],
				e.shared_vertices[1],
				e.shared_vertices[2],
			),
			Self::VertexOrderViolationOnCycle(i) => write!(f, "Placement is not valid because vertices around cycle {i} would be out of order."),
			Self::OverlappedLinkedCycles(e) => write!(f, "Cycles {} and {} cannot be linked because they share vertex {}.", e.source_cycle, e.dest_cycle, e.shared_vertex),
			Self::CycleLinkageConflict(a, b) => write!(f, "Cycles {a} and {b} cannot be linked because they are already linked in the opposite direction."),
			Self::OneWayLinkLoop => write!(f, "One way links cannot form a cycle.")
		}
	}
}
