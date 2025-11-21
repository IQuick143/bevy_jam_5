use super::*;

/// Error data for [`VertexSolverError::CycleDoesNotContainVertex`]
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct CycleDoesNotContainVertexError {
	/// Index of the affected cycle
	pub cycle: usize,
	/// Placement that was requested for the cycle
	pub placement: CyclePlacement,
	/// Index of the vertex with fixed position
	/// that would not lie on the cycle as placed
	pub vertex: usize,
	/// Position of the failing vertex
	pub position: Vec2,
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

/// Marker error type for when one-way links for an oriented cycle
///
/// TODO: Include actual data in here to help debug the cycle
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct OneWayLinkLoopError;

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
	/// [`place_circle`](LevelBuilder::place_circle) was called
	/// on a cycle that had already been placed
	CycleAlreadyPlaced(usize),
	/// A vertex positioning operation was called on a vertex that
	/// already has a placement too specific to perform the operation
	/// ## Causes
	/// - [`place_vertex`](LevelBuilder::place_vertex)
	VertexAlreadyPlaced(usize),
	/// Two cycles are linked, but they share a vertex
	OverlappedLinkedCycles(OverlappedLinkedCyclesError),
	/// There is a loop in cycle links, and their directions are contradicting
	CycleLinkageConflict(usize, usize),
	/// There is a loop in one-way cycle links
	OneWayLinkLoop(OneWayLinkLoopError),
	/// Vertices could not be assigned positions
	VertexSolverError(VertexSolverError),
}

impl LevelBuilderError {
	/// Checks whether the error is only a warning or a hard error
	pub fn is_warning(&self) -> bool {
		matches!(
			self,
			Self::VertexSolverError(VertexSolverError::VertexRemainsUndecided { .. })
				| Self::VertexSolverError(VertexSolverError::VertexHasNoCycle(_))
				| Self::VertexSolverError(VertexSolverError::UnnecessaryHint(_))
		)
	}

	/// Checks whether the error is a weak warning
	///
	/// Weak warnings should not be logged unless they accompany an error
	pub fn is_weak(&self) -> bool {
		matches!(
			self,
			Self::VertexSolverError(VertexSolverError::VertexRemainsUndecided { .. })
		)
	}
}

/// Errors specific to vertex position solver
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum VertexSolverError {
	/// There are no valid positions for a vertex
	VertexHasNoPointsAvailable { vertex: usize },
	/// Two vertices would need to be placed at the same position
	TwoVerticesCollide { vertex_a: usize, vertex_b: usize },
	/// Position of a vertex is not bounded
	VertexIsUnconstrained { vertex: usize },
	/// Vertex has multiple available placements
	/// and its position must be decided heuristically
	VertexRemainsUndecided { vertex: usize },
	/// A vertex was not assigned to any parent cycle
	VertexHasNoCycle(usize),
	/// Pinning pair vertices based on heuristic
	/// failed because there is another unpinned pair
	/// on the same cycles
	///
	/// Contains the pair whose pinning was attempted
	/// and one of the other vertices that blocked it
	CannotPinTwinPair([usize; 3]),
	/// Pinning an unsaturated pair vertex failed because another
	/// cycle is intersecting it between the possible positions
	CannotPinUnsaturatedPair(usize),
	/// Vertices do not follow their common cycle in clockwise order
	VerticesNotClockwise { cycle: usize, vertices: [usize; 3] },
	/// A vertex has been explicitly placed at a position that its cycle does not intersect
	CycleDoesNotContainVertex(CycleDoesNotContainVertexError),
	/// A vertex was given a placement hint that did not affect its final position
	UnnecessaryHint(usize),
}

impl std::error::Error for OneWayLinkLoopError {}

impl std::fmt::Display for OneWayLinkLoopError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "one way links form an oriented cycle")
	}
}

impl std::error::Error for VertexSolverError {}

impl std::fmt::Display for VertexSolverError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::VertexHasNoPointsAvailable { vertex } => {
				write!(f, "there is no valid position to place vertex {vertex}")
			}
			Self::TwoVerticesCollide { vertex_a, vertex_b } => {
				write!(f, "vertices {vertex_a} and {vertex_b} require placement at the same position")
			}
			Self::VertexIsUnconstrained { vertex } => {
				write!(f, "position of vertex {vertex} has not been set")
			}
			Self::VertexRemainsUndecided { vertex } => {
				write!(f, "position of vertex {vertex} is ambiguous")
			}
			Self::VertexHasNoCycle(i) => write!(f, "vertex {i} does not lie on any cycle"),
			Self::CannotPinTwinPair([a, b, c]) => write!(f, "vertices {a}, {b}, and {c} could not be pinned heuristically because they lie on the same cycle"),
			Self::CannotPinUnsaturatedPair(i) => write!(f, "vertex {i} could not be pinned heuristically because its owner cycle is intersected between its possible positions"),
			Self::VerticesNotClockwise { cycle, vertices: [a, b, c] } => write!(f, "vertices {a}, {b}, and {c} on cycle {cycle} do not appear in clockwise order"),
			Self::CycleDoesNotContainVertex(e) => write!(
				f,
				"vertex {} has been explicitly placed at position {} which does not lie on cycle {} at {:?}",
				e.cycle,
				e.placement,
				e.vertex,
				e.position
			),
			Self::UnnecessaryHint(i) => write!(f, "placement hint on vertex {i} did not affect its position"),
		}
	}
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
			Self::OverlappedLinkedCycles(e) => write!(f, "Cycles {} and {} cannot be linked because they share vertex {}.", e.source_cycle, e.dest_cycle, e.shared_vertex),
			Self::CycleLinkageConflict(a, b) => write!(f, "Cycles {a} and {b} cannot be linked because they are already linked in the opposite direction."),
			Self::OneWayLinkLoop(e) => e.fmt(f),
			Self::VertexSolverError(e) => e.fmt(f),
		}
	}
}

/// Sequence of errors and warnings emited by the level builder
#[derive(Clone, Debug, Default, Deref, DerefMut)]
pub struct LevelBuilderErrorLog(pub Vec<LevelBuilderError>);

impl LevelBuilderErrorLog {
	/// Checks whether there are any critical errors present, or just warnings
	pub fn is_ok(&self) -> bool {
		self.iter().all(LevelBuilderError::is_warning)
	}

	/// Checks whether there are any errors or warnings present that can be logged
	pub fn has_loggable_entries(&self) -> bool {
		!self.iter().all(LevelBuilderError::is_weak)
	}

	/// Logs all errors at appropriate log levels using the Bevy logging facilities
	pub fn log_with_bevy(&self, level_name: &str) {
		if self.has_loggable_entries() {
			warn!("Level {level_name} encountered errors during loading:");
			for err in self.iter() {
				if err.is_warning() {
					warn!("{err}");
				} else {
					error!("{err}");
				}
			}
		}
	}
}
