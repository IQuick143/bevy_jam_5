//! Description of animation sequences

use bevy::prelude::*;

pub fn plugin(app: &mut App) {
	app.init_resource::<PlaybackSequence>()
		.init_resource::<PlaybackState>();
}

/// Single atomic step of a playback sequence
#[derive(Clone)]
pub enum PlaybackPrimitive {
	/// Sleep for a number of seconds before running the next step
	WaitSeconds(f32),
	/// Turn a cycle by a specified amount
	TurnCycle { target_cycle: usize, amount: i64 },
	/// Move input focus to a specific cycle
	SelectCycle(Option<usize>),
	/// Open and initialize a specific level by its ID
	EnterLevel(String),
}

/// Full script of an animation sequence
#[derive(Resource, Clone, Default, Deref, DerefMut)]
pub struct PlaybackSequence(pub Vec<PlaybackPrimitive>);

/// Progression in a playback recording
#[derive(Resource, Clone, Copy, Default)]
pub struct PlaybackState {
	/// Whether the playback is currently executing
	pub is_running: bool,
	/// Index of the next step in the [`PlaybackSequence`]
	/// (past-the-end if the last step is active)
	pub step_index: usize,
	/// Seconds since the last step was executed
	/// for waiting for [`PlaybackPrimitive::WaitSeconds`]
	pub seconds_elapsed: f32,
}

impl PlaybackState {
	pub fn stop(&mut self) {
		*self = Self::default();
	}

	pub fn toggle(&mut self) {
		let is_running = self.is_running;
		*self = Self::default();
		self.is_running = !is_running;
	}
}

/// [`SystemCondition`] that checks whether playback is currently running
pub fn is_playback_running(playback: Res<PlaybackState>) -> bool {
	playback.is_running
}
