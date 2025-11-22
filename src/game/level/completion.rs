//! Precomputed level and hub completion information

use super::list::LevelList;
use crate::{game::level::list::LevelOrHubId, save::SaveGame};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub enum CompletionStatus {
	/// Level hub has not been completed
	#[default]
	Started,
	/// Completion threshold has been reached
	Completed,
	/// All levels under the hub have been completed
	Cleared,
}

#[derive(Clone, Debug, Default)]
pub struct LevelHubCompletion {
	levels_completed: Vec<bool>,
	hub_completion: Vec<LevelHubCompletionState>,
}

#[derive(Clone, Copy, Debug, Default)]
struct LevelHubCompletionState {
	children_completed: usize,
	children_cleared: usize,
	completion_status: CompletionStatus,
}

impl LevelHubCompletion {
	pub fn new(level_list: &LevelList) -> Self {
		Self {
			levels_completed: vec![false; level_list.levels.len()],
			hub_completion: vec![LevelHubCompletionState::default(); level_list.hubs.len()],
		}
	}

	pub fn from_save(level_list: &LevelList, save: &SaveGame) -> Self {
		let mut completion = Self::new(level_list);
		for (level_id, level) in level_list.levels.iter().enumerate() {
			if save.is_level_completed(&level.identifier) {
				completion.set_level_completed(level_list, level_id);
			}
		}
		completion
	}

	pub fn set_level_completed(&mut self, level_list: &LevelList, level_id: usize) {
		self.levels_completed[level_id] = true;
		let parent_hub = level_list.levels[level_id].parent_hub;
		self.add_completed_child_to_hub(level_list, parent_hub, true, true);
	}

	fn add_completed_child_to_hub(
		&mut self,
		level_list: &LevelList,
		mut hub_id: usize,
		mut add_as_completed: bool,
		mut add_as_cleared: bool,
	) {
		loop {
			let hub = &level_list.hubs[hub_id];
			let hub_completion = &mut self.hub_completion[hub_id];
			let children_completed = &mut hub_completion.children_completed;
			let children_cleared = &mut hub_completion.children_cleared;

			if add_as_completed {
				*children_completed += 1;
			}
			if add_as_cleared {
				*children_cleared += 1;
			}
			let children_to_clear = hub.child_hubs.len() + hub.levels.len();
			let has_misconfigured_completion_condition =
				hub.children_to_complete > children_to_clear;
			let just_cleared = add_as_cleared && *children_cleared == children_to_clear;
			let just_completed = if has_misconfigured_completion_condition {
				// In the degenerate case where hub needs more children completed
				// than how many there actually are,
				// cap completion threshold at clear, and mark it at the same time
				just_cleared
			} else {
				add_as_completed && *children_completed == hub.children_to_complete
			};

			if just_cleared {
				hub_completion.completion_status = CompletionStatus::Cleared;
			} else if just_completed {
				hub_completion.completion_status = CompletionStatus::Completed;
			}

			if just_completed || just_cleared {
				if let Some(parent_id) = hub.parent_hub {
					// Recursive call for parent goes here:
					// self.add_completed_child_to_hub(level_list, parent_id, just_completed, just_cleared);
					//
					// Buuut I'm a little anxious about the compiler neglecting
					// to do a tail recursion optimization, so...
					hub_id = parent_id;
					add_as_completed = just_completed;
					add_as_cleared = just_cleared;
					continue;
				}
			}

			break;
		}
	}

	pub fn hub_completion_status(&self, hub_id: usize) -> CompletionStatus {
		self.hub_completion[hub_id].completion_status
	}

	pub fn is_level_unlocked(&self, level_list: &LevelList, level_id: usize) -> bool {
		self.is_prerequisite_satisfied(&level_list.levels[level_id].prerequisites)
	}

	pub fn is_hub_unlocked(&self, level_list: &LevelList, hub_id: usize) -> bool {
		self.is_prerequisite_satisfied(&level_list.hubs[hub_id].prerequisites)
	}

	fn is_prerequisite_satisfied(&self, prerequisites: &[LevelOrHubId]) -> bool {
		prerequisites.iter().all(|prereq| match prereq {
			LevelOrHubId::Level(level_id) => self.levels_completed[*level_id],
			LevelOrHubId::Hub(hub_id) => {
				self.hub_completion_status(*hub_id) >= CompletionStatus::Completed
			}
		})
	}
}
