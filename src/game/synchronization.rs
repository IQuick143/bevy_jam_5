use super::{logic::IsLevelCompleted, *};
use crate::{game::spawn::LevelInitialization, save::SaveGame, screen::PlayingLevelListEntry};

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<IsLevelPersistentlyCompleted>()
		.add_systems(
			Update,
			progress_persistence_sync_system.run_if(
				resource_changed::<IsLevelCompleted>.and(resource_equals(IsLevelCompleted(true))),
			),
		)
		.add_systems(LevelInitialization, load_persistent_completion_indicator);
}

/// Indicates whether the level being played has ever been completed,
/// including in a previous run of the game (read from the save file)
#[derive(Resource, Clone, Copy, PartialEq, Eq, Debug, Default, Deref, DerefMut)]
pub struct IsLevelPersistentlyCompleted(pub bool);

fn load_persistent_completion_indicator(
	mut is_completed: ResMut<IsLevelPersistentlyCompleted>,
	save: Res<SaveGame>,
	playing_level: PlayingLevelListEntry,
) {
	let level_id = match playing_level.get() {
		Ok(level_info) => &level_info.identifier,
		Err(e) => {
			warn!("{e}");
			return;
		}
	};
	**is_completed = save.is_level_completed(level_id);
}

fn progress_persistence_sync_system(
	mut save: ResMut<SaveGame>,
	mut is_completed: ResMut<IsLevelPersistentlyCompleted>,
	playing_level: PlayingLevelListEntry,
) {
	let level_id = match playing_level.get() {
		Ok(level_info) => &level_info.identifier,
		Err(e) => {
			warn!("{e}");
			return;
		}
	};
	save.set_level_completion(level_id, true);
	**is_completed = true;
}
