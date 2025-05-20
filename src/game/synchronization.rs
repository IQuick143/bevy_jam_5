use super::{logic::IsLevelCompleted, *};
use crate::{save::SaveGame, screen::PlayingLevelListEntry};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		progress_persistence_sync_system.run_if(
			resource_changed::<IsLevelCompleted>.and(resource_equals(IsLevelCompleted(true))),
		),
	);
}

fn progress_persistence_sync_system(
	mut save: ResMut<SaveGame>,
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
}
