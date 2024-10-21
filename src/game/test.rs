use bevy::prelude::*;

use super::spawn::{LevelInitialization, LevelInitializationSet};

fn setup_app() -> App {
	let mut app = App::new();
	app.add_plugins((
		super::logic::plugin,
		super::history::plugin,
		super::spawn::plugin,
	))
	// Disable spawning of visual entities
	.configure_sets(
		LevelInitialization,
		LevelInitializationSet::SpawnVisuals.run_if(|| false),
	);
	app
}

/// Metatest for asserting that running the headless game works.
#[test]
fn test_app() {
	let mut app = setup_app();
	for _ in 0..5 {
		app.update();
	}
}
