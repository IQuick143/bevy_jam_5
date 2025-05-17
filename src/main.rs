// Disable console on Windows for non-dev builds.
#![cfg_attr(not(feature = "dev"), windows_subsystem = "windows")]

use bevy::ecs::error::{warn, GLOBAL_ERROR_HANDLER};
use bevy::prelude::*;
use bevy_cycle_permutation_puzzle::AppPlugin;

fn main() -> AppExit {
	// Configure the ECS error handler to not explode, hopefully.
	GLOBAL_ERROR_HANDLER
		.set(warn)
		.expect("The error handler can only be set once, globally.");

	App::new().add_plugins(AppPlugin).run()
}
