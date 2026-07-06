//! UI needed to interact with the playback engine

use super::{
	asset::{PlaybackSequenceRepoHandle, PlaybackSequenceRepository},
	script::{PlaybackSequence, PlaybackState},
};
use crate::{
	AppSet,
	assets::GlobalFont,
	ui::{interaction::InteractionQuery, widgets},
};
use bevy::{input::common_conditions::input_just_pressed, prelude::*};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			toggle_playback.run_if(input_just_pressed(KeyCode::KeyP)),
			toggle_recording_selection_ui.run_if(input_just_pressed(KeyCode::KeyO)),
			recording_selection_inputs,
		)
			.in_set(AppSet::ExecuteInput),
	);
}

/// Marker component for the root of the UI for picking recordings
#[derive(Component, Clone, Copy, Default)]
struct RecordingSelectionUiRoot;

/// Component for buttons for selecting playback scenario
#[derive(Component, Clone, Copy)]
struct RecordingSelectionButton(usize);

fn toggle_recording_selection_ui(
	existing_q: Query<Entity, With<RecordingSelectionUiRoot>>,
	mut commands: Commands,
	recordings: Res<Assets<PlaybackSequenceRepository>>,
	recording_handle: Res<PlaybackSequenceRepoHandle>,
	font: Res<GlobalFont>,
) {
	// If there is already a widget, hide it
	if !existing_q.is_empty() {
		for id in &existing_q {
			commands.entity(id).despawn();
		}
		return;
	}

	let Some(recordings) = recordings.get(&**recording_handle) else {
		error!("No recordings available");
		return;
	};

	let root = commands
		.spawn((
			Node {
				width: Val::Percent(100.0),
				height: Val::Percent(100.0),
				flex_direction: FlexDirection::Column,
				align_items: AlignItems::End,
				justify_content: JustifyContent::Center,
				..default()
			},
			RecordingSelectionUiRoot,
		))
		.id();

	for i in 0..recordings.len() {
		commands.spawn((
			widgets::inline_button(i.to_string(), font.0.clone()),
			RecordingSelectionButton(i),
			ChildOf(root),
		));
	}
}

fn recording_selection_inputs(
	mut current_sequence: ResMut<PlaybackSequence>,
	mut playback_state: ResMut<PlaybackState>,
	recordings: Res<Assets<PlaybackSequenceRepository>>,
	recording_handle: Res<PlaybackSequenceRepoHandle>,
	query: InteractionQuery<&RecordingSelectionButton>,
) {
	for (interaction, enabled, RecordingSelectionButton(index)) in &query {
		if *interaction != Interaction::Pressed || enabled.is_some_and(|x| !**x) {
			continue;
		}

		let Some(recordings) = recordings.get(&**recording_handle) else {
			error!("No recordings available");
			return;
		};

		let Some(selected_sequence) = recordings.get(*index) else {
			error!("Recording with index {index} not found");
			return;
		};

		*current_sequence = selected_sequence.clone();
		*playback_state = PlaybackState::default();

		break;
	}
}

fn toggle_playback(mut state: ResMut<PlaybackState>) {
	state.toggle();
}
