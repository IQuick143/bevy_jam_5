//! Extensions to title screen for playtesting builds

use crate::{
	assets::{GlobalFont, HandleMap, ImageKey},
	drawing::{ColorKey, NodeColorKey},
	screen::{DoScreenTransitionCommands as _, Screen},
	ui::{prelude::*, widgets},
	AppSet,
};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::Title), spawn_playtest_title_ui)
		.add_systems(
			Update,
			enter_playtest_screen
				.run_if(ui_not_frozen)
				.in_set(AppSet::RecordInput),
		);
}

/// Marker component for the playtest panel button on title screen
#[derive(Clone, Copy, Debug, Default, Component)]
struct TitlePlaytestPanelButton;

fn spawn_playtest_title_ui(
	mut commands: Commands,
	font: Res<GlobalFont>,
	image_handles: Res<HandleMap<ImageKey>>,
) {
	commands
		.spawn((
			widgets::ui_root_justified(JustifyContent::End),
			DespawnOnExit(Screen::Title),
		))
		.with_children(|children| {
			children
				.spawn((
					widgets::grid_button("Playtester panel", font.0.clone()),
					TitlePlaytestPanelButton,
				))
				.with_child((
					Node {
						position_type: PositionType::Absolute,
						left: Val::Px(0.0),
						top: Val::Px(2.5),
						width: Val::Px(40.0),
						height: Val::Px(40.0),
						..default()
					},
					ImageNode {
						image: image_handles[&ImageKey::PlaytestMarker].clone(),
						..default()
					},
					NodeColorKey(ColorKey::PlaytestMarker),
				));
		});
}

fn enter_playtest_screen(
	query: InteractionQuery<(), With<TitlePlaytestPanelButton>>,
	mut commands: Commands,
) {
	for (interaction, enabled, ()) in &query {
		if *interaction == Interaction::Pressed && enabled.is_none_or(|e| **e) {
			commands.do_screen_transition(Screen::Playtest);
		}
	}
}
