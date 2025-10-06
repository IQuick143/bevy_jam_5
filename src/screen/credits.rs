//! A credits screen that can be accessed from the title screen.

use bevy::prelude::*;

use super::*;
use crate::{
	assets::GlobalFont,
	ui::{consts::*, prelude::*},
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::Credits), enter_credits);
	app.add_systems(OnExit(Screen::Credits), exit_credits);

	app.add_systems(
		Update,
		handle_credits_action.run_if(in_state(Screen::Credits).and(ui_not_frozen)),
	);
	app.register_type::<CreditsAction>();
}

#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Reflect)]
#[reflect(Component)]
enum CreditsAction {
	Back,
}

const MAX_CENTERED_PANEL_WIDTH: f32 = 800.0;
const AUTHOR_NAME_FIELD_WIDTH: f32 = 290.0;
const ASSET_NAME_FIELD_WIDTH: f32 = 175.0;

fn enter_credits(mut commands: Commands, font: Res<GlobalFont>) {
	let mut table_node = Node {
		display: Display::Grid,
		width: Val::Percent(100.0),
		max_width: Val::Px(MAX_CENTERED_PANEL_WIDTH),
		column_gap: WIDE_GAP,
		row_gap: COMMON_GAP,
		grid_template_columns: vec![
			RepeatedGridTrack::px(1, AUTHOR_NAME_FIELD_WIDTH),
			RepeatedGridTrack::auto(1),
		],
		..default()
	};
	commands.spawn(
		(
			widgets::ui_root(),
			DespawnOnExit(Screen::Credits),
			children![
				widgets::header("Made by", font.0.clone()),
				(
					table_node.clone(),
					children![
						widgets::text("IQuick 143", JustifyContent::End, font.0.clone()),
						widgets::text("Game design, Programming, Visual direction, Level design", JustifyContent::Start, font.0.clone()),
						widgets::text("IWonderWhatThisAPIDoes", JustifyContent::End, font.0.clone()),
						widgets::text("Programming, Art, Level Design", JustifyContent::Start, font.0.clone()),
						widgets::text("SoysCodingCafe", JustifyContent::End, font.0.clone()),
						widgets::text("Level Design", JustifyContent::Start, font.0.clone()),
						widgets::text("spilledcereals", JustifyContent::End, font.0.clone()),
						widgets::text("Music, SFX", JustifyContent::Start, font.0.clone()),
					]
				),

				widgets::header("Assets", font.0.clone()),
				(
					{
						table_node.grid_template_columns[0] = RepeatedGridTrack::px(1, ASSET_NAME_FIELD_WIDTH);
						table_node
					},
					children![
						widgets::text("Bevy logo", JustifyContent::End, font.0.clone()),
						widgets::text("All rights reserved by the Bevy Foundation. Permission granted for splash screen use when unmodified.", JustifyContent::Start, font.0.clone()),
						widgets::text("Comfortaa font", JustifyContent::End, font.0.clone()),
						widgets::text("By Johan Aakerlund, licensed under Open Font License.", JustifyContent::Start, font.0.clone()),
					],
				),
				(widgets::menu_button("Back", font.0.clone()),CreditsAction::Back),
			]
		)
	);
}

fn exit_credits(mut _commands: Commands) {
	//commands.trigger(PlaySoundtrack::Disable);
}

fn handle_credits_action(
	mut commands: Commands,
	mut button_query: InteractionQuery<&CreditsAction>,
) {
	for (interaction, action) in &mut button_query {
		if matches!(interaction, Interaction::Pressed) {
			match action {
				CreditsAction::Back => {
					commands.do_screen_transition(Screen::Title);
				}
			}
		}
	}
}
