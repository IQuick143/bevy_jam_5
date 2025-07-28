//! The Settings screen

use super::*;
use crate::{
	assets::GlobalFont,
	ui::{prelude::*, slider::Slider},
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::Settings), enter_settings)
		.add_systems(
			Update,
			handle_settings_action.run_if(in_state(Screen::Settings).and(ui_not_frozen)),
		);
}

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Reflect)]
#[reflect(Component)]
enum SettingsAction {
	Back,
}

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Reflect)]
#[reflect(Component)]
enum SettingsSliderControl {
	SfxVolume,
	MusicVolume,
}

/// Width of a slider control, in pixels
const SLIDER_WIDTH: f32 = 200.0;
/// Number of steps on each slider on the Settings screen
const SLIDER_STEP_COUNT: u32 = 8;

fn enter_settings(mut commands: Commands, font: Res<GlobalFont>) {
	commands
		.ui_root()
		.insert(StateScoped(Screen::Settings))
		.with_children(|children| {
			children.header("Settings", font.0.clone_weak());
			children
				.spawn(Node {
					display: Display::Grid,
					width: Val::Percent(100.0),
					column_gap: Val::Px(20.0),
					row_gap: Val::Px(10.0),
					padding: UiRect::vertical(Val::Px(10.0)),
					grid_template_columns: vec![RepeatedGridTrack::auto(2)],
					..default()
				})
				.with_children(|children| {
					children.text("Music volume", JustifyContent::End, font.0.clone_weak());
					children.spawn((
						Node {
							width: Val::Px(SLIDER_WIDTH),
							..default()
						},
						Slider::new(SLIDER_STEP_COUNT, 0),
						SettingsSliderControl::MusicVolume,
					));
					children.text("Sfx volume", JustifyContent::End, font.0.clone_weak());
					children.spawn((
						Node {
							width: Val::Px(SLIDER_WIDTH),
							..default()
						},
						Slider::new(SLIDER_STEP_COUNT, 0),
						SettingsSliderControl::SfxVolume,
					));
				});
			children
				.button("Back", font.0.clone_weak())
				.insert(SettingsAction::Back);
		});
}

fn handle_settings_action(mut commands: Commands, query: InteractionQuery<&SettingsAction>) {
	for (interaction, action) in &query {
		if *interaction == Interaction::Pressed {
			match action {
				SettingsAction::Back => {
					commands.spawn((
						FadeAnimationBundle::default(),
						DoScreenTransition(Screen::Title),
					));
				}
			}
		}
	}
}
