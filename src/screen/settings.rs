//! The Settings screen

use super::*;
use crate::{
	assets::{GlobalFont, SfxKey},
	audio::sfx::PlaySfx,
	settings::Settings,
	ui::{
		background::BackgroundMode,
		consts::*,
		multistate::{MultiStateButton, MultiStateButtonLabels},
		prelude::*,
		slider::Slider,
	},
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::Settings), enter_settings)
		.add_systems(
			Update,
			(
				handle_settings_action,
				handle_settings_slider_input,
				handle_settings_checkbox_input,
			)
				.run_if(in_state(Screen::Settings).and(ui_not_frozen)),
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

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Reflect)]
#[reflect(Component)]
enum SettingsCheckboxControl {
	Background,
	Parallax,
}

/// Width of a slider control, in pixels
const SLIDER_WIDTH: f32 = 200.0;
/// Number of steps on each slider on the Settings screen
const SLIDER_STEP_COUNT: u32 = 8;

fn enter_settings(mut commands: Commands, font: Res<GlobalFont>, settings: Res<Settings>) {
	commands.spawn((
		widgets::ui_root(),
		DespawnOnExit(Screen::Settings),
		children![
			widgets::header("Settings", font.0.clone()),
			(
				Node {
					display: Display::Grid,
					width: Val::Percent(100.0),
					column_gap: WIDE_GAP,
					row_gap: COMMON_GAP,
					padding: UiRect::vertical(COMMON_GAP),
					grid_template_columns: vec![RepeatedGridTrack::auto(2)],
					..default()
				},
				children![
					widgets::text("Music volume", JustifyContent::End, font.0.clone()),
					(
						Node {
							width: Val::Px(SLIDER_WIDTH),
							..default()
						},
						Slider::new_fraction(SLIDER_STEP_COUNT, settings.soundtrack_volume),
						SettingsSliderControl::MusicVolume,
					),
					widgets::text("Sfx volume", JustifyContent::End, font.0.clone()),
					(
						Node {
							width: Val::Px(SLIDER_WIDTH),
							..default()
						},
						Slider::new_fraction(SLIDER_STEP_COUNT, settings.sfx_volume),
						SettingsSliderControl::SfxVolume,
					),
					widgets::text("Background", JustifyContent::End, font.0.clone()),
					(
						Node::DEFAULT,
						children![(
							widgets::inline_button("", font.0.clone()),
							MultiStateButton::new(
								3,
								background_mode_to_option_index(settings.background_mode),
							),
							MultiStateButtonLabels::new(["Off", "Static", "Animated"]),
							SettingsCheckboxControl::Background,
						)],
					),
					widgets::text("Parallax", JustifyContent::End, font.0.clone()),
					(
						Node::DEFAULT,
						children![(
							widgets::inline_button("", font.0.clone()),
							MultiStateButton::new(2, settings.enable_parallax as u32),
							MultiStateButtonLabels::new(["Off", "On"]),
							SettingsCheckboxControl::Parallax,
						)],
					),
				],
			),
			(
				widgets::menu_button("Back", font.0.clone()),
				SettingsAction::Back,
			),
		],
	));
}

fn background_mode_to_option_index(mode: BackgroundMode) -> u32 {
	match mode {
		BackgroundMode::None => 0,
		BackgroundMode::Static => 1,
		BackgroundMode::Animated => 2,
	}
}

fn handle_settings_action(mut commands: Commands, query: InteractionQuery<&SettingsAction>) {
	for (interaction, action) in &query {
		if *interaction == Interaction::Pressed {
			match action {
				SettingsAction::Back => {
					commands.do_screen_transition(Screen::Title);
				}
			}
		}
	}
}

fn handle_settings_slider_input(
	query: Query<(&Slider, &SettingsSliderControl), Changed<Slider>>,
	mut settings: ResMut<Settings>,
	mut commands: Commands,
) {
	for (slider, control) in &query {
		match control {
			SettingsSliderControl::SfxVolume => {
				settings.sfx_volume = slider.fraction();
				// Play a sound to immediately sample the volume
				commands.trigger(PlaySfx::Effect(SfxKey::ButtonPress));
			}
			SettingsSliderControl::MusicVolume => settings.soundtrack_volume = slider.fraction(),
		}
	}
}

fn handle_settings_checkbox_input(
	query: Query<(&MultiStateButton, &SettingsCheckboxControl), Changed<MultiStateButton>>,
	mut settings: ResMut<Settings>,
) {
	for (MultiStateButton { current_state, .. }, control) in &query {
		match control {
			SettingsCheckboxControl::Background => {
				settings.background_mode = match current_state {
					0 => BackgroundMode::None,
					1 => BackgroundMode::Static,
					2.. => BackgroundMode::Animated,
				};
			}
			SettingsCheckboxControl::Parallax => {
				settings.enable_parallax = *current_state != 0;
			}
		}
	}
}
