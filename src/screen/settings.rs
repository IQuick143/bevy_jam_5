//! The Settings screen

use super::*;
use crate::{
	assets::{GlobalFont, SfxKey},
	audio::sfx::PlaySfx,
	settings::Settings,
	ui::{
		checkbox::{Checkbox, CheckboxLabels},
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
	AnimateBackground,
	Parallax,
}

/// Width of a slider control, in pixels
const SLIDER_WIDTH: f32 = 200.0;
/// Number of steps on each slider on the Settings screen
const SLIDER_STEP_COUNT: u32 = 8;

fn enter_settings(mut commands: Commands, font: Res<GlobalFont>, settings: Res<Settings>) {
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
						Slider::new_fraction(SLIDER_STEP_COUNT, settings.soundtrack_volume),
						SettingsSliderControl::MusicVolume,
					));
					children.text("Sfx volume", JustifyContent::End, font.0.clone_weak());
					children.spawn((
						Node {
							width: Val::Px(SLIDER_WIDTH),
							..default()
						},
						Slider::new_fraction(SLIDER_STEP_COUNT, settings.sfx_volume),
						SettingsSliderControl::SfxVolume,
					));
					children.text("Background", JustifyContent::End, font.0.clone_weak());
					children.spawn(Node::DEFAULT).with_children(|children| {
						children.tool_button("", font.0.clone_weak()).insert((
							Checkbox(settings.render_background),
							CheckboxLabels::new("On", "Off"),
							SettingsCheckboxControl::Background,
						));
					});
					children.text(
						"Animate background",
						JustifyContent::End,
						font.0.clone_weak(),
					);
					children.spawn(Node::DEFAULT).with_children(|children| {
						children.tool_button("", font.0.clone_weak()).insert((
							Checkbox(settings.animate_background),
							CheckboxLabels::new("On", "Off"),
							SettingsCheckboxControl::AnimateBackground,
						));
					});
					children.text("Parallax", JustifyContent::End, font.0.clone_weak());
					children.spawn(Node::DEFAULT).with_children(|children| {
						children.tool_button("", font.0.clone_weak()).insert((
							Checkbox(settings.enable_parallax),
							CheckboxLabels::new("On", "Off"),
							SettingsCheckboxControl::Parallax,
						));
					});
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
	query: Query<(&Checkbox, &SettingsCheckboxControl), Changed<Checkbox>>,
	mut settings: ResMut<Settings>,
) {
	for (is_checked, control) in &query {
		match control {
			SettingsCheckboxControl::Background => {
				settings.render_background = **is_checked;
			}
			SettingsCheckboxControl::AnimateBackground => {
				settings.animate_background = **is_checked;
			}
			SettingsCheckboxControl::Parallax => {
				settings.enable_parallax = **is_checked;
			}
		}
	}
}
