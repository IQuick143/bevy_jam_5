//! The title screen that appears when the game starts.

use bevy::prelude::*;

use super::*;
use crate::{
	assets::{self, GlobalFont, HandleMap, ImageKey},
	graphics,
	ui::prelude::*,
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::Title), enter_title);

	app.register_type::<TitleAction>();
	app.add_systems(
		Update,
		handle_title_action.run_if(in_state(Screen::Title).and_then(ui_not_frozen)),
	);
}

#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Reflect)]
#[reflect(Component)]
enum TitleAction {
	Play,
	Credits,
	/// Exit doesn't work well with embedded applications.
	#[cfg(not(target_family = "wasm"))]
	Exit,
}

fn enter_title(
	mut commands: Commands,
	font: Res<GlobalFont>,
	image_handles: Res<HandleMap<ImageKey>>,
) {
	commands
		.ui_root()
		.insert(StateScoped(Screen::Title))
		.with_children(|children| {
			// Invisible spacer node to bring the menu lower
			children.spawn(NodeBundle {
				style: Style {
					height: Val::Px(200.0),
					..default()
				},
				..default()
			});
			children
				.button("Play", font.0.clone_weak())
				.insert(TitleAction::Play);
			children
				.button("Credits", font.0.clone_weak())
				.insert(TitleAction::Credits);

			#[cfg(not(target_family = "wasm"))]
			children
				.button("Exit", font.0.clone_weak())
				.insert(TitleAction::Exit);
		});
	commands.spawn((
		StateScoped(Screen::Title),
		SpriteBundle {
			sprite: Sprite {
				custom_size: Some(graphics::GAME_AREA * assets::TITLE_IMAGE_OVERFLOW),
				..default()
			},
			texture: image_handles[&ImageKey::Title].clone_weak(),
			transform: Transform::from_translation(
				Vec2::ZERO.extend(graphics::layers::TITLE_IMAGE),
			),
			..default()
		},
	));
}

fn handle_title_action(
	mut commands: Commands,
	mut button_query: InteractionQuery<&TitleAction>,
	#[cfg(not(target_family = "wasm"))] mut app_exit: EventWriter<AppExit>,
) {
	for (interaction, action) in &mut button_query {
		if matches!(interaction, Interaction::Pressed) {
			match action {
				TitleAction::Play => {
					commands.spawn((
						FadeAnimationBundle::default(),
						DoScreenTransition(Screen::LevelSelect),
					));
				}
				TitleAction::Credits => {
					commands.spawn((
						FadeAnimationBundle::default(),
						DoScreenTransition(Screen::Credits),
					));
				}
				#[cfg(not(target_family = "wasm"))]
				TitleAction::Exit => {
					app_exit.send(AppExit::Success);
				}
			}
		}
	}
}
