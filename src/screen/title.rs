//! The title screen that appears when the game starts.

use super::*;
use crate::{
	assets::{self, GlobalFont, HandleMap, ImageKey},
	camera::parallax::Parallax,
	graphics,
	ui::prelude::*,
};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(OnEnter(Screen::Title), enter_title);
	app.add_systems(
		Update,
		handle_title_action.run_if(in_state(Screen::Title).and(ui_not_frozen)),
	);
	// Create the material later, when all of its dependencies have been initialized
	app.add_systems(Startup, |mut commands: Commands| {
		commands.init_resource::<AnimatedBackgroundMaterial>()
	});
}

#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Reflect)]
#[reflect(Component)]
enum TitleAction {
	GoToScreen(Screen),
	/// Exit doesn't work well with embedded applications.
	#[cfg(not(target_family = "wasm"))]
	Exit,
}

/// Width of the background mesh, in multiples of nominal viewport width
const ANIMATED_BACKGROUND_X_OVERFLOW: f32 = 3.0;

/// Size of the background mesh, in world units
const ANIMATED_BACKGROUND_MESH_SIZE: Vec2 = Vec2::new(
	graphics::GAME_AREA.x * ANIMATED_BACKGROUND_X_OVERFLOW,
	graphics::GAME_AREA.x * 489.0 / 2383.0,
);

/// Speed of the background animation, in viewport widths per second
const ANIMATED_BACKGROUND_SPEED: Vec2 = Vec2::new(-0.01, 0.0);

/// Parallax factor of the background mesh
const ANIMATED_BACKGROUND_PARALLAX: f32 = graphics::BACKGROUND_PARALLAX / 2.0;

#[derive(Resource, Clone, Deref, DerefMut, Debug, Reflect)]
struct AnimatedBackgroundMaterial(Handle<graphics::ScrollingTextureMaterial>);

impl FromWorld for AnimatedBackgroundMaterial {
	fn from_world(world: &mut World) -> Self {
		let images = world.resource::<HandleMap<ImageKey>>();
		let material = graphics::ScrollingTextureMaterial {
			scale: Vec2::new(ANIMATED_BACKGROUND_X_OVERFLOW, 1.0),
			speed: ANIMATED_BACKGROUND_SPEED,
			texture: images[&ImageKey::TitleBack].clone(),
		};
		let mut materials = world.resource_mut::<Assets<graphics::ScrollingTextureMaterial>>();
		Self(materials.add(material))
	}
}

fn enter_title(
	mut commands: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	font: Res<GlobalFont>,
	background_material: Res<AnimatedBackgroundMaterial>,
	image_handles: Res<HandleMap<ImageKey>>,
) {
	commands.spawn((
		widgets::ui_root(),
		DespawnOnExit(Screen::Title),
		children![
			// Invisible spacer node to bring the menu lower
			Node {
				height: Val::Px(200.0),
				..default()
			},
			(
				widgets::menu_button("Play", font.0.clone()),
				TitleAction::GoToScreen(Screen::LevelSelect)
			),
			(
				widgets::menu_button("Settings", font.0.clone()),
				TitleAction::GoToScreen(Screen::Settings)
			),
			(
				widgets::menu_button("Credits", font.0.clone()),
				TitleAction::GoToScreen(Screen::Credits)
			),
			#[cfg(not(target_family = "wasm"))]
			(
				widgets::menu_button("Exit", font.0.clone()),
				TitleAction::Exit
			),
		],
	));
	commands.spawn((
		DespawnOnExit(Screen::Title),
		Sprite {
			custom_size: Some(graphics::GAME_AREA * assets::TITLE_IMAGE_OVERFLOW),
			image: image_handles[&ImageKey::Title].clone(),
			..default()
		},
		Transform::from_translation(Vec3::Z * graphics::layers::TITLE_IMAGE),
	));
	commands.spawn((
		DespawnOnExit(Screen::Title),
		Mesh2d(meshes.add(Rectangle::from_size(ANIMATED_BACKGROUND_MESH_SIZE))),
		MeshMaterial2d(background_material.0.clone()),
		Transform::from_translation(Vec3::Z * graphics::layers::TITLE_BACKDROP),
		Parallax(ANIMATED_BACKGROUND_PARALLAX),
	));
}

fn handle_title_action(
	mut commands: Commands,
	button_query: InteractionQuery<&TitleAction>,
	#[cfg(not(target_family = "wasm"))] mut app_exit: MessageWriter<AppExit>,
) {
	for (interaction, enabled, action) in &button_query {
		if enabled.is_none_or(|e| **e) && matches!(interaction, Interaction::Pressed) {
			match action {
				TitleAction::GoToScreen(screen) => {
					commands.do_screen_transition(*screen);
				}
				#[cfg(not(target_family = "wasm"))]
				TitleAction::Exit => {
					app_exit.write(AppExit::Success);
				}
			}
		}
	}
}
