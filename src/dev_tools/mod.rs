//! Development tools for the game. This plugin is only enabled in dev builds.

mod palette_picker;

use crate::{
	camera::CameraHarness,
	game::{animation::TurnAnimationLength, components::*, prelude::*},
	graphics::VERTICAL_PADDING_FRACTION,
	save::SaveGame,
	screen::PlayingLevel,
	ui::{
		hover::{HoverHintBoundingCircle, HoverHintBoundingRect},
		prelude::FadeAnimationBundle,
	},
};
use bevy::{
	color::palettes,
	dev_tools::{
		fps_overlay::{FpsOverlayConfig, FpsOverlayPlugin},
		states::log_transitions,
	},
	input::common_conditions::input_just_pressed,
	math::bounding::BoundingVolume,
	window::WindowMode,
};

use crate::screen::Screen;

pub(super) fn plugin(app: &mut App) {
	// Print state transitions in dev builds
	app.add_systems(
		Update,
		(
			log_transitions::<Screen>,
			log_transitions::<PlayingLevel>,
			automatic_reloading.run_if(in_state(Screen::Playing)),
			print_level_data.run_if(input_just_pressed(KeyCode::KeyY)),
			(debug_oneways, debug_camera_bounds, draw_hover_boxes)
				.run_if(resource_equals(RenderOutlines(true))),
			toggle_debug_outline_display.run_if(resource_changed::<RenderOutlines>),
			toggle_box_outlines.run_if(input_just_pressed(KeyCode::KeyB)),
			toggle_turning_animation_speed.run_if(input_just_pressed(KeyCode::KeyT)),
			(|mut s: ResMut<SaveGame>| *s = default()).run_if(input_just_pressed(KeyCode::Delete)),
			toggle_fps_diagnostic.run_if(input_just_pressed(KeyCode::KeyF)),
			toggle_ui_display.run_if(input_just_pressed(KeyCode::F1)),
			toggle_full_screen.run_if(input_just_pressed(KeyCode::F11)),
		),
	);
	app.add_systems(Startup, init_viewport_box);
	app.init_resource::<RenderOutlines>();
	app.init_resource::<AutoReload>();
	app.init_resource::<TurnAnimationSpeedState>();
	app.init_resource::<HideUi>();
	app.add_plugins(FpsOverlayPlugin {
		config: FpsOverlayConfig {
			text_color: Color::BLACK,
			text_config: TextFont {
				font_size: 12.0,
				..default()
			},
			..default()
		},
	})
	.add_plugins(palette_picker::plugin);
}

/// Whether hover and layout boxes should be drawn
#[derive(Resource, PartialEq, Eq, Debug, Default, Reflect)]
struct RenderOutlines(pub bool);

/// Whether the autoreloading system should run
#[derive(Resource, PartialEq, Eq, Debug, Default, Reflect)]
struct AutoReload(pub bool);

#[derive(Resource, Clone, Copy, PartialEq, Eq, Deref, DerefMut, Debug, Default, Reflect)]
struct TurnAnimationSpeedState(pub usize);

/// Whether the UI should hide itself so we can see the world
#[derive(Resource, PartialEq, Eq, Debug, Default, Deref, DerefMut)]
struct HideUi(pub bool);

/// Marks a [`Node`] as a debug outline, only making it visible
/// when [`RenderOutlines`] is set to true
#[derive(Component, PartialEq, Eq, PartialOrd, Ord, Debug, Default, Reflect)]
struct IsDebugOutline;

fn init_viewport_box(mut commands: Commands) {
	commands
		.spawn((
			Node {
				display: Display::None,
				width: Val::Percent(100.0),
				height: Val::Percent(100.0),
				flex_direction: FlexDirection::Column,
				justify_content: JustifyContent::Center,
				..default()
			},
			IsDebugOutline,
		))
		.with_child((
			Node {
				width: Val::Percent(100.0),
				height: Val::Percent(100.0 * (1.0 - VERTICAL_PADDING_FRACTION)),
				border: UiRect::all(Val::Px(1.0)),
				..default()
			},
			BorderColor::all(palettes::basic::NAVY),
		));
}

fn toggle_fps_diagnostic(mut config: ResMut<FpsOverlayConfig>) {
	config.enabled = !config.enabled;
	config.frame_time_graph_config.enabled = config.enabled;
}

fn toggle_debug_outline_display(
	mut query: Query<&mut Node, With<IsDebugOutline>>,
	render_outlines: Res<RenderOutlines>,
) {
	let display = if render_outlines.0 {
		Display::Flex
	} else {
		Display::None
	};
	for mut node in &mut query {
		node.display = display;
	}
}

fn toggle_box_outlines(mut render: ResMut<RenderOutlines>, mut ui_debug: ResMut<UiDebugOptions>) {
	render.0 = !render.0;
	ui_debug.toggle();
}

fn draw_hover_boxes(
	mut gizmos: Gizmos,
	rects: Query<(&HoverHintBoundingRect, &GlobalTransform)>,
	circles: Query<(&HoverHintBoundingCircle, &GlobalTransform)>,
) {
	for (bounding_box, transform) in &rects {
		let quat = transform.rotation();
		let rotation = Rot2 {
			cos: quat.w,
			sin: quat.z,
		}
		.try_normalize()
		.unwrap_or_default();
		let rotation = rotation * rotation;
		gizmos.rect_2d(
			Isometry2d::new(
				transform.translation().xy() + bounding_box.center(),
				rotation,
			),
			bounding_box.half_size() * 2.0,
			palettes::basic::LIME,
		);
	}
	for (bounding_circle, transform) in &circles {
		gizmos.circle_2d(
			transform.translation().xy() + bounding_circle.center,
			bounding_circle.radius(),
			palettes::basic::LIME,
		);
	}
}

fn debug_camera_bounds(camera: Single<&CameraHarness>, mut gizmos: Gizmos) {
	gizmos.rect(
		Vec3::ZERO,
		camera.level_bounds.half_size() * 2.0,
		palettes::basic::LIME,
	);
}

fn automatic_reloading(
	mut changed_events: MessageReader<AssetEvent<LevelData>>,
	asset_server: Res<AssetServer>,
	mut commands: Commands,
) {
	if !asset_server.watching_for_changes() {
		return;
	}
	let mut reload = false;
	for event in changed_events.read() {
		if let AssetEvent::Modified { id: _ } = event {
			reload = true
		}
	}
	if reload {
		commands.spawn((
			FadeAnimationBundle::from_time(0.05),
			crate::screen::LoadLevel,
		));
	}
}

fn debug_oneways(
	mut gizmos: Gizmos,
	cycle_transforms: Query<&Transform>,
	entity_index: Res<GameStateEcsIndex>,
	level: PlayingLevelData,
) {
	let Ok(level) = level.get() else {
		return;
	};
	for link in level.declared_one_way_links.iter() {
		let Ok(start) = cycle_transforms.get(entity_index.cycles[link.source]) else {
			continue;
		};
		let start = start.translation;
		let Ok(end) = cycle_transforms.get(entity_index.cycles[link.dest_cycle]) else {
			continue;
		};
		let end = end.translation;
		gizmos.arrow(start, end, bevy::color::palettes::basic::RED);
	}
}

fn print_level_data(level: PlayingLevelData) {
	let Ok(level) = level.get() else {
		return;
	};
	log::info!("{:?}", level);
}

fn toggle_turning_animation_speed(
	mut animation_time: ResMut<TurnAnimationLength>,
	mut current_setting: ResMut<TurnAnimationSpeedState>,
) {
	const OPTIONS: [f32; 6] = [TurnAnimationLength::DEFAULT.0, 1.0, 2.0, 3.0, 5.0, 10.0];
	**current_setting = (**current_setting + 1) % OPTIONS.len();
	**animation_time = OPTIONS[**current_setting];
}

fn toggle_ui_display(
	mut is_hidden: ResMut<HideUi>,
	mut query: Query<&mut Visibility, (With<Node>, Without<ChildOf>)>,
) {
	**is_hidden = !**is_hidden;
	let new_visibility = if **is_hidden {
		Visibility::Hidden
	} else {
		Visibility::Inherited
	};
	for mut visibility in &mut query {
		*visibility = new_visibility;
	}
}

fn toggle_full_screen(mut window: Single<&mut Window>) {
	if window.mode == WindowMode::Windowed {
		window.mode = WindowMode::BorderlessFullscreen(MonitorSelection::Current);
	} else {
		window.mode = WindowMode::Windowed;
	}
}
