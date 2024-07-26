//! Development tools for the game. This plugin is only enabled in dev builds.

use crate::{
	game::{
		graphics::{GAME_AREA, LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH},
		logic,
		prelude::*,
	},
	screen::QueueScreenTransition,
};
use bevy::{color::palettes, dev_tools::states::log_transitions, utils::hashbrown::HashMap};

use crate::screen::Screen;

pub(super) fn plugin(app: &mut App) {
	// Print state transitions in dev builds
	app.add_systems(Update, log_transitions::<Screen>);
	app.add_systems(
		Update,
		(
			switch_level,
			//gizmo_draw,
			draw_layout,
			debug_inputs.before(logic::LogicSystemSet),
		),
	);
	app.add_systems(Startup, spawn_text);
}

fn draw_layout(mut gizmos: Gizmos) {
	gizmos.rect(Vec3::ZERO, Quat::IDENTITY, GAME_AREA, palettes::basic::RED);
	gizmos.rect(
		LEVEL_AREA_CENTER.extend(0.0),
		Quat::IDENTITY,
		LEVEL_AREA_WIDTH,
		palettes::basic::NAVY,
	);
}

fn switch_level(
	keyboard: Res<ButtonInput<KeyCode>>,
	mut next_screen: EventWriter<QueueScreenTransition>,
) {
	if keyboard.just_pressed(KeyCode::Digit1) {
		next_screen.send(QueueScreenTransition::fade(Screen::Level(
			crate::game::LevelID::Cycle,
		)));
	}
	if keyboard.just_pressed(KeyCode::Digit2) {
		next_screen.send(QueueScreenTransition::fade(Screen::Level(
			crate::game::LevelID::Bicycle,
		)));
	}
	if keyboard.just_pressed(KeyCode::Digit3) {
		next_screen.send(QueueScreenTransition::fade(Screen::Level(
			crate::game::LevelID::Tricycle,
		)));
	}
	if keyboard.just_pressed(KeyCode::Digit4) {
		next_screen.send(QueueScreenTransition::fade(Screen::Level(
			crate::game::LevelID::CargoTricycle,
		)));
	}
	if keyboard.just_pressed(KeyCode::Digit5) {
		next_screen.send(QueueScreenTransition::fade(Screen::Level(
			crate::game::LevelID::SquareCycle,
		)));
	}
	if keyboard.just_pressed(KeyCode::Digit6) {
		next_screen.send(QueueScreenTransition::fade(Screen::Level(
			crate::game::LevelID::DiamondCycle,
		)));
	}
	if keyboard.just_pressed(KeyCode::Digit7) {
		next_screen.send(QueueScreenTransition::fade(Screen::Level(
			crate::game::LevelID::Lotus,
		)));
	}
	if keyboard.just_pressed(KeyCode::Digit8) {
		next_screen.send(QueueScreenTransition::fade(Screen::Level(
			crate::game::LevelID::ThreeInARow,
		)));
	}
	if keyboard.just_pressed(KeyCode::Digit9) {
		next_screen.send(QueueScreenTransition::fade(Screen::Level(
			crate::game::LevelID::TripleRing,
		)));
	}
	if keyboard.just_pressed(KeyCode::Digit0) {
		next_screen.send(QueueScreenTransition::fade(Screen::Level(
			crate::game::LevelID::Car,
		)));
	}
	if keyboard.just_pressed(KeyCode::KeyQ) {
		next_screen.send(QueueScreenTransition::fade(Screen::Level(
			crate::game::LevelID::Olympic,
		)));
	}
	if keyboard.just_pressed(KeyCode::KeyW) {
		next_screen.send(QueueScreenTransition::fade(Screen::Level(
			crate::game::LevelID::Pedalo,
		)));
	}
	if keyboard.just_pressed(KeyCode::KeyE) {
		next_screen.send(QueueScreenTransition::fade(Screen::Level(
			crate::game::LevelID::Pyramid,
		)));
	}
}

fn spawn_text(mut commands: Commands, asset_server: Res<AssetServer>) {
	let text_box_z = -100.0;
	let text_box_loc = Vec2::new(0.0, -337.5);
	let text_box_size = Vec2::new(1600.0, 225.0);
	let text_box_color = Color::srgba(0.3, 0.3, 0.3, 0.8);
	let margin = 10.0;
	let text = "Click to rotate the wheels clockwise! Right click rotates them anti-clockwise! Get the boxes on the buttons and the player to the flag!";
	commands
		.spawn((SpriteBundle {
			transform: Transform::from_xyz(text_box_loc.x, text_box_loc.y, text_box_z),
			sprite: Sprite {
				color: text_box_color,
				custom_size: Some(text_box_size),
				..default()
			},
			..default()
		},))
		.with_children(|parent| {
			parent.spawn((Text2dBundle {
				text_2d_bounds: bevy::text::Text2dBounds {
					size: Vec2::new(
						text_box_size.x - margin * 2.0,
						text_box_size.y - margin * 2.0,
					),
				},
				transform: Transform::from_xyz(
					-text_box_size.x / 2.0 + margin,
					text_box_size.y / 2.0 - margin,
					0.1, // Relative to text box
				),
				text_anchor: bevy::sprite::Anchor::TopLeft,
				text: Text::from_section(text, get_text_style(&asset_server))
					.with_justify(JustifyText::Left),
				..default()
			},));
		});
}

fn get_text_style(_asset_server: &Res<AssetServer>) -> TextStyle {
	TextStyle {
		//font: asset_server.load("fonts/your_font_here.ttf"),
		font_size: 32.0,
		color: Color::srgba(0.9, 0.9, 0.9, 1.0),
		..default()
	}
}

pub fn debug_inputs(
	input: Res<ButtonInput<MouseButton>>,
	window_q: Query<&Window>,
	cycles_q: Query<(Entity, &Transform, &ComputedCycleTurnability)>,
	camera_q: Query<(&Camera, &GlobalTransform)>,
	mut rotate_cycle_events: EventWriter<RotateCycleGroup>,
) {
	let lmb = input.just_pressed(MouseButton::Left);
	let rmb = input.just_pressed(MouseButton::Right);
	let direction = match (lmb, rmb) {
		(true, true) => return,
		(true, false) => CycleTurningDirection::Nominal,
		(false, true) => CycleTurningDirection::Reverse,
		(false, false) => return,
	};
	let window = window_q.single();
	let (camera, camera_transform) = camera_q.single();
	if let Some(cursor_pos) = window
		.cursor_position()
		.and_then(|p| camera.viewport_to_world_2d(camera_transform, p))
	{
		if let (Some(target_id), _) = cycles_q
			.iter()
			.filter(|(_, _, x)| x.0)
			.map(|(e, t, _)| (Some(e), t.translation.xy().distance_squared(cursor_pos)))
			.fold((None, f32::INFINITY), |a, b| if a.1 > b.1 { b } else { a })
		{
			rotate_cycle_events.send(RotateCycleGroup(RotateCycle {
				target_cycle: target_id,
				direction,
			}));
		}
	}
}

pub fn _gizmo_draw(
	vertices: Query<&Transform, With<Vertex>>,
	circles: Query<(
		&Transform,
		&CycleVertices,
		&ComputedCycleTurnability,
		&CycleTurnability,
		Option<&LinkedCycles>,
	)>,
	players: Query<&Transform, With<Player>>,
	boxes: Query<&Transform, With<Box>>,
	buttons: Query<&Transform, With<BoxSlot>>,
	flags: Query<&Transform, With<Goal>>,
	mut gizmos: Gizmos,
) {
	// Draw vertices
	for transform in vertices.iter() {
		gizmos.sphere(
			transform.translation,
			Quat::IDENTITY,
			1.0,
			palettes::tailwind::BLUE_300,
		);
	}

	// Draw boxes
	for transform in boxes.iter() {
		gizmos.rect(
			transform.translation,
			Quat::IDENTITY,
			Vec2::splat(10.0),
			palettes::css::MAROON,
		);
	}

	// Draw players
	for transform in players.iter() {
		gizmos.rect(
			transform.translation,
			Quat::IDENTITY,
			Vec2::splat(10.0),
			palettes::css::TEAL,
		);
	}

	// Draw buttons
	for transform in buttons.iter() {
		gizmos.rounded_rect(
			transform.translation,
			Quat::IDENTITY,
			Vec2::splat(20.0),
			palettes::css::MAROON,
		);
	}

	// Draw flags
	for transform in flags.iter() {
		gizmos.rounded_rect(
			transform.translation,
			Quat::IDENTITY,
			Vec2::splat(20.0),
			palettes::css::TEAL,
		);
	}

	// Draw cycles & links
	for (circle_transform, vertex_ids, current_turnability, turnability, links) in circles.iter() {
		// Draw cycle centers
		gizmos.sphere(
			circle_transform.translation,
			Quat::IDENTITY,
			10.0,
			match (turnability, current_turnability.0) {
				(CycleTurnability::Always, true) => palettes::tailwind::GREEN_600,
				(CycleTurnability::Always, false) => {
					warn!("Always cycle has no turning somehow");
					palettes::tailwind::RED_600
				}
				(CycleTurnability::WithPlayer, true) => palettes::tailwind::AMBER_100,
				(CycleTurnability::WithPlayer, false) => palettes::tailwind::AMBER_600,
			},
		);
		// Draw the loops
		let mut positions: Vec<Vec3> = vertex_ids
			.0
			.iter()
			.map(|entity| vertices.get(*entity).unwrap().translation)
			.collect();
		positions.push(positions[0]);
		let spline = CubicCardinalSpline::new(0.5, positions).to_curve();
		let samples = spline.iter_positions(32);
		gizmos.linestrip(samples, palettes::tailwind::AMBER_900);

		// Draw links
		if let Some(links) = links {
			for (linked_entity, direction) in links.0.iter() {
				if let Ok((other_transform, _, _, _, _)) = circles.get(*linked_entity) {
					gizmos.line(
						circle_transform.translation,
						other_transform.translation,
						match direction {
							LinkedCycleDirection::Coincident => palettes::tailwind::GRAY_100,
							LinkedCycleDirection::Inverse => palettes::tailwind::GRAY_500,
						},
					);
				}
			}
		}
	}
}

pub fn _simulate_vertices(
	mut vertices: Query<&mut Transform, (With<Vertex>, Without<CycleVertices>)>,
	mut circles: Query<(Entity, &CycleVertices, &mut Transform)>,
	time: Res<Time<Real>>,
) {
	const TARGET_RADIUS: f32 = 150.0;

	let mut gradients: HashMap<Entity, Vec2> = HashMap::new();

	let mut add_gradient = |e: Entity, grad: Vec2| {
		gradients.insert(e, gradients.get(&e).copied().unwrap_or_default() + grad);
	};

	fn spring_force_attractive(a: Vec3, b: Vec3, target_distance: f32) -> Vec3 {
		let (n, dist) = Dir3::new_and_length(a - b).unwrap(); // TODO: div by 0
		n * (target_distance - dist).min(0.0)
	}

	fn spring_force_repulsive(a: Vec3, b: Vec3, target_distance: f32) -> Vec3 {
		let (n, dist) = Dir3::new_and_length(a - b).unwrap(); // TODO: div by 0
		n * (target_distance - dist).max(0.0)
	}

	for (circle, vertex_ids, circle_transform) in circles.iter() {
		let mut r = 0.0;
		let mut n = 0;
		for vertex_id in vertex_ids.0.iter() {
			r += circle_transform
				.translation
				.distance(vertices.get(*vertex_id).unwrap().translation);
			n += 1;
		}
		let radius = r / n as f32;

		let positions = vertex_ids
			.0
			.iter()
			.map(|vertex_id| vertices.get(*vertex_id).unwrap().translation)
			.collect::<Vec<Vec3>>();

		let average_position = positions.iter().sum::<Vec3>() / positions.len() as f32;
		let central_offset = average_position - circle_transform.translation;

		add_gradient(
			circle,
			(central_offset * 0.3 - circle_transform.translation * 0.10).xy(),
		);

		for (i, vertex_id) in vertex_ids.0.iter().enumerate() {
			let prev_pos = if i == 0 {
				*positions.last().unwrap()
			} else {
				positions[i - 1]
			};
			let position = positions[i];
			let next_pos = if i + 1 == positions.len() {
				positions[0]
			} else {
				positions[i + 1]
			};
			let offset = position - circle_transform.translation;
			let distance = offset.length();
			let norm = offset / distance; // TODO: Div by 0
			let circle_force = -(distance - radius) * 2.9 - (radius - TARGET_RADIUS) * 1.5;

			let neighboor_forces = spring_force_attractive(
				position,
				prev_pos,
				TARGET_RADIUS / vertex_ids.0.len() as f32,
			) + spring_force_attractive(
				position,
				next_pos,
				TARGET_RADIUS / vertex_ids.0.len() as f32,
			);
			let offset_force = -central_offset * 5.2;

			add_gradient(
				*vertex_id,
				(circle_force * norm + (neighboor_forces + offset_force).reject_from(norm)).xy(),
			);
		}
	}

	for [(circle_a, _, circle_transform_a), (circle_b, _, circle_transform_b)] in
		circles.iter_combinations()
	{
		let force = spring_force_repulsive(
			circle_transform_a.translation,
			circle_transform_b.translation,
			2.0 * TARGET_RADIUS,
		);
		let force = force.xy();
		add_gradient(circle_a, force);
		add_gradient(circle_b, -force);
	}

	for (entity, gradient) in gradients.iter() {
		if let Ok(mut transform) = vertices.get_mut(*entity) {
			transform.translation += (*gradient * time.delta_seconds() * 5.0).extend(0.0);
		}
		if let Ok((_, _, mut transform)) = circles.get_mut(*entity) {
			transform.translation += (*gradient * time.delta_seconds()).extend(0.0);
		}
	}
}
