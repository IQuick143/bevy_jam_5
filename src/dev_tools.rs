//! Development tools for the game. This plugin is only enabled in dev builds.

use crate::game::prelude::*;
use bevy::{color::palettes, dev_tools::states::log_transitions, utils::hashbrown::HashMap};

use crate::screen::Screen;

pub(super) fn plugin(app: &mut App) {
	// Print state transitions in dev builds
	app.add_systems(Update, log_transitions::<Screen>);
	app.add_systems(Update, simulate_vertices);
	app.add_systems(Update, (gizmo_draw, debug_inputs));
}

pub fn debug_inputs(
	input: Res<ButtonInput<MouseButton>>,
	window_q: Query<&Window>,
	cycles_q: Query<(Entity, &Transform, &ComputedCycleTurnability)>,
	camera_q: Query<(&Camera, &GlobalTransform)>,
	mut commands: Commands
) {
	let lmb = input.just_pressed(MouseButton::Left);
	let rmb = input.just_pressed(MouseButton::Right);
	let direction = match (lmb, rmb) {
		(true, true) => return,
		(true, false) => RotateCycle::Nominal,
		(false, true) => RotateCycle::Reverse,
		(false, false) => return,
	};
	let window = window_q.single();
	let (camera, camera_transform) = camera_q.single();
	if let Some(cursor_pos) = window.cursor_position()
		.and_then(|p| camera.viewport_to_world_2d(camera_transform, p)) {
		if let (Some(target_id), _) = cycles_q.iter()
			.filter(|(_, _, x)| x.0)
			.map(|(e, t, _)| (Some(e), t.translation.xy().distance_squared(cursor_pos)))
			.fold((None, f32::INFINITY), |a, b| if a.1 > b.1 { b } else { a }) {
			commands.trigger_targets(direction, target_id);
		}
	}
}

pub fn gizmo_draw(
	vertices: Query<&Transform, With<Vertex>>,
	circles: Query<(&CycleVertices, &Transform)>,
	objects: Query<&VertexPosition, With<Object>>,
	mut gizmos: Gizmos,
) {
	for transform in vertices.iter() {
		gizmos.sphere(
			transform.translation,
			Quat::IDENTITY,
			1.0,
			palettes::tailwind::BLUE_300,
		);
	}

	for vertex_id in objects.iter() {
		gizmos.rect(
			vertices.get(vertex_id.0).unwrap().translation,
			Quat::IDENTITY,
			Vec2::splat(5.0),
			palettes::tailwind::TEAL_300
		);
	}

	for (vertex_ids, circle_transform) in circles.iter() {
		gizmos.sphere(
			circle_transform.translation,
			Quat::IDENTITY,
			1.0,
			palettes::tailwind::AMBER_100,
		);
		let mut positions: Vec<Vec3> = vertex_ids
			.0
			.iter()
			.map(|entity| vertices.get(*entity).unwrap().translation)
			.collect();
		positions.push(positions[0]);
		let spline = CubicCardinalSpline::new(0.5, positions).to_curve();
		let samples = spline.iter_positions(32);
		gizmos.linestrip(samples, palettes::tailwind::AMBER_900)
	}
}

const TARGET_RADIUS: f32 = 150.0;

pub fn simulate_vertices(
	mut vertices: Query<&mut Transform, (With<Vertex>, Without<CycleVertices>)>,
	mut circles: Query<(Entity, &CycleVertices, &mut Transform)>,
	time: Res<Time<Real>>,
) {
	let mut gradients: HashMap<Entity, Vec2> = HashMap::new();

	let mut add_gradient = |e: Entity, grad: Vec2| {
		gradients.insert(e, gradients.get(&e).copied().unwrap_or_default() + grad);
	};

	fn spring_force_attractive(a: Vec3, b: Vec3, target_distance: f32) -> Vec3 {
		let (n, dist) = Dir3::new_and_length(a - b).unwrap(); // TODO: div by 0
		return n * (target_distance - dist).min(0.0);
	}

	fn spring_force_repulsive(a: Vec3, b: Vec3, target_distance: f32) -> Vec3 {
		let (n, dist) = Dir3::new_and_length(a - b).unwrap(); // TODO: div by 0
		return n * (target_distance - dist).max(0.0);
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
