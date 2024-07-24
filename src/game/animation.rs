use bevy::utils::hashbrown::HashMap;

use crate::game::prelude::*;

use super::logic;

pub fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(listen_for_moves.after(logic::LogicSystemSet), move_objects).chain(),
	);
}

const ANIMATION_TIME: f32 = 0.5;

fn listen_for_moves(
	mut rotation_events: EventReader<RotateSingleCycle>,
	cycles: Query<(&CycleVertices, &GlobalTransform)>,
	vertices: Query<&GlobalTransform, With<Vertex>>,
	mut objects: Query<
		(&mut Transform, &VertexPosition, Option<&mut AnimatedObject>),
		With<Object>,
	>,
) {
	// Maps current vertices to previous vertices and the center of rotation as well as direction
	let mut permutation_map = HashMap::new();
	for event in rotation_events.read() {
		let Ok((vertices, transform)) = cycles.get(event.0.target_cycle) else {
			// TODO: warn
			continue;
		};
		let location = transform.translation();
		let mut vertex_loop = logic::get_flipped_wrapped_iterator(&vertices.0, event.0.direction);
		let Some(mut last_id) = vertex_loop.next() else {
			continue;
		};
		for vertex in vertex_loop {
			permutation_map.insert(vertex, (last_id, location, event.0.direction));
			last_id = vertex;
		}
	}

	for (mut transform, vertex_position, animation) in objects.iter_mut() {
		let end_vertex = vertex_position.0;

		let Ok(end_vertex_transform) = vertices.get(end_vertex) else {
			// TODO: warn
			continue;
		};

		let end_position = end_vertex_transform.translation();

		if let Some(mut animation) = animation {
			let Some(&(start_vertex, center_of_rotation, direction)) =
				permutation_map.get(&end_vertex)
			else {
				// No movement, we can skip
				continue;
			};

			let Ok(start_vertex_transform) = vertices.get(start_vertex) else {
				// TODO: warn
				continue;
			};

			// If the animation hasn't finished playing, we give it a chance to catch up.
			let sample = animation.sample();
			#[allow(clippy::unnecessary_unwrap)] // Clippy is annoying and it cannot be fixed cleanly
			let start_vector = if sample.is_some() && animation.progress < 1.0 {
				sample.unwrap()
			} else {
				start_vertex_transform.translation()
			};

			animation.rotation_direction = direction.into();

			animation.progress = 0.0;
			animation.cycle_center = center_of_rotation;
			let (start_dir, start_magnitude) =
				Dir2::new_and_length((start_vector - center_of_rotation).xy())
					.unwrap_or((Dir2::X, 0.0));
			let (end_dir, end_magnitude) = Dir2::new_and_length(
				(end_vertex_transform.translation() - center_of_rotation).xy(),
			)
			.unwrap_or((Dir2::X, 0.0));
			animation.start_direction = Some(start_dir);
			animation.start_magnitude = start_magnitude;
			animation.final_direction = Some(end_dir);
			animation.final_magnitude = end_magnitude;
		} else {
			// Object is not animated, so we just set the translation to the desired place.
			transform.translation = end_position;
		}
	}
}

fn move_objects(mut objects: Query<(&mut Transform, &mut AnimatedObject)>, time: Res<Time<Real>>) {
	for (mut transform, mut animation) in objects.iter_mut() {
		if animation.progress >= 1.0 {
			continue;
		}
		animation.progress += (time.delta_seconds() / ANIMATION_TIME).min(1.0);
		if let Some(goal) = animation.sample() {
			transform.translation = goal;
		}
	}
}
