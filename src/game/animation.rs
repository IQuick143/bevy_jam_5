use bevy::utils::hashbrown::HashMap;

use crate::game::prelude::*;

use super::logic;

pub fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			(
				(listen_for_moves, move_objects).chain(),
				goal_unlock_animation_system
					.run_if(resource_exists_and_changed::<LevelCompletionConditions>),
				(
					button_trigger_animation_system,
					cycle_center_turnability_visuals_update_system
						.before(cycle_center_interaction_visuals_update_system),
				)
					.run_if(on_event::<GameLayoutChanged>()),
				cycle_center_interaction_visuals_update_system,
				cycle_turning_animation_system.run_if(on_event::<RotateSingleCycle>()),
			)
				.after(logic::LogicSystemSet),
			spin_animation_system,
		),
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
			#[allow(clippy::unnecessary_unwrap)]
			// Clippy is annoying and it cannot be fixed cleanly
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
			transform.translation.x = end_position.x;
			transform.translation.y = end_position.y;
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
			transform.translation.x = goal.x;
			transform.translation.y = goal.y;
		}
	}
}

fn goal_unlock_animation_system(
	mut query: Query<&mut Sprite, With<Goal>>,
	palette: Res<ThingPalette>,
	completion: Res<LevelCompletionConditions>,
) {
	let color = if completion.is_goal_unlocked() {
		palette.goal_open
	} else {
		palette.goal_closed
	};
	for mut sprite in &mut query {
		sprite.color = color;
	}
}

fn button_trigger_animation_system(
	mut buttons_q: Query<&mut Sprite, With<BoxSlot>>,
	mut boxes_q: Query<
		&mut Sprite,
		(
			With<Box>,
			Without<BoxSlot>, /* To guarantee memory aliasing */
		),
	>,
	nodes_q: Query<(&PlacedGlyph, &PlacedObject)>,
	palette: Res<ThingPalette>,
) {
	for (glyph_id, object_id) in &nodes_q {
		let button = glyph_id.0.and_then(|id| buttons_q.get_mut(id).ok());
		let object = object_id.0.and_then(|id| boxes_q.get_mut(id).ok());
		// Use trigger color if both things are at the same place,
		// otherwise use base color
		match (button, object) {
			(Some(mut button), Some(mut object)) => {
				button.color = palette.button_trigger;
				object.color = palette.box_trigger;
			}
			(Some(mut button), None) => {
				button.color = palette.button_base;
			}
			(None, Some(mut object)) => {
				object.color = palette.box_base;
			}
			(None, None) => {}
		}
	}
}

fn cycle_center_turnability_visuals_update_system(
	cycles_q: Query<(&ComputedCycleTurnability, &Children)>,
	mut sprites_q: Query<(&mut SpinAnimation, &mut Sprite)>,
	palette: Res<ThingPalette>,
) {
	for (is_turnable, children) in &cycles_q {
		let Ok((mut animation, mut sprite)) = sprites_q.get_mut(children[0]) else {
			log::warn!("Child of cycle entity does not have SpinAnimation and Sprite components");
			continue;
		};
		if is_turnable.0 {
			animation.frequency = SpinAnimation::DEFAULT_FREQUENCY;
			sprite.color = palette.cycle_ready;
		} else {
			animation.frequency = 0.0;
			sprite.color = palette.cycle_disabled;
		}
	}
}

fn cycle_center_interaction_visuals_update_system(
	cycles_q: Query<
		(
			&CycleInteraction,
			&ComputedCycleTurnability,
			Option<&LinkedCycles>,
			&Children,
		),
		Changed<CycleInteraction>,
	>,
	all_cycles_q: Query<(&ComputedCycleTurnability, &Children)>,
	mut sprites_q: Query<&mut Sprite, With<SpinAnimation>>,
	palette: Res<ThingPalette>,
) {
	for (interaction, is_turnable, links, children) in &cycles_q {
		let target_sprites = std::iter::once((children[0], is_turnable.0)).chain(
			links
				.into_iter()
				.flat_map(|links| &links.0)
				.filter_map(|&(id, _)| {
					all_cycles_q
						.get(id)
						.inspect_err(|e| {
							log::warn!("LinkedCycles refers to a non-cycle entity {e}")
						})
						.ok()
						.map(|(turnable, children)| (children[0], turnable.0))
				}),
		);
		for (id, is_turnable) in target_sprites {
			let Ok(mut sprite) = sprites_q.get_mut(id) else {
				log::warn!(
					"Child of cycle entity does not have SpinAnimation and Sprite components"
				);
				continue;
			};
			sprite.color = if *interaction != CycleInteraction::None {
				palette.cycle_trigger
			} else if is_turnable {
				palette.cycle_ready
			} else {
				palette.cycle_disabled
			};
		}
	}
}

fn spin_animation_system(
	mut query: Query<(&mut SpinAnimation, &mut Transform)>,
	time: Res<Time<Real>>,
) {
	let delta_seconds = time.delta_seconds();
	for (mut animation, mut transform) in &mut query {
		animation.progress(delta_seconds);
		transform.rotation = Quat::from_axis_angle(Vec3::Z, animation.sample());
	}
}

const CYCLE_CENTER_ANIMATION_ANGLE: f32 = std::f32::consts::PI / 2.0;

fn cycle_turning_animation_system(
	cycles_q: Query<&Children, With<CycleVertices>>,
	mut spin_q: Query<&mut SpinAnimation>,
	mut events: EventReader<RotateSingleCycle>,
) {
	for event in events.read() {
		let Ok(children) = cycles_q.get(event.0.target_cycle) else {
			log::warn!("RotateSingleCycle event does not target a cycle entity");
			continue;
		};
		let Ok(mut animation) = spin_q.get_mut(children[0]) else {
			log::warn!("Child of cycle entity does not have SpinAnimation component");
			continue;
		};
		let direction_multiplier = match event.0.direction.into() {
			RotationDirection::Clockwise => -1.0,
			RotationDirection::CounterClockwise => 1.0,
		};
		animation.make_jump(
			direction_multiplier * CYCLE_CENTER_ANIMATION_ANGLE,
			ANIMATION_TIME,
		);
	}
}
