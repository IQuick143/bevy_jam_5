use super::prelude::*;

pub fn plugin(app: &mut App) {
	app
		.observe(cycle_rotation_observer)
		.add_systems(Update, (
			level_completion_check_system,
			cycle_turnability_update_system
		).run_if(on_event::<GameLayoutChanged>()));
}

fn cycle_rotation_observer(
	event: Trigger<RotateCycle>,
	cycles_q: Query<&CycleVertices>,
	mut vertices_q: Query<&mut PlacedObject>,
	mut objects_q: Query<&mut VertexPosition>
) {
	if let Ok(cycle_vertices) = cycles_q.get(event.entity()).inspect_err(|e| log::warn!("{e}")) {
		// Visit the first vertex again at the end to close the loop
		let vertex_ids_wrapped = cycle_vertices.0.iter()
			.copied()
			.chain(std::iter::once(cycle_vertices.0[0]));
		// Messy but simple way of chosing the iterator direction at run time
		// https://stackoverflow.com/a/52064434
		let vertex_ids_wrapped = match event.event() {
			RotateCycle::Nominal => Some(vertex_ids_wrapped).into_iter().flatten().chain(None.into_iter().flatten()),
			RotateCycle::Reverse => None.into_iter().flatten().chain(Some(vertex_ids_wrapped.rev()).into_iter().flatten())
		};
		let mut cached_object_id = None;
		for vertex_id in vertex_ids_wrapped {
			if let Some(cached_object_id) = cached_object_id {
				if let Ok(mut object_pos) = objects_q.get_mut(cached_object_id).inspect_err(|e| log::warn!("{e}")) {
					object_pos.0 = vertex_id;
				}
			}
			if let Ok(mut placed_object_id) = vertices_q.get_mut(vertex_id).inspect_err(|e| log::warn!("{e}")) {
				std::mem::swap(&mut cached_object_id, &mut placed_object_id.0);
			}
		}
	}
}

fn cycle_turnability_update_system(
	vertices_q: Query<&PlacedObject>,
	players_q: Query<(), With<Player>>,
	mut cycles_q: Query<(&CycleVertices, &CycleTurnability, &mut ComputedCycleTurnability)>
) {
	'next_cycle: for (vertex_ids, turnability, mut computed_turnability) in &mut cycles_q {
		if *turnability == CycleTurnability::Always {
			computed_turnability.0 = true;
			continue;
		}

		for &vertex_id in &vertex_ids.0 {
			let player_is_present = vertices_q.get(vertex_id)
				.inspect_err(|e| log::warn!("{e}"))
				.ok()
				.and_then(|object_id| object_id.0
					.and_then(|id| players_q.get(id).ok()))
				.is_some();
			if player_is_present {
				computed_turnability.0 = true;
				continue 'next_cycle;
			}
		}

		computed_turnability.0 = false;
	}
}

fn level_completion_check_system(
	vertices_q: Query<(&PlacedObject, &PlacedGlyph)>,
	objects_q: Query<(Option<&Player>, Option<&Box>), With<Object>>,
	glyphs_q: Query<(Option<&Goal>, Option<&BoxSlot>), With<Glyph>>,
	mut completion: ResMut<LevelCompletionConditions>
) {
	let mut new_completion = LevelCompletionConditions {
		buttons_present: 0,
		buttons_triggered: 0,
		players_present: 0,
		players_flagged: 0
	};

	for (object, glyph) in &vertices_q {
		let (contains_player, contains_box) = object.0
			.and_then(|id| objects_q.get(id)
				.inspect_err(|e| log::warn!("{e}"))
				.ok())
			.map(|(a, b)| (a.is_some(), b.is_some()))
			.unwrap_or((false, false));
		let (contains_goal, contains_button) = glyph.0
			.and_then(|id| glyphs_q.get(id)
				.inspect_err(|e| log::warn!("{e}"))
				.ok())
			.map(|(a, b)| (a.is_some(), b.is_some()))
			.unwrap_or((false, false));

		if contains_button {
			new_completion.buttons_present += 1;
			if contains_box {
				new_completion.buttons_triggered += 1;
			}
		}
		if contains_player {
			new_completion.players_present += 1;
			if contains_goal {
				new_completion.players_flagged += 1;
			}
		}
	}

	if new_completion.is_level_completed() {
		// TODO: End the level
		eprintln!("Completed!");
	}
	else if new_completion.is_goal_unlocked() {
		// TODO: Possibly add a visual indicator that the goal is open
		eprintln!("Goal open!");
	}

	*completion = new_completion;
}
