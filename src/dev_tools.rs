//! Development tools for the game. This plugin is only enabled in dev builds.

use crate::{
	game::{components::*, level::CycleTurnability, logic::*, prelude::*},
	graphics::{GAME_AREA, LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH},
	save::SaveGame,
	screen::PlayingLevel,
	ui::{hover::Hoverable, prelude::FadeAnimationBundle},
};
use bevy::{
	color::palettes, dev_tools::states::log_transitions,
	input::common_conditions::input_just_pressed, math::bounding::BoundingVolume,
	platform::collections::HashMap,
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
			debug_oneways.run_if(resource_equals(RenderOutlines(true))),
			draw_layout.run_if(resource_equals(RenderOutlines(true))),
			draw_hover_boxes.run_if(resource_equals(RenderOutlines(true))),
			toggle_box_outlines.run_if(input_just_pressed(KeyCode::KeyB)),
			(|mut s: ResMut<SaveGame>| *s = default()).run_if(input_just_pressed(KeyCode::Delete)),
		),
	);
	app.init_resource::<RenderOutlines>();
}

/// Whether hover and layout boxes should be drawn
#[derive(Resource, PartialEq, Eq, Debug, Default, Reflect)]
struct RenderOutlines(pub bool);

fn toggle_box_outlines(mut render: ResMut<RenderOutlines>) {
	render.0 = !render.0;
}

fn draw_hover_boxes(mut gizmos: Gizmos, hoverables: Query<(&Hoverable, &GlobalTransform)>) {
	for (hover, transform) in hoverables.iter() {
		if let Some(bounding_box) = hover.hover_bounding_box {
			gizmos.rect_2d(
				transform.translation().xy() + bounding_box.center(),
				bounding_box.half_size() * 2.0,
				palettes::basic::LIME,
			);
		}
		if let Some(bounding_circle) = hover.hover_bounding_circle {
			gizmos.circle_2d(
				transform.translation().xy() + bounding_circle.center,
				bounding_circle.radius(),
				palettes::basic::LIME,
			);
		}
	}
}

fn draw_layout(mut gizmos: Gizmos) {
	gizmos.rect(Vec3::ZERO, GAME_AREA, palettes::basic::RED);
	gizmos.rect(
		LEVEL_AREA_CENTER.extend(0.0),
		LEVEL_AREA_WIDTH,
		palettes::basic::NAVY,
	);
}

fn automatic_reloading(
	mut changed_events: EventReader<AssetEvent<LevelData>>,
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
	cycles_q: Query<&Transform>,
	cycle_index: Res<CycleEntities>,
	level_asset: Res<Assets<LevelData>>,
	level_handle: Res<LevelHandle>,
) {
	let Some(level) = level_asset.get(&level_handle.0) else {
		return;
	};
	for link in level.declared_one_way_links.iter() {
		let Ok(start) = cycles_q.get(cycle_index.0[link.source]) else {
			return;
		};
		let start = start.translation;
		let Ok(end) = cycles_q.get(cycle_index.0[link.dest_cycle]) else {
			return;
		};
		let end = end.translation;
		gizmos.arrow(start, end, bevy::color::palettes::basic::RED);
	}
}

fn print_level_data(level_asset: Res<Assets<LevelData>>, level_handle: Res<LevelHandle>) {
	let Some(level) = level_asset.get(&level_handle.0) else {
		return;
	};
	log::info!("{:?}", level);
}

pub fn _debug_inputs(
	input: Res<ButtonInput<MouseButton>>,
	window: Single<&Window>,
	camera: Single<(&Camera, &GlobalTransform)>,
	cycles_q: Query<(Entity, &Transform, &ComputedCycleTurnability)>,
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
	let (camera, camera_transform) = *camera;
	if let Some(cursor_pos) = window
		.cursor_position()
		.and_then(|p| camera.viewport_to_world_2d(camera_transform, p).ok())
	{
		if let (Some(target_id), _) = cycles_q
			.iter()
			.filter(|(_, _, x)| x.0)
			.map(|(e, t, _)| (Some(e), t.translation.xy().distance_squared(cursor_pos)))
			.fold((None, f32::INFINITY), |a, b| if a.1 > b.1 { b } else { a })
		{
			rotate_cycle_events.write(RotateCycleGroup(RotateCycle {
				target_cycle: target_id,
				direction,
				amount: 1,
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
	)>,
	players: Query<&Transform, With<Player>>,
	boxes: Query<&Transform, With<SokoBox>>,
	buttons: Query<&Transform, With<SokoButton>>,
	flags: Query<&Transform, With<Goal>>,
	mut gizmos: Gizmos,
) {
	// Draw vertices
	for transform in vertices.iter() {
		gizmos.sphere(transform.translation, 1.0, palettes::tailwind::BLUE_300);
	}

	// Draw boxes
	for transform in boxes.iter() {
		gizmos.rect(
			transform.translation,
			Vec2::splat(10.0),
			palettes::css::MAROON,
		);
	}

	// Draw players
	for transform in players.iter() {
		gizmos.rect(
			transform.translation,
			Vec2::splat(10.0),
			palettes::css::TEAL,
		);
	}

	// Draw buttons
	for transform in buttons.iter() {
		gizmos.rounded_rect(
			transform.translation,
			Vec2::splat(20.0),
			palettes::css::MAROON,
		);
	}

	// Draw flags
	for transform in flags.iter() {
		gizmos.rounded_rect(
			transform.translation,
			Vec2::splat(20.0),
			palettes::css::TEAL,
		);
	}

	// Draw cycles & links
	for (circle_transform, vertex_ids, current_turnability, turnability) in circles.iter() {
		// Draw cycle centers
		gizmos.sphere(
			circle_transform.translation,
			10.0,
			match (turnability, current_turnability.0) {
				(CycleTurnability::Always, true) => palettes::tailwind::GREEN_600,
				(CycleTurnability::Always, false) => {
					log::warn!("Always cycle has no turning somehow");
					palettes::tailwind::RED_600
				}
				(CycleTurnability::WithPlayer, true) => palettes::tailwind::AMBER_100,
				(CycleTurnability::WithPlayer, false) => palettes::tailwind::AMBER_600,
				(CycleTurnability::Never, false) => palettes::tailwind::RED_600,
				(CycleTurnability::Never, true) => {
					log::warn!("Never cycle has turning somehow");
					palettes::tailwind::GREEN_600
				}
			},
		);
		// Draw the loops
		let mut positions: Vec<Vec3> = vertex_ids
			.0
			.iter()
			.map(|entity| vertices.get(*entity).unwrap().translation)
			.collect();
		positions.push(positions[0]);
		let spline = CubicCardinalSpline::new(0.5, positions).to_curve().unwrap();
		let samples = spline.iter_positions(32);
		gizmos.linestrip(samples, palettes::tailwind::AMBER_900);

		//	// Draw links
		//	if let Some(links) = links {
		//		for (linked_entity, direction) in links.0.iter() {
		//			if let Ok((other_transform, _, _, _, _)) = circles.get(*linked_entity) {
		//				gizmos.line(
		//					circle_transform.translation,
		//					other_transform.translation,
		//					match direction {
		//						LinkedCycleDirection::Coincident => palettes::tailwind::GRAY_100,
		//						LinkedCycleDirection::Inverse => palettes::tailwind::GRAY_500,
		//					},
		//				);
		//			}
		//		}
		//	}
	}
}

pub fn _simulate_vertices(
	mut vertices: Query<&mut Transform, (With<Vertex>, Without<CycleVertices>)>,
	mut circles: Query<(Entity, &CycleVertices, &mut Transform)>,
	time: Res<Time<Real>>,
) {
	const TARGET_RADIUS: f32 = 150.0;

	let mut gradients: HashMap<Entity, Vec2> = HashMap::default();

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
			transform.translation += (*gradient * time.delta_secs() * 5.0).extend(0.0);
		}
		if let Ok((_, _, mut transform)) = circles.get_mut(*entity) {
			transform.translation += (*gradient * time.delta_secs()).extend(0.0);
		}
	}
}
