//! Spawn the main level by triggering other observers.

use crate::{
	assets::{HandleMap, ImageKey},
	game::{level::*, prelude::*},
	graphics::*,
	screen::Screen,
	ui::hover::{self, HintText, Hoverable},
};

use bevy::math::{
	bounding::{Aabb2d, BoundingCircle},
	primitives,
};
use bevy::sprite::Anchor::Custom;
use rand::Rng;

pub(super) fn plugin(app: &mut App) {
	app.observe(spawn_level);
}

fn spawn_level(
	trigger: Trigger<SpawnLevel>,
	mut events: EventWriter<GameLayoutChanged>,
	mut commands: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	levels: Res<Assets<LevelData>>,
	mut is_level_completed: ResMut<IsLevelCompleted>,
	mut move_history: ResMut<MoveHistory>,
	cycle_material: ResMut<RingMaterial>,
	link_material: Res<LinkMaterial>,
	palette: ResMut<ThingPalette>,
	image_handles: Res<HandleMap<ImageKey>>,
	mut hint_text: ResMut<HintText>,
) {
	println!("Spawning!"); //TODO: debug
	let SpawnLevel(level) = trigger.event();
	let level = levels
		.get(level)
		.expect("Got an invalid handle to a level asset");

	let vertices: Vec<Entity> = level
		.vertices
		.iter()
		.map(|data| {
			spawn_vertex(
				commands.reborrow(),
				data,
				meshes.reborrow(),
				cycle_material.0.clone(),
				palette.as_ref(),
				image_handles.as_ref(),
			)
		})
		.collect();

	let cycle_ids = level
		.cycles
		.iter()
		.map(|data| {
			spawn_cycle(
				commands.reborrow(),
				meshes.reborrow(),
				cycle_material.0.clone(),
				&palette,
				&image_handles,
				data,
				&vertices,
			)
		})
		.collect::<Vec<_>>();

	for (i, cycle_id) in cycle_ids.iter().copied().enumerate() {
		let linked_cycles = level.cycles[i]
			.link_closure
			.iter()
			.map(|&(j, dir)| (cycle_ids[j], dir))
			.collect::<Vec<_>>();
		if !linked_cycles.is_empty() {
			commands
				.entity(cycle_id)
				.insert(LinkedCycles(linked_cycles));
		}
	}

	for link in &level.declared_links {
		let a = level.cycles[link.source_cycle].placement.position;
		let b = level.cycles[link.dest_cycle].placement.position;
		let d_sq = a.distance_squared(b);
		if d_sq <= CYCLE_LINK_SPACING.powi(2) {
			// The link cannot be rendered if the cycles are too close
			log::warn!("Skipped drawing a cycle link because the cycles are {} units apart, need at least {CYCLE_LINK_SPACING}", d_sq.sqrt());
			continue;
		}
		let connector_length = match link.direction {
			LinkedCycleDirection::Coincident => a.distance(b),
			LinkedCycleDirection::Inverse => (d_sq - CYCLE_LINK_SPACING.powi(2)).sqrt(),
		};
		let mesh = primitives::Rectangle::from_size(Vec2::new(
			connector_length - CYCLE_LINK_END_CUT,
			CYCLE_LINK_WIDTH,
		))
		.mesh();
		let mesh = meshes.add(mesh);
		let dir_from_a_to_b = (a - b).normalize();
		let rotation = Quat::from_rotation_arc_2d(Vec2::X, dir_from_a_to_b);
		let position = a.lerp(b, 0.5);
		let offset = match link.direction {
			LinkedCycleDirection::Coincident => dir_from_a_to_b.perp() * CYCLE_LINK_SPACING / 2.0,
			LinkedCycleDirection::Inverse => Vec2::ZERO,
		};
		let extra_rotation = match link.direction {
			LinkedCycleDirection::Coincident => Quat::IDENTITY,
			LinkedCycleDirection::Inverse => Quat::from_rotation_arc_2d(
				Vec2::X,
				Vec2::new(d_sq.sqrt(), CYCLE_LINK_SPACING).normalize(),
			),
		};
		commands.spawn((
			StateScoped(Screen::Playing),
			LevelScoped,
			ColorMesh2dBundle {
				mesh: bevy::sprite::Mesh2dHandle(mesh.clone_weak()),
				transform: Transform::from_rotation(rotation.mul_quat(extra_rotation))
					.with_translation((position + offset).extend(layers::CYCLE_LINKS)),
				material: link_material.clone_weak(),
				..default()
			},
		));
		commands.spawn((
			StateScoped(Screen::Playing),
			LevelScoped,
			ColorMesh2dBundle {
				mesh: bevy::sprite::Mesh2dHandle(mesh),
				transform: Transform::from_rotation(rotation.mul_quat(extra_rotation.inverse()))
					.with_translation((position - offset).extend(layers::CYCLE_LINKS)),
				material: link_material.clone_weak(),
				..default()
			},
		));
	}

	is_level_completed.0 = false;
	move_history.clear();
	events.send(GameLayoutChanged);

	hint_text.hint_text.clone_from(&level.hint);
}

fn spawn_vertex(
	mut commands: Commands,
	data: &VertexData,
	mut meshes: Mut<Assets<Mesh>>,
	base_material: Handle<ColorMaterial>,
	palette: &ThingPalette,
	image_handles: &HandleMap<ImageKey>,
) -> Entity {
	let transform =
		TransformBundle::from_transform(Transform::from_translation(data.position.extend(0.0)));
	let vertex_id = commands
		.spawn((
			Vertex,
			StateScoped(Screen::Playing),
			LevelScoped,
			PlacedGlyph(None),
			PlacedObject(None),
			transform,
		))
		.id();
	let mesh = primitives::Circle::new(NODE_RADIUS).mesh();
	commands.spawn((
		StateScoped(Screen::Playing),
		LevelScoped,
		ColorMesh2dBundle {
			transform: Transform::from_translation(data.position.extend(layers::CYCLE_NODES)),
			mesh: bevy::sprite::Mesh2dHandle(meshes.add(mesh)),
			material: base_material,
			..default()
		},
	));
	if let Some(object_data) = data.object {
		let object_type = object_data.object_type;
		let thing_type = ThingType::Object(object_type);
		let mut entity = match object_type {
			ObjectType::Player => commands.spawn((
				StateScoped(Screen::Playing),
				LevelScoped,
				Object,
				Player,
				VertexPosition(vertex_id),
				ObjectKind(thing_type),
				SpriteBundle {
					sprite: Sprite {
						color: palette.player,
						custom_size: Some(SPRITE_SIZE),
						anchor: Custom(Vec2::new(0.0, -0.25)),
						..default()
					},
					texture: image_handles[&ImageKey::Object(thing_type)].clone_weak(),
					transform: Transform::from_translation(
						data.position.extend(layers::OBJECT_SPRITES),
					),
					..Default::default()
				},
				AnimatedObject::default(),
				Hoverable {
					hover_text: hover::PLAYER,
					hover_bounding_circle: None,
					hover_bounding_box: Some(Aabb2d::new(
						SPRITE_LENGTH * Vec2::new(0.0, 0.25),
						SPRITE_LENGTH * Vec2::new(0.25, 0.4),
					)),
				},
			)),
			ObjectType::Box => commands.spawn((
				StateScoped(Screen::Playing),
				LevelScoped,
				Object,
				Box,
				VertexPosition(vertex_id),
				ObjectKind(thing_type),
				SpriteBundle {
					sprite: Sprite {
						color: object_data
							.color
							.map(|c| palette.colored_base[c.0])
							.unwrap_or(palette.box_base),
						custom_size: Some(SPRITE_SIZE),
						anchor: Custom(Vec2::new(0.0, -0.25)),
						..default()
					},
					texture: image_handles[&ImageKey::Object(thing_type)].clone_weak(),
					transform: Transform::from_translation(
						data.position.extend(layers::OBJECT_SPRITES),
					),
					..Default::default()
				},
				AnimatedObject::default(),
				Hoverable {
					hover_text: hover::BOX,
					hover_bounding_circle: None,
					hover_bounding_box: Some(Aabb2d::new(
						SPRITE_LENGTH * Vec2::new(0.0, 0.125),
						SPRITE_LENGTH * Vec2::new(0.25, 0.25),
					)),
				},
			)),
		};
		if let Some(color) = object_data.color {
			entity.insert(color);
		}
		let object_id = entity.id();
		commands
			.entity(vertex_id)
			.insert(PlacedObject(Some(object_id)));
	}
	if let Some(glyph_data) = data.glyph {
		let glyph_type = glyph_data.glyph_type;
		let thing_type = ThingType::Glyph(glyph_type);
		let mut entity = match glyph_type {
			GlyphType::Button => commands.spawn((
				StateScoped(Screen::Playing),
				LevelScoped,
				Glyph,
				BoxSlot,
				VertexPosition(vertex_id),
				ObjectKind(thing_type),
				SpriteBundle {
					sprite: Sprite {
						color: glyph_data
							.color
							.map(|c| palette.colored_base[c.0])
							.unwrap_or(palette.button_base),
						custom_size: Some(SPRITE_SIZE),
						anchor: Custom(Vec2::new(0.0, -0.25)),
						..default()
					},
					texture: image_handles[&ImageKey::Object(thing_type)].clone_weak(),
					transform: Transform::from_translation(
						data.position.extend(layers::GLYPH_SPRITES),
					),
					..Default::default()
				},
				Hoverable {
					hover_text: hover::BUTTON,
					hover_bounding_circle: None,
					hover_bounding_box: Some(Aabb2d::new(
						SPRITE_LENGTH * Vec2::new(0.0, -0.125),
						SPRITE_LENGTH * Vec2::new(0.375, 0.125),
					)),
				},
			)),
			GlyphType::Flag => commands.spawn((
				StateScoped(Screen::Playing),
				LevelScoped,
				Glyph,
				Goal,
				VertexPosition(vertex_id),
				ObjectKind(thing_type),
				SpriteBundle {
					sprite: Sprite {
						color: palette.goal_closed,
						custom_size: Some(SPRITE_SIZE),
						anchor: Custom(Vec2::new(0.0, -0.25)),
						..default()
					},
					texture: image_handles[&ImageKey::Object(thing_type)].clone_weak(),
					transform: Transform::from_translation(
						data.position.extend(layers::GLYPH_SPRITES),
					),
					..Default::default()
				},
				Hoverable {
					hover_text: hover::FLAG,
					hover_bounding_circle: None,
					hover_bounding_box: Some(Aabb2d::new(
						SPRITE_LENGTH * Vec2::new(0.0, 0.125),
						SPRITE_LENGTH * Vec2::new(0.25, 0.30),
					)),
				},
			)),
		};
		if let Some(color) = glyph_data.color {
			entity.insert(color);
		}
		let glyph_id = entity.id();
		commands
			.entity(vertex_id)
			.insert(PlacedGlyph(Some(glyph_id)));
	}

	vertex_id
}

fn spawn_cycle(
	mut commands: Commands,
	mut meshes: Mut<Assets<Mesh>>,
	material: Handle<ColorMaterial>,
	palette: &ThingPalette,
	image_handles: &HandleMap<ImageKey>,
	data: &CycleData,
	vertex_entities: &[Entity],
) -> Entity {
	let mesh = primitives::Annulus::new(
		data.placement.radius - RING_HALF_WIDTH,
		data.placement.radius + RING_HALF_WIDTH,
	)
	.mesh()
	.resolution(64)
	.build();

	commands
		.spawn((
			data.turnability,
			StateScoped(Screen::Playing),
			LevelScoped,
			ComputedCycleTurnability(true),
			CycleVertices(
				data.vertex_indices
					.iter()
					.map(|i| *vertex_entities.get(*i).unwrap())
					.collect(),
			),
			CycleInterationRadius(data.placement.radius),
			CycleInteraction::default(),
			TransformBundle::from_transform(Transform::from_translation(
				data.placement.position.extend(0.0),
			)),
			VisibilityBundle::default(),
		))
		.with_children(|parent| {
			parent.spawn((
				SpriteBundle {
					sprite: Sprite {
						custom_size: Some(SPRITE_SIZE),
						color: palette.cycle_ready,
						..default()
					},
					texture: image_handles[&ImageKey::CycleCenter(data.turnability)].clone_weak(),
					transform: Transform::from_translation(
						Vec2::ZERO.extend(layers::CYCLE_CENTER_SPRITES),
					),
					..default()
				},
				JumpTurnAnimation::default(),
				Hoverable {
					hover_text: match data.turnability {
						CycleTurnability::Always => hover::CYCLE_AUTOMATIC,
						CycleTurnability::WithPlayer => hover::CYCLE_MANUAL,
						CycleTurnability::Never => hover::CYCLE_STILL,
					},
					hover_bounding_circle: Some(BoundingCircle::new(
						Vec2::ZERO,
						SPRITE_LENGTH / 2.0,
					)),
					hover_bounding_box: None,
				},
			));
			parent.spawn((
				SpriteBundle {
					sprite: Sprite {
						custom_size: Some(SPRITE_SIZE * 2.0),
						color: palette.cycle_ready,
						..default()
					},
					texture: image_handles[&ImageKey::CycleRotationArrow].clone_weak(),
					transform: Transform::from_translation(Vec3::Z * layers::CYCLE_CENTER_ARROWS),
					..default()
				},
				SpinAnimation {
					current_phase: rand::thread_rng().gen_range(0.0..std::f32::consts::TAU),
					..default()
				},
			));
			parent.spawn(ColorMesh2dBundle {
				mesh: bevy::sprite::Mesh2dHandle(meshes.add(mesh)),
				material,
				transform: Transform::from_translation(Vec2::ZERO.extend(layers::CYCLE_RINGS)),
				..default()
			});
		})
		.id()
}
