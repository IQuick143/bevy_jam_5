//! Spawn the main level by triggering other observers.

use crate::{
	game::{
		assets::{HandleMap, ImageKey},
		graphics::*,
		level::{layout::CyclePlacement, CycleData, GlyphType, ObjectType, ThingType, VertexData},
		prelude::*,
		LevelID,
	},
	screen::PlayingLevel,
	ui::hover,
};

use bevy::math::{
	bounding::{Aabb2d, BoundingCircle},
	primitives,
};
use bevy::sprite::Anchor::Custom;
use itertools::Itertools;
use rand::Rng;

pub(super) fn plugin(app: &mut App) {
	app.observe(spawn_level);
}

fn spawn_level(
	trigger: Trigger<SpawnLevel>,
	mut events: EventWriter<GameLayoutChanged>,
	mut commands: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut is_level_completed: ResMut<IsLevelCompleted>,
	cycle_material: ResMut<RingMaterial>,
	link_material: Res<LinkMaterial>,
	palette: ResMut<ThingPalette>,
	image_handles: Res<HandleMap<ImageKey>>,
) {
	println!("Spawning!"); //TODO: debug
	let level_id = trigger.event().0;
	let data = &trigger.event().1;
	let layout = &{
		let mut layout = trigger.event().2.clone();
		layout.recompute_to_fit(LEVEL_AREA_WIDTH / 2.0, LEVEL_AREA_CENTER);
		layout
	};

	let vertices: Vec<Entity> = data
		.vertices
		.iter()
		.zip_eq(&layout.vertices)
		.map(|(data, pos)| {
			spawn_vertex(
				commands.reborrow(),
				level_id,
				data,
				*pos,
				meshes.reborrow(),
				cycle_material.0.clone(),
				palette.as_ref(),
				image_handles.as_ref(),
			)
		})
		.collect();

	let cycle_ids = data
		.cycles
		.iter()
		.zip_eq(&layout.cycles)
		.map(|(data, pos)| {
			spawn_cycle(
				commands.reborrow(),
				level_id,
				meshes.reborrow(),
				cycle_material.0.clone(),
				&palette,
				&image_handles,
				data,
				*pos,
				&vertices,
			)
		})
		.collect::<Vec<_>>();

	for (i, cycle_id) in cycle_ids.iter().copied().enumerate() {
		let linked_cycles = data
			.cycles_linked_to(i)
			.map(|(j, dir)| (cycle_ids[j], dir))
			.collect::<Vec<_>>();
		if !linked_cycles.is_empty() {
			commands
				.entity(cycle_id)
				.insert(LinkedCycles(linked_cycles));
		}
	}

	for link in &data.linkages {
		let a = layout.cycles[link.cycle_a_index].position;
		let b = layout.cycles[link.cycle_b_index].position;
		let mesh = primitives::Rectangle::from_size(Vec2::new(
			a.distance(b) - CYCLE_LINK_END_CUT,
			CYCLE_LINK_WIDTH,
		))
		.mesh();
		let mesh = meshes.add(mesh);
		let rotation = Quat::from_axis_angle(Vec3::Z, -(a - b).angle_between(Vec2::X));
		let position = a.lerp(b, 0.5);
		let offset = (a - b).normalize_or_zero().perp() * CYCLE_LINK_SPACING / 2.0;
		commands.spawn((
			StateScoped(PlayingLevel(Some(level_id))),
			ColorMesh2dBundle {
				mesh: bevy::sprite::Mesh2dHandle(mesh.clone_weak()),
				transform: Transform::from_rotation(rotation)
					.with_translation((position + offset).extend(-400.0)),
				material: link_material.clone_weak(),
				..default()
			},
		));
		commands.spawn((
			StateScoped(PlayingLevel(Some(level_id))),
			ColorMesh2dBundle {
				mesh: bevy::sprite::Mesh2dHandle(mesh),
				transform: Transform::from_rotation(rotation)
					.with_translation((position - offset).extend(-400.0)),
				material: link_material.clone_weak(),
				..default()
			},
		));
	}

	is_level_completed.0 = false;
	events.send(GameLayoutChanged);
}

fn spawn_vertex(
	mut commands: Commands,
	level_id: LevelID,
	data: &VertexData,
	position: Vec2,
	mut meshes: Mut<Assets<Mesh>>,
	base_material: Handle<ColorMaterial>,
	palette: &ThingPalette,
	image_handles: &HandleMap<ImageKey>,
) -> Entity {
	let transform =
		TransformBundle::from_transform(Transform::from_translation(position.extend(0.0)));
	let vertex_id = commands
		.spawn((
			Vertex,
			StateScoped(PlayingLevel(Some(level_id))),
			PlacedGlyph(None),
			PlacedObject(None),
			transform,
		))
		.id();
	let mesh = primitives::Circle::new(NODE_RADIUS).mesh();
	commands.spawn((
		StateScoped(PlayingLevel(Some(level_id))),
		ColorMesh2dBundle {
			transform: Transform::from_translation(position.extend(-100.0)),
			mesh: bevy::sprite::Mesh2dHandle(meshes.add(mesh)),
			material: base_material,
			..default()
		},
	));
	if let Some(object_type) = data.object {
		let thing_type = ThingType::Object(object_type);
		let object_id = match object_type {
			ObjectType::Player => commands
				.spawn((
					StateScoped(PlayingLevel(Some(level_id))),
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
						transform: Transform::from_translation(position.extend(-10.0)),
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
				))
				.id(),
			ObjectType::Box => commands
				.spawn((
					StateScoped(PlayingLevel(Some(level_id))),
					Object,
					Box,
					VertexPosition(vertex_id),
					ObjectKind(thing_type),
					SpriteBundle {
						sprite: Sprite {
							color: palette.box_base,
							custom_size: Some(SPRITE_SIZE),
							anchor: Custom(Vec2::new(0.0, -0.25)),
							..default()
						},
						texture: image_handles[&ImageKey::Object(thing_type)].clone_weak(),
						transform: Transform::from_translation(position.extend(-10.0)),
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
				))
				.id(),
		};
		commands
			.entity(vertex_id)
			.insert(PlacedObject(Some(object_id)));
	}
	if let Some(glyph_type) = data.glyph {
		let thing_type = ThingType::Glyph(glyph_type);
		let glyph_id = match glyph_type {
			GlyphType::Button => commands
				.spawn((
					StateScoped(PlayingLevel(Some(level_id))),
					Glyph,
					BoxSlot,
					VertexPosition(vertex_id),
					ObjectKind(thing_type),
					SpriteBundle {
						sprite: Sprite {
							color: palette.button_base,
							custom_size: Some(SPRITE_SIZE),
							anchor: Custom(Vec2::new(0.0, -0.25)),
							..default()
						},
						texture: image_handles[&ImageKey::Object(thing_type)].clone_weak(),
						transform: Transform::from_translation(position.extend(-50.0)),
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
				))
				.id(),
			GlyphType::Flag => commands
				.spawn((
					StateScoped(PlayingLevel(Some(level_id))),
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
						transform: Transform::from_translation(position.extend(-50.0)),
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
				))
				.id(),
		};
		commands
			.entity(vertex_id)
			.insert(PlacedGlyph(Some(glyph_id)));
	}

	vertex_id
}

fn spawn_cycle(
	mut commands: Commands,
	level_id: LevelID,
	mut meshes: Mut<Assets<Mesh>>,
	material: Handle<ColorMaterial>,
	palette: &ThingPalette,
	image_handles: &HandleMap<ImageKey>,
	data: &CycleData,
	placement: CyclePlacement,
	vertex_entities: &[Entity],
) -> Entity {
	let mesh = primitives::Annulus::new(
		placement.radius - RING_HALF_WIDTH,
		placement.radius + RING_HALF_WIDTH,
	)
	.mesh()
	.resolution(64)
	.build();

	commands
		.spawn((
			data.cycle_turnability,
			StateScoped(PlayingLevel(Some(level_id))),
			ComputedCycleTurnability(true),
			CycleVertices(
				data.vertex_indices
					.iter()
					.map(|i| *vertex_entities.get(*i).unwrap())
					.collect(),
			),
			CycleInterationRadius(placement.radius),
			CycleInteraction::default(),
			TransformBundle::from_transform(Transform::from_translation(
				placement.position.extend(0.0),
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
					texture: image_handles[&ImageKey::CycleCenter(data.cycle_turnability)]
						.clone_weak(),
					transform: Transform::from_translation(Vec2::ZERO.extend(-300.0)),
					..default()
				},
				JumpTurnAnimation::default(),
				Hoverable {
					hover_text: match data.cycle_turnability {
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
					transform: Transform::from_translation(Vec3::Z * -250.0),
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
				transform: Transform::from_translation(Vec2::ZERO.extend(-200.0)),
				..default()
			});
		})
		.id()
}
