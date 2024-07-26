//! Spawn the main level by triggering other observers.

use crate::game::{
	assets::{HandleMap, ImageKey},
	graphics::*,
	level::{
		layout::{CyclePlacement, LevelLayout},
		CycleData, GlyphType, ObjectType, ThingType, ValidLevelData, VertexData,
	},
	prelude::*,
};

use bevy::math::primitives;
use bevy::sprite::Anchor::Custom;
use itertools::Itertools;
use rand::Rng;

pub(super) fn plugin(app: &mut App) {
	app.observe(spawn_level);
}

#[derive(Event, Debug)]
pub struct SpawnLevel(pub ValidLevelData, pub LevelLayout);

fn spawn_level(
	trigger: Trigger<SpawnLevel>,
	mut events: EventWriter<GameLayoutChanged>,
	mut commands: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	cycle_material: ResMut<RingMaterial>,
	palette: ResMut<ThingPalette>,
	image_handles: Res<HandleMap<ImageKey>>,
	mut texture_atlas_layouts: ResMut<Assets<TextureAtlasLayout>>,
) {
	println!("Spawning!"); //TODO: debug
	let data = trigger.event().0.clone();
	let layout = &{
		let mut layout = trigger.event().1.clone();
		layout.recompute_to_fit(LEVEL_AREA_WIDTH / 2.0, LEVEL_AREA_CENTER);
		layout
	};

	let texture_layout =
		TextureAtlasLayout::from_grid(UVec2::splat(32), 6, 2, Some(UVec2::splat(1)), None);
	let texture_atlas_layout = texture_atlas_layouts.add(texture_layout);

	let vertices: Vec<Entity> = data
		.vertices
		.iter()
		.zip_eq(&layout.vertices)
		.map(|(data, pos)| {
			spawn_vertex(
				commands.reborrow(),
				data,
				*pos,
				meshes.reborrow(),
				cycle_material.0.clone(),
				palette.as_ref(),
				image_handles.as_ref(),
				texture_atlas_layout.clone(),
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

	commands.init_resource::<LevelCompletionConditions>();
	events.send(GameLayoutChanged);
}

fn spawn_vertex(
	mut commands: Commands,
	data: &VertexData,
	position: Vec2,
	mut meshes: Mut<Assets<Mesh>>,
	base_material: Handle<ColorMaterial>,
	palette: &ThingPalette,
	image_handles: &HandleMap<ImageKey>,
	texture_atlas_layout: Handle<TextureAtlasLayout>,
) -> Entity {
	let transform =
		TransformBundle::from_transform(Transform::from_translation(position.extend(0.0)));
	let vertex_id = commands
		.spawn((Vertex, PlacedGlyph(None), PlacedObject(None), transform))
		.id();
	let mesh = primitives::Circle::new(NODE_RADIUS).mesh();
	commands.spawn((
		ColorMesh2dBundle {
			transform: Transform::from_translation(position.extend(-100.0)),
			mesh: bevy::sprite::Mesh2dHandle(meshes.add(mesh)),
			material: base_material,
			..default()
		},
		TextureAtlas {
			layout: texture_atlas_layout.clone(),
			index: 0,
		},
	));
	if let Some(object_type) = data.object {
		let thing_type = ThingType::Object(object_type);
		let object_id = match object_type {
			ObjectType::Player => commands
				.spawn((
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
				))
				.id(),
			ObjectType::Box => commands
				.spawn((
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
				))
				.id(),
			GlyphType::Flag => commands
				.spawn((
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
			ComputedCycleTurnability(true),
			CycleVertices(
				data.vertex_indices
					.iter()
					.map(|i| *vertex_entities.get(*i).unwrap())
					.collect(),
			),
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
					texture: image_handles[&ImageKey::CycleCenter(CycleTurnability::WithPlayer)]
						.clone_weak(),
					transform: Transform::from_translation(Vec2::ZERO.extend(-300.0)),
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
