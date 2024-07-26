//! Spawn the main level by triggering other observers.

use crate::{
	game::{
		assets::{HandleMap, ImageKey},
		graphics::{LEVEL_AREA_CENTER, LEVEL_AREA_WIDTH, RING_HALF_WIDTH, SPRITE_SIZE},
		level::{
			self, layout::{CyclePlacement, LevelLayout}, CycleData, GlyphType, ObjectType, ThingType, ValidLevelData, VertexData
		},
		prelude::*,
	},
	screen::Screen,
};

use bevy::math::primitives;
use bevy::sprite::Anchor::Custom;
use itertools::Itertools;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		load_level.run_if(on_event::<StateTransitionEvent<Screen>>()),
	)
	.observe(spawn_level);
}

#[derive(Event, Debug)]
pub struct SpawnLevel(pub ValidLevelData, pub LevelLayout);

fn load_level(mut commands: Commands, mut event: EventReader<StateTransitionEvent<Screen>>) {
	let Some(StateTransitionEvent {
		exited: _,
		entered: Some(Screen::Level(level_id)),
	}) = event.read().last()
	else {
		return;
	};

	let data = {
		use crate::game::LevelID::*;
		match level_id {
			Level1 => {
				r"
VERTEX b1 b2 b3 bgi bgo bri bro r1 r2 r3 rgi rgo g1 g2 g3

CYCLE[MANUAL] blue b1 b2 b3 bgo bri bgi bro
CYCLE[MANUAL] red r1 r2 r3 bro rgi bri rgo
CYCLE[MANUAL] green g1 g2 g3 rgo bgi rgi bgo

OBJECT[BOX] b2 r2 g2
OBJECT[BUTTON] rgi bri bgi
OBJECT[PLAYER] rgi bri bgi
OBJECT[FLAG] b2 r2 g2

PLACE blue -200 280 300
PLACE red 0 0 300
PLACE green 200 280 300
"
			}
		}
	};
	let level_file = level::parser::parse(data).unwrap();
	let level: ValidLevelData = level_file.data.try_into().unwrap();
	let mut layout_builder = level::layout::LevelLayoutBuilder::new(&level);
	for placement in level_file.layout {
		layout_builder.add_placement(placement).unwrap();
	}
	let level_layout = layout_builder.build().unwrap();
	eprintln!("{level_layout:?}");
	commands.trigger(SpawnLevel(level, level_layout));
}

fn spawn_level(
	trigger: Trigger<SpawnLevel>,
	mut events: EventWriter<GameLayoutChanged>,
	mut commands: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	cycle_material: ResMut<RingMaterial>,
	thing_materials: ResMut<ThingColor>,
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
				thing_materials.as_ref(),
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
	materials: &ThingColor,
	image_handles: &HandleMap<ImageKey>,
	texture_atlas_layout: Handle<TextureAtlasLayout>,
) -> Entity {
	let transform =
		TransformBundle::from_transform(Transform::from_translation(position.extend(0.0)));
	let vertex_id = commands
		.spawn((Vertex, PlacedGlyph(None), PlacedObject(None), transform))
		.id();
	commands.spawn((
		SpriteBundle {
			texture: image_handles[&ImageKey::Ducky].clone_weak(),
			transform: Transform::from_translation(position.extend(-100.0)),
			..Default::default()
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
							color: *materials.get(thing_type),
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
							color: *materials.get(thing_type),
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
							color: *materials.get(thing_type),
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
							color: *materials.get(thing_type),
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
			ColorMesh2dBundle {
				transform: Transform::from_translation(placement.position.extend(-200.0)),
				mesh: bevy::sprite::Mesh2dHandle(meshes.add(mesh)),
				material,
				..default()
			},
		))
		.id()
}
