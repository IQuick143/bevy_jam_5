//! Spawn the main level by triggering other observers.

use crate::{
	game::{
		assets::{HandleMap, ImageKey},
		graphics::*,
		level::{
			self,
			layout::{CyclePlacement, LevelLayout},
			CycleData, GlyphType, ObjectType, ThingType, ValidLevelData, VertexData,
		},
		prelude::*,
	},
	screen::{DestroyOnTransition, Screen},
};

use bevy::math::primitives;
use bevy::sprite::Anchor::Custom;
use itertools::Itertools;
use rand::Rng;

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
			Cycle => {
				r"
VERTEX a b c d

CYCLE[MANUAL] circle a b c d

OBJECT[BOX] a
OBJECT[BUTTON] c
OBJECT[PLAYER] b
OBJECT[FLAG] d

PLACE circle 0 0 300
"
			}
			Bicycle => {
				r"
VERTEX a b c d e f g h

CYCLE[ENGINE] back_wheel a b c d
CYCLE[ENGINE] front_wheel e f g h
CYCLE[MANUAL] chain b h

OBJECT[BOX] f
OBJECT[BUTTON] d
OBJECT[PLAYER] d
OBJECT[FLAG] f

PLACE back_wheel -400 0 300
PLACE front_wheel 400 0 300
PLACE chain 0 0 100
"
			}
			Tricycle => {
				r"
VERTEX b1 b2 b3 bgi bgo bri bro r1 r2 r3 rgi rgo g1 g2 g3

CYCLE[MANUAL] blue b1 b2 b3 bgo bri bgi bro
CYCLE[MANUAL] red r1 r2 r3 bro rgi bri rgo
CYCLE[MANUAL] green g1 g2 g3 rgo bgi rgi bgo

OBJECT[BOX]
OBJECT[BUTTON] 
OBJECT[PLAYER] b2 r2 g2
OBJECT[FLAG] rgi bri bgi

PLACE blue -200 280 300
PLACE red 0 0 300
PLACE green 200 280 300
"
			}
			CargoTricycle => {
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
			SquareCycle => {
				r"
VERTEX a b c d

CYCLE[MANUAL] red a b
CYCLE[MANUAL] blue b c
CYCLE[MANUAL] orange c d
CYCLE[MANUAL] green d a

OBJECT[BOX] b
OBJECT[BUTTON] c
OBJECT[PLAYER] a
OBJECT[FLAG] d

PLACE red -300 300 300
PLACE blue 300 300 300
PLACE orange 300 -300 300
PLACE green -300 -300 300
"
			}
			DiamondCycle => {
				r"
VERTEX a b c d

CYCLE[MANUAL] top a b
CYCLE[MANUAL] right b c
CYCLE[MANUAL] bottom c d
CYCLE[MANUAL] left d a

OBJECT[BOX] b
OBJECT[BUTTON] c
OBJECT[PLAYER] a
OBJECT[FLAG] d

PLACE top 0 424 300
PLACE right 424 0 300
PLACE bottom 0 -424 300
PLACE left -424 0 300
"
			}
			Lotus => {
				r"
VERTEX n e s w n1 n2 n3 e1 e2 e3 s1 s2 s3 w1 w2 w3

CYCLE[MANUAL] center n e s w
CYCLE[MANUAL] north n n1 n2 n3
CYCLE[MANUAL] east e e1 e2 e3
CYCLE[MANUAL] south s s1 s2 s3
CYCLE[MANUAL] west w w1 w2 w3
CYCLE[MANUAL] ne n3 e1
CYCLE[MANUAL] se e3 s1
CYCLE[MANUAL] sw s3 w1
CYCLE[MANUAL] nw w3 n1

OBJECT[BOX] n2 e2 s2 w2
OBJECT[BUTTON] n e s w
OBJECT[PLAYER] s1
OBJECT[FLAG] s1

PLACE center 0 0 300
PLACE north 0 600 300
PLACE east 600 0 300
PLACE south 0 -600 300
PLACE west -600 0 300
PLACE ne 400 400 150
PLACE se 400 -400 150
PLACE sw -400 -400 150
PLACE nw -400 400 150
"
			}
			ThreeInARow => {
				r"
VERTEX a b c d e f g h i j k l m n o p q r s t u v w x

CYCLE[MANUAL] top_left a e h d
CYCLE[MANUAL] top_mid b f i e
CYCLE[MANUAL] top_right c g j f
CYCLE[MANUAL] mid_left h l o k
CYCLE[MANUAL] mid_mid i m p l
CYCLE[MANUAL] mid_right j n q m
CYCLE[MANUAL] bot_left o s v r
CYCLE[MANUAL] bot_mid p t w s
CYCLE[MANUAL] bot_right q u x t

OBJECT[BOX]
OBJECT[BUTTON]
OBJECT[PLAYER] a
OBJECT[FLAG] x

PLACE top_left -600 600 300
PLACE top_mid 0 600 300
PLACE top_right 600 600 300
PLACE mid_left -600 0 300
PLACE mid_mid 0 0 300
PLACE mid_right 600 0 300
PLACE bot_left -600 -600 300
PLACE bot_mid 0 -600 300
PLACE bot_right 600 -600 300
"
			}
			TripleRing => {
				r"
VERTEX a b c d e f g h i j k l m

CYCLE[MANUAL] left a b g k j e
CYCLE[MANUAL] middle c h l k f b
CYCLE[MANUAL] right d i m l g c

OBJECT[BOX] b c k l
OBJECT[BUTTON] a d m j
OBJECT[PLAYER] e i
OBJECT[FLAG] g

PLACE left -300 0 300
PLACE middle 0 0 300
PLACE right 300 0 300
"
			}
			Car => {
				r"
VERTEX a b c d e f g h i j k l m n o p

CYCLE[MANUAL] left a b h m l f
CYCLE[MANUAL] middle_left i n m g b c
CYCLE[MANUAL] middle_right j o n h c d
CYCLE[MANUAL] right k p o i d e

OBJECT[BOX] h j
OBJECT[BUTTON] f h
OBJECT[PLAYER] f
OBJECT[FLAG] j

PLACE left -300 0 300
PLACE middle_left 0 0 300
PLACE middle_right 300 0 300
PLACE right 600 0 300
"
			}
			Olympic => {
				r"
VERTEX a b c d e f g h i j k l m n o p q r s t u v

CYCLE[MANUAL] blue a b h n m g
CYCLE[MANUAL] yellow t s n h i o
CYCLE[MANUAL] black c d j p o i
CYCLE[MANUAL] green v u p j k q
CYCLE[MANUAL] red e f l r q k

OBJECT[BOX] b c d e f h i j k l m n o p q r s t u v
OBJECT[BUTTON] a b c d e g h i j k
OBJECT[PLAYER] a g
OBJECT[FLAG] f l

PLACE blue -400 300 300
PLACE yellow 0 0 300
PLACE black 400 300 300
PLACE green 800 0 300
PLACE red 1200 300 300
"
			}
			Pedalo => {
				r"
VERTEX a b c d e f g h

CYCLE[MANUAL] back_wheel a b c d
CYCLE[MANUAL] front_wheel e f g h

LINK[STRAIGHT] back_wheel front_wheel

OBJECT[BOX] e
OBJECT[BUTTON] g
OBJECT[PLAYER] a
OBJECT[FLAG] c

PLACE back_wheel -300 0 300
PLACE front_wheel 300 0 300
"
			}
			Pyramid => {
				r"
VERTEX a b c d e f g h i j k l

CYCLE[MANUAL] middle b c f k j e
CYCLE[MANUAL] top a c b
CYCLE[MANUAL] bot_right g l k f
CYCLE[MANUAL] bot_left d e j i h

OBJECT[BOX]
OBJECT[BUTTON]
OBJECT[PLAYER] a b c
OBJECT[FLAG] a h g

PLACE top 0 500 300
PLACE middle 0 0 300
PLACE bot_right 400 -300 300
PLACE bot_left -400 -300 300
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
		.spawn((
			Vertex,
			DestroyOnTransition,
			PlacedGlyph(None),
			PlacedObject(None),
			transform,
		))
		.id();
	let mesh = primitives::Circle::new(NODE_RADIUS).mesh();
	commands.spawn((
		DestroyOnTransition,
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
					DestroyOnTransition,
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
					DestroyOnTransition,
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
					DestroyOnTransition,
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
					DestroyOnTransition,
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
			DestroyOnTransition,
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
					texture: image_handles[&ImageKey::CycleCenter(data.cycle_turnability)]
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
