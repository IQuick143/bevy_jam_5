//! Initialization of level content entities

use super::{
	components::*,
	drawing::*,
	inputs::CycleInteraction,
	level::*,
	logic::{ComputedCycleTurnability, IsTriggered},
	prelude::*,
};
use crate::{assets::*, graphics::*, AppSet};
use bevy::{ecs::schedule::ScheduleLabel, sprite::Anchor};
use std::f32::consts::{PI, TAU};

pub(super) fn plugin(app: &mut App) {
	use LevelInitializationSet::*;
	app.init_resource::<LastLevelSessionId>()
		.init_resource::<ExpiringLevelSessionId>()
		.add_event::<EnterLevel>()
		.add_event::<SpawnLevel>()
		.configure_sets(
			LevelInitialization,
			(SpawnPrimaryEntities, SpawnVisuals).chain(),
		)
		.add_systems(
			Update,
			(
				handle_enter_level.after(AppSet::ExecuteInput),
				(
					(|w: &mut World| w.run_schedule(LevelInitialization))
						.run_if(on_event::<SpawnLevel>),
					despawn_expired_level_entities
						.run_if(resource_changed::<ExpiringLevelSessionId>),
				)
					.after(handle_enter_level)
					.before(AppSet::GameLogic),
			),
		)
		.add_systems(
			LevelInitialization,
			(
				(spawn_primary_level_entities, spawn_thing_entities)
					.chain()
					.in_set(SpawnPrimaryEntities),
				(
					set_vertex_transforms,
					set_thing_transforms,
					set_cycle_transforms,
				)
					.after(SpawnPrimaryEntities),
				(
					create_vertex_visuals,
					create_cycle_visuals,
					create_link_visuals,
					create_thing_sprites,
					create_box_color_markers,
					create_button_color_markers,
				)
					.in_set(SpawnVisuals),
			),
		);
}

/// Schedule that runs within [`Update`] schedule
/// every time a non-empty level is entered.
#[derive(ScheduleLabel, Clone, Copy, PartialEq, Eq, Hash, Default, Debug)]
pub struct LevelInitialization;

/// System sets for initialization of a level.
/// All of these run after inputs have been processed and dispatched,
/// but before game logic is run for that frame.
#[derive(SystemSet, Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum LevelInitializationSet {
	/// Spawning of all structural entities
	SpawnPrimaryEntities,
	/// Spawning of sprites and meshes that visualize level entities
	SpawnVisuals,
}

/// An event that is sent to switch the game to a level
/// or to exit a level entirely
#[derive(Event, Clone, Debug)]
pub struct EnterLevel(pub Option<Handle<LevelData>>);

/// An event that is sent to spawn entities for a particular level.
/// These will replace any older entities
#[derive(Event, Clone, Debug)]
pub struct SpawnLevel(pub Handle<LevelData>, pub LevelSessionId);

/// Identifier of a level session.
/// Every time a level is entered, all its entities are assigned a unique identifier.
#[derive(Component, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Debug, Reflect)]
pub struct LevelSessionId(usize);

/// The last [`LevelSessionId`] used by a level spawn
#[derive(Resource, Clone, Copy, Default, Debug, Reflect)]
struct LastLevelSessionId(LevelSessionId);

/// The last [`LevelSessionId`] whose entities should be despawned
#[derive(Resource, Clone, Copy, Default, Debug, Reflect)]
struct ExpiringLevelSessionId(LevelSessionId);

fn handle_enter_level(
	mut enter_events: EventReader<EnterLevel>,
	mut spawn_events: EventWriter<SpawnLevel>,
	mut last_session_id: ResMut<LastLevelSessionId>,
	mut expiring_session_id: ResMut<ExpiringLevelSessionId>,
) {
	if let Some(event) = enter_events.read().last() {
		expiring_session_id.0 = last_session_id.0;
		if let Some(level_handle) = &event.0 {
			last_session_id.0 .0 += 1;
			spawn_events.send(SpawnLevel(level_handle.clone_weak(), last_session_id.0));
		}
	}
}

fn despawn_expired_level_entities(
	mut commands: Commands,
	expiring_session_id: Res<ExpiringLevelSessionId>,
	query: Query<(Entity, &LevelSessionId)>,
) {
	for (id, session) in &query {
		if *session <= expiring_session_id.0 {
			commands.entity(id).despawn_recursive();
		}
	}
}

fn spawn_primary_level_entities(
	mut commands: Commands,
	mut events: EventReader<SpawnLevel>,
	mut levels: ResMut<Assets<LevelData>>,
) {
	for SpawnLevel(level_handle, session_id) in events.read() {
		// Get the level data
		let Some(level) = levels.get(level_handle) else {
			log::warn!("Got an invalid level handle");
			continue;
		};

		// Spawn vertices
		let vertices = level
			.vertices
			.iter()
			.map(|vertex| {
				commands
					.spawn((
						*session_id,
						*vertex,
						Vertex,
						Transform::default(),
						Visibility::default(),
					))
					.id()
			})
			.collect::<Vec<_>>();

		// Spawn cycles
		let cycles = level
			.cycles
			.iter()
			.enumerate()
			.map(|(id, cycle)| {
				commands
					.spawn((
						*session_id,
						cycle.placement,
						cycle.center_sprite_appearence,
						cycle.turnability,
						Cycle {
							id,
							group_id: cycle.group,
							orientation_within_group: cycle.orientation_within_group,
						},
						ComputedCycleTurnability(false),
						CycleInteraction::default(),
						CycleVertices(cycle.vertex_indices.iter().map(|i| vertices[*i]).collect()),
						Transform::default(),
						Visibility::default(),
					))
					.id()
			})
			.collect::<Vec<_>>();

		// Spawn links
		// Links are children of their source cycle
		for link in &level.declared_links {
			let target_cycle = commands
				.get_entity(cycles[link.dest_cycle])
				.expect("The entity has just been spawned")
				.id();
			commands
				.get_entity(cycles[link.source_cycle])
				.expect("The entity has just been spawned")
				.with_children(|children| {
					children.spawn((
						LinkTargetCycle(target_cycle),
						link.direction,
						Transform::default(),
						Visibility::default(),
					));
				});
		}
		for link in &level.declared_one_way_links {
			let target_cycle = commands
				.get_entity(cycles[link.dest_cycle])
				.expect("The entity has just been spawned")
				.id();
			commands
				.get_entity(cycles[link.source])
				.expect("The entity has just been spawned")
				.with_children(|children| {
					children.spawn((
						LinkTargetCycle(target_cycle),
						link.direction,
						LinkMultiplicity(link.multiplicity),
						Transform::default(),
						Visibility::default(),
					));
				});
		}

		// Spawn cycle list
		commands.insert_resource(CycleEntities(cycles));
		commands.insert_resource(LevelHandle(
			levels
				.get_strong_handle(level_handle.id())
				.expect("I expect you to work."),
		));
	}
}

fn spawn_thing_entities(
	mut commands: Commands,
	query: Query<(Entity, &VertexData, &LevelSessionId), Added<VertexData>>,
) {
	for (id, data, session) in &query {
		let object_id = data.object.map(|object| match object {
			ObjectData::Player => commands
				.spawn((
					Object,
					Player,
					object,
					ThingData::Object(object),
					*session,
					VertexPosition(id),
					IsTriggered::default(),
					Transform::default(),
					Visibility::default(),
				))
				.id(),
			ObjectData::Box(None) => commands
				.spawn((
					Object,
					Box,
					object,
					ThingData::Object(object),
					*session,
					VertexPosition(id),
					IsTriggered::default(),
					Transform::default(),
					Visibility::default(),
				))
				.id(),
			ObjectData::Box(Some(color)) => commands
				.spawn((
					Object,
					Box,
					color,
					object,
					ThingData::Object(object),
					*session,
					VertexPosition(id),
					IsTriggered::default(),
					Transform::default(),
					Visibility::default(),
				))
				.id(),
		});
		let glyph_id = data.glyph.map(|glyph| match glyph {
			GlyphData::Flag => commands
				.spawn((
					Glyph,
					Goal,
					glyph,
					ThingData::Glyph(glyph),
					*session,
					VertexPosition(id),
					IsTriggered::default(),
					Transform::default(),
					Visibility::default(),
				))
				.id(),
			GlyphData::Button(None) => commands
				.spawn((
					Glyph,
					BoxSlot,
					glyph,
					ThingData::Glyph(glyph),
					*session,
					VertexPosition(id),
					IsTriggered::default(),
					Transform::default(),
					Visibility::default(),
				))
				.id(),
			GlyphData::Button(Some(color_data)) => commands
				.spawn((
					Glyph,
					BoxSlot,
					glyph,
					color_data,
					ThingData::Glyph(glyph),
					*session,
					VertexPosition(id),
					IsTriggered::default(),
					Transform::default(),
					Visibility::default(),
				))
				.id(),
		});
		commands
			.entity(id)
			.insert((PlacedObject(object_id), PlacedGlyph(glyph_id)));
	}
}

fn set_vertex_transforms(mut query: Query<(&VertexData, &mut Transform), Added<VertexData>>) {
	for (data, mut transform) in &mut query {
		transform.translation.x = data.position.x;
		transform.translation.y = data.position.y;
	}
}

fn set_thing_transforms(
	mut things_q: Query<(&VertexPosition, &mut Transform), Added<VertexPosition>>,
	vertices_q: Query<&VertexData>,
) {
	for (vertex, mut transform) in &mut things_q {
		let vertex_data = vertices_q
			.get(vertex.0)
			.expect("Parent of ThingData does not have VertexData component");
		transform.translation.x = vertex_data.position.x;
		transform.translation.y = vertex_data.position.y;
	}
}

fn set_cycle_transforms(
	mut query: Query<(&CyclePlacement, &mut Transform), Added<CyclePlacement>>,
) {
	for (placement, mut transform) in &mut query {
		transform.translation.x = placement.position.x;
		transform.translation.y = placement.position.y;
	}
}

fn create_vertex_visuals(
	mut commands: Commands,
	query: Query<Entity, Added<VertexData>>,
	materials: Res<GameObjectMaterials>,
	meshes: Res<GameObjectMeshes>,
) {
	for id in &query {
		let node = commands
			.spawn((
				Mesh2d(meshes.vertices.clone_weak()),
				MeshMaterial2d(materials.cycle_rings_ready.clone_weak()),
				Transform::from_translation(Vec3::Z * layers::CYCLE_RINGS),
			))
			.id();
		let outline = commands
			.spawn((
				Mesh2d(meshes.vertex_outlines.clone_weak()),
				MeshMaterial2d(materials.cycle_ring_outlines.clone_weak()),
				Transform::from_translation(Vec3::Z * layers::CYCLE_RING_OUTLINES),
			))
			.id();
		commands
			.entity(id)
			.insert(VertexVisualEntities { node, outline })
			.add_children(&[node, outline]);
	}
}

fn create_cycle_visuals(
	mut commands: Commands,
	query: Query<
		(
			Entity,
			&CyclePlacement,
			&CycleTurnability,
			&CycleCenterSpriteAppearence,
		),
		Added<CyclePlacement>,
	>,
	palette: Res<ThingPalette>,
	materials: Res<GameObjectMaterials>,
	images: Res<HandleMap<ImageKey>>,
	mut meshes: ResMut<Assets<Mesh>>,
) {
	for (id, placement, turnability, center_sprite) in &query {
		let mesh;
		let outline_mesh;

		match placement.shape {
			CycleShape::Circle(radius) => {
				mesh = Annulus::new(radius - RING_HALF_WIDTH, radius + RING_HALF_WIDTH)
					.mesh()
					.resolution(cycle_ring_mesh_resolution(radius))
					.build();
				outline_mesh = Annulus::new(
					radius - RING_HALF_WIDTH - RING_OUTLINE_WIDTH,
					radius + RING_HALF_WIDTH + RING_OUTLINE_WIDTH,
				)
				.mesh()
				.resolution(cycle_ring_mesh_resolution(radius))
				.build();
			}
		}

		let ring = commands
			.spawn((
				Mesh2d(meshes.add(mesh)),
				MeshMaterial2d(materials.cycle_rings_ready.clone_weak()),
				Transform::from_translation(Vec3::Z * layers::CYCLE_RINGS),
			))
			.id();
		let outline = commands
			.spawn((
				Mesh2d(meshes.add(outline_mesh)),
				MeshMaterial2d(materials.cycle_ring_outlines.clone_weak()),
				Transform::from_translation(Vec3::Z * layers::CYCLE_RING_OUTLINES),
			))
			.id();
		commands
			.entity(id)
			.insert(CycleRingVisualEntities { ring, outline })
			.add_children(&[ring, outline]);

		if let Some(offset) = center_sprite.0 {
			let sprite = commands
				.spawn((
					Sprite {
						custom_size: Some(SPRITE_SIZE),
						image: images[&ImageKey::CycleCenter(*turnability)].clone_weak(),
						color: palette.cycle_ready,
						..default()
					},
					Transform::from_translation(offset.extend(layers::CYCLE_CENTER_SPRITES)),
				))
				.id();
			let arrow = commands
				.spawn((
					Sprite {
						custom_size: Some(SPRITE_SIZE * 2.0),
						image: images[&ImageKey::CycleRotationArrow].clone_weak(),
						color: palette.cycle_ready,
						..default()
					},
					Transform::from_translation(offset.extend(layers::CYCLE_CENTER_ARROWS)),
				))
				.id();
			commands
				.entity(id)
				.insert(CycleCenterVisualEntities { sprite, arrow })
				.add_children(&[sprite, arrow]);
		}
	}
}

fn create_link_visuals(
	mut commands: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	materials: Res<GameObjectMaterials>,
	links_q: Query<
		(
			Entity,
			&Parent,
			&LinkTargetCycle,
			&LinkedCycleDirection,
			Option<&LinkMultiplicity>,
		),
		Added<LinkTargetCycle>,
	>,
	mut cycles_q: Query<(&CyclePlacement, &CycleCenterSpriteAppearence)>,
) {
	for (id, source, dest, direction, multiplicity) in &links_q {
		// Fetch endpoints
		let a = get_link_endpoint(source.get(), cycles_q.reborrow());
		let b = get_link_endpoint(dest.0, cycles_q.reborrow());
		let (Some(a), Some(b)) = (a, b) else {
			continue;
		};

		// Spawn the visuals under the link entity
		commands.entity(id).with_children(|children| {
			if let Some(multiplicity) = multiplicity {
				create_one_way_link_visual(
					children,
					a,
					b,
					*direction,
					**multiplicity,
					&mut meshes,
					materials.link_lines.clone_weak(),
				);
			} else {
				create_hard_link_visual(
					children,
					a,
					b,
					*direction,
					&mut meshes,
					materials.link_lines.clone_weak(),
				);
			}
		});
	}
}

fn get_link_endpoint(
	cycle_id: Entity,
	query: Query<(&CyclePlacement, &CycleCenterSpriteAppearence)>,
) -> Option<Vec2> {
	let Ok((placement, center_sprite)) = query.get(cycle_id) else {
		// Skip drawing link if its endpoints are not cycles
		log::warn!("Link endpoint entity does not have CyclePlacement or CycleCenterSpriteAppearence component");
		return None;
	};

	if let Some(center_offset) = center_sprite.0 {
		// Links are anchored at cycle center sprites
		Some(placement.position + center_offset)
	} else {
		// Links cannot be drawn if a cycle does not have center sprite
		log::warn!("Visible link to a cycle with invisible center sprite");
		None
	}
}

fn create_hard_link_visual(
	children: &mut ChildBuilder,
	a: Vec2,
	b: Vec2,
	direction: LinkedCycleDirection,
	meshes: &mut Assets<Mesh>,
	material: Handle<ColorMaterial>,
) {
	let d_sq = a.distance_squared(b);
	if d_sq <= CYCLE_LINK_SPACING.powi(2) {
		// The link cannot be rendered if the cycles are too close
		log::warn!("Skipped drawing a cycle link because the cycles are {} units apart, need at least {CYCLE_LINK_SPACING}", d_sq.sqrt());
		return;
	}
	let connector_length = match direction {
		LinkedCycleDirection::Coincident => a.distance(b),
		LinkedCycleDirection::Inverse => (d_sq - CYCLE_LINK_SPACING.powi(2)).sqrt(),
	};
	let mesh = Rectangle::from_size(Vec2::new(
		connector_length - CYCLE_LINK_END_CUT,
		CYCLE_LINK_WIDTH,
	))
	.mesh();
	let mesh = meshes.add(mesh);
	let dir_from_a_to_b = (a - b).normalize();
	let rotation = Quat::from_rotation_arc_2d(Vec2::X, dir_from_a_to_b);
	let position = (b - a) / 2.0;
	let offset = match direction {
		LinkedCycleDirection::Coincident => dir_from_a_to_b.perp() * CYCLE_LINK_SPACING / 2.0,
		LinkedCycleDirection::Inverse => Vec2::ZERO,
	};
	let extra_rotation = match direction {
		LinkedCycleDirection::Coincident => Quat::IDENTITY,
		LinkedCycleDirection::Inverse => Quat::from_rotation_arc_2d(
			Vec2::X,
			Vec2::new(d_sq.sqrt(), CYCLE_LINK_SPACING).normalize(),
		),
	};
	children.spawn((
		Mesh2d(mesh.clone_weak()),
		Transform::from_rotation(rotation.mul_quat(extra_rotation))
			.with_translation((position + offset).extend(layers::CYCLE_LINKS)),
		MeshMaterial2d(material.clone_weak()),
	));
	children.spawn((
		Mesh2d(mesh),
		Transform::from_rotation(rotation.mul_quat(extra_rotation.inverse()))
			.with_translation((position - offset).extend(layers::CYCLE_LINKS)),
		MeshMaterial2d(material.clone_weak()),
	));
}

fn create_one_way_link_visual(
	children: &mut ChildBuilder,
	a: Vec2,
	b: Vec2,
	_direction: LinkedCycleDirection,
	_multiplicity: u64,
	meshes: &mut Assets<Mesh>,
	material: Handle<ColorMaterial>,
) {
	let d = a.distance(b);
	let line_length = d - CYCLE_LINK_END_CUT - ONEWAY_LINK_TARGET_OFFSET;
	if line_length < 0.0 {
		// The link cannot be rendered if the cycles are too close
		log::warn!(
			"Skipped drawing a cycle link because the cycles are {d} units apart, need at least {}",
			CYCLE_LINK_END_CUT / 2.0 + ONEWAY_LINK_TARGET_OFFSET
		);
		return;
	}
	let line_mesh = Rectangle::from_size(Vec2::new(line_length, CYCLE_LINK_WIDTH)).mesh();
	let line_mesh = meshes.add(line_mesh);
	let tip_mesh = Capsule2d::new(CYCLE_LINK_WIDTH / 2.0, ONEWAY_LINK_TIP_LENGTH)
		.mesh()
		.resolution(8);
	let tip_mesh = meshes.add(tip_mesh);
	let dir_a_to_b = (b - a).normalize();
	let rotation = Quat::from_rotation_arc_2d(Vec2::X, dir_a_to_b);
	let main_tip_rotation = rotation * Quat::from_rotation_z(PI / 2.0);
	let relative_tip_rotation = Quat::from_rotation_z(ONEWAY_LINK_TIP_ANGLE);
	let line_center_distance_from_a = line_length / 2.0 + CYCLE_LINK_END_CUT;
	let line_center_position = dir_a_to_b * line_center_distance_from_a;
	let tip_distance_from_a = d - ONEWAY_LINK_TARGET_OFFSET;
	let tip_position = dir_a_to_b * tip_distance_from_a;
	let tip_inner_transform = Transform::from_translation(Vec3::Y * ONEWAY_LINK_TIP_LENGTH / 2.0);
	children.spawn((
		Mesh2d(line_mesh),
		Transform::from_rotation(rotation)
			.with_translation(line_center_position.extend(layers::CYCLE_LINKS)),
		MeshMaterial2d(material.clone_weak()),
	));
	children.spawn((
		Mesh2d(tip_mesh.clone()),
		Transform::from_rotation(main_tip_rotation * relative_tip_rotation)
			.with_translation(tip_position.extend(layers::CYCLE_LINKS))
			.mul_transform(tip_inner_transform),
		MeshMaterial2d(material.clone_weak()),
	));
	children.spawn((
		Mesh2d(tip_mesh),
		Transform::from_rotation(main_tip_rotation * relative_tip_rotation.inverse())
			.with_translation(tip_position.extend(layers::CYCLE_LINKS))
			.mul_transform(tip_inner_transform),
		MeshMaterial2d(material.clone_weak()),
	));
}

fn create_thing_sprites(
	mut commands: Commands,
	query: Query<(Entity, &ThingData), Added<ThingData>>,
	palette: Res<ThingPalette>,
	sprites: Res<HandleMap<ImageKey>>,
) {
	for (id, thing) in &query {
		let (color, anchor, z_depth) = match thing {
			ThingData::Object(ObjectData::Player) => (
				palette.player,
				Anchor::Custom(PLAYER_FLAG_SPRITE_ANCHOR),
				layers::OBJECT_SPRITES,
			),
			ThingData::Object(ObjectData::Box(_)) => {
				(palette.box_base, default(), layers::OBJECT_SPRITES)
			}
			ThingData::Glyph(GlyphData::Flag) => (
				palette.goal_closed,
				Anchor::Custom(PLAYER_FLAG_SPRITE_ANCHOR),
				layers::GLYPH_SPRITES,
			),
			ThingData::Glyph(GlyphData::Button(_)) => {
				(palette.button_base, default(), layers::GLYPH_SPRITES)
			}
		};
		commands.entity(id).with_children(|children| {
			children.spawn((
				Sprite {
					image: sprites[&ImageKey::Object(ThingType::from(*thing))].clone_weak(),
					custom_size: Some(SPRITE_SIZE),
					color,
					anchor,
					..default()
				},
				Transform::from_translation(Vec3::Z * z_depth),
			));
		});
	}
}

fn create_box_color_markers(
	mut commands: Commands,
	sprites: Res<HandleMap<ImageKey>>,
	atlas_layout: Res<BoxColorSpriteAtlasLayout>,
	palette: Res<ThingPalette>,
	query: Query<(Entity, &LogicalColor), (With<Box>, Added<LogicalColor>)>,
) {
	let atlas = &sprites[&ImageKey::BoxSpriteAtlas];
	for (id, color) in &query {
		commands.entity(id).with_children(|children| {
			create_logical_color_sprite(
				children,
				*color,
				palette.box_base,
				atlas.clone_weak(),
				atlas_layout.0.clone_weak(),
				Transform::from_translation(Vec3::Z * layers::BOX_COLOR_SPRITES),
			);
		});
	}
}

fn create_button_color_markers(
	mut commands: Commands,
	sprites: Res<HandleMap<ImageKey>>,
	atlas_layout: Res<BoxColorSpriteAtlasLayout>,
	meshes: Res<GameObjectMeshes>,
	materials: Res<GameObjectMaterials>,
	palette: Res<ThingPalette>,
	query: Query<
		(Entity, &LogicalColor, &ButtonColorLabelAppearence),
		(With<BoxSlot>, Added<LogicalColor>),
	>,
) {
	let atlas = &sprites[&ImageKey::BoxSpriteAtlas];
	for (id, color, label_appearence) in &query {
		commands.entity(id).with_children(|children| {
			let label_mesh = if label_appearence.has_arrow_tip {
				meshes.arrow_labels.clone_weak()
			} else {
				meshes.square_labels.clone_weak()
			};
			let (translation, label_rotation, sprite_rotation) =
				get_button_color_label_placement(label_appearence);

			children.spawn((
				MeshMaterial2d(materials.colored_button_labels.clone_weak()),
				Mesh2d(label_mesh),
				Transform::from_translation(translation.extend(layers::BUTTON_COLOR_LABELS))
					.with_rotation(Quat::from_rotation_z(label_rotation)),
			));

			create_logical_color_sprite(
				children,
				*color,
				palette.button_base,
				atlas.clone_weak(),
				atlas_layout.0.clone_weak(),
				Transform::from_translation(translation.extend(layers::BUTTON_COLOR_SPRITES))
					.with_rotation(Quat::from_rotation_z(sprite_rotation)),
			);
		});
	}
}

fn create_logical_color_sprite(
	children: &mut ChildBuilder,
	logical_color: LogicalColor,
	sprite_color: Color,
	atlas: Handle<Image>,
	atlas_layout: Handle<TextureAtlasLayout>,
	transform: Transform,
) {
	// Shorthand for spawning a color sprite (more than one may be needed for numeric colors)
	let mut spawn_sprite = |index, x_offset, x_scale| {
		let transform = transform.mul_transform(
			Transform::from_translation(Vec3::X * x_offset).with_scale(Vec3::ONE.with_x(x_scale)),
		);
		children.spawn((
			transform,
			Sprite {
				custom_size: Some(COLOR_SPRITE_SIZE),
				image: atlas.clone_weak(),
				color: sprite_color,
				texture_atlas: Some(TextureAtlas {
					layout: atlas_layout.clone_weak(),
					index,
				}),
				..default()
			},
		));
	};

	if logical_color.is_pictogram {
		let sprite_index = logical_color.color_index + BOX_COLOR_SPRITE_PICTOGRAM_OFFSET;
		spawn_sprite(sprite_index, 0.0, 1.0);
	} else {
		let index_str = logical_color.color_index.to_string();
		// Calculate the width of the written number. Digit 1 needs less space than the others.
		let number_width = index_str
			.chars()
			.map(|c| {
				if c == '1' {
					DIGIT_ONE_SPRITE_WIDTH + DIGIT_SPRITE_SPACING
				} else {
					DIGIT_SPRITE_WIDTH + DIGIT_SPRITE_SPACING
				}
			})
			.sum::<f32>()
			- DIGIT_SPRITE_SPACING;
		let (mut width_progress, number_width) = if number_width < 1.0 {
			// Sprites should never grow in width, so cap the width at one
			((1.0 - number_width) / 2.0, 1.0)
		} else {
			(0.0, number_width)
		};
		for digit in index_str.chars() {
			let digit = digit
				.to_digit(10)
				.expect("String representation of a number should only be digits");
			let current_width = if digit == 1 {
				DIGIT_ONE_SPRITE_WIDTH
			} else {
				DIGIT_SPRITE_WIDTH
			};
			let relative_offset = (width_progress + current_width / 2.0) / number_width - 0.5;
			width_progress += current_width + DIGIT_SPRITE_SPACING;
			spawn_sprite(
				digit as usize,
				relative_offset * COLOR_SPRITE_SIZE.x,
				1.0 / number_width,
			);
		}
	}
}

/// Calculates the actual placement for a button color label
/// ## Return Value
/// The position of the center of the label, rotation of the label marker
/// and rotation of the color sprite
fn get_button_color_label_placement(style: &ButtonColorLabelAppearence) -> (Vec2, f32, f32) {
	use color_labels::*;
	use ButtonColorLabelPosition::*;
	let secondary_offset = if style.has_arrow_tip {
		OFFSET_SECONDARY_ARROW
	} else {
		OFFSET_SECONDARY_SQUARE
	};
	match style.position {
		Inside => (Vec2::Y * CENTER_Y_OFFSET, 0.0, 0.0),
		AnglePlaced(angle) | AngleRotated(angle) => {
			// Clamp angle to the [0, 2pi] range
			let angle = angle.rem_euclid(TAU);
			let (relative_position, quadrant_rotation) = match angle / PI {
				// Above
				0.0..0.25 | 1.75..=2.0 => (
					Vec2::new(angle.tan() * secondary_offset, secondary_offset),
					PI,
				),
				// Right
				0.25..0.75 => (
					Vec2::new(
						secondary_offset,
						-(angle - PI / 2.0).tan() * secondary_offset,
					),
					PI / 2.0,
				),
				// Below
				0.75..1.25 => (
					Vec2::new(-angle.tan() * secondary_offset, -secondary_offset),
					0.0,
				),
				// Left
				1.25..1.75 => (
					Vec2::new(
						-secondary_offset,
						(angle - PI / 2.0).tan() * secondary_offset,
					),
					-PI / 2.0,
				),
				_ => unreachable!("The angle should be clamped to [0, 2pi]"),
			};
			let position = relative_position + Vec2::Y * CENTER_Y_OFFSET;
			// Rotated placements should propagate any rotation angle
			if matches!(style.position, AngleRotated(_)) {
				let label_rotation = PI - angle;
				let sprite_rotation = PI - angle - quadrant_rotation;
				// If we have a rotated placement near a corner,
				// we bring it closer to the box to avoid a gap
				let max_distance = if style.has_arrow_tip {
					MAX_ROTATED_DISPLACEMENT_ARROW
				} else {
					MAX_ROTATED_DISPLACEMENT_SQUARE
				};
				let cut_position = if relative_position.length_squared() >= max_distance.powi(2) {
					Vec2::from_angle(PI / 2.0 - angle) * max_distance + Vec2::Y * CENTER_Y_OFFSET
				} else {
					position
				};
				(cut_position, label_rotation, sprite_rotation)
			} else {
				(position, quadrant_rotation, 0.0)
			}
		}
	}
}
