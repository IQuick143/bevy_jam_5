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
pub struct LastLevelSessionId(LevelSessionId);

impl LastLevelSessionId {
	pub fn get_session(&self) -> LevelSessionId {
		self.0
	}
}

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
			let source_center_pos = level.cycles[link.source_cycle]
				.center_sprite_appearence
				.0
				.unwrap_or_default();
			commands
				.get_entity(cycles[link.source_cycle])
				.expect("The entity has just been spawned")
				.with_children(|children| {
					children.spawn((
						LinkTargetCycle(target_cycle),
						link.direction,
						Transform::from_translation(source_center_pos.extend(0.0)),
						Visibility::default(),
					));
				});
		}
		for link in &level.declared_one_way_links {
			let target_cycle = commands
				.get_entity(cycles[link.dest_cycle])
				.expect("The entity has just been spawned")
				.id();
			let source_center_pos = level.cycles[link.source]
				.center_sprite_appearence
				.0
				.unwrap_or_default();
			commands
				.get_entity(cycles[link.source])
				.expect("The entity has just been spawned")
				.with_children(|children| {
					children.spawn((
						LinkTargetCycle(target_cycle),
						link.direction,
						LinkMultiplicity(link.multiplicity),
						Transform::from_translation(source_center_pos.extend(0.0)),
						Visibility::default(),
					));
				});
		}

		// Spawn cycle list
		if level.is_valid {
			commands.insert_resource(CycleEntities(cycles));
			commands.insert_resource(VertexEntities(vertices));
			commands.insert_resource(LevelHandle(
				levels
					.get_strong_handle(level_handle.id())
					.expect("I expect you to work."),
			));
		} else {
			// Ensure the [`LevelHandle`] is not around for an invalid level.
			commands.remove_resource::<LevelHandle>();
			// Just to be sure, remove these resources as well.
			commands.remove_resource::<CycleEntities>();
			commands.remove_resource::<VertexEntities>();
		}
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
					SokoBox,
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
					SokoBox,
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
					SokoButton,
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
					SokoButton,
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
	standard_meshes: Res<GameObjectMeshes>,
	digit_atlas: Res<DigitAtlas>,
	palette: Res<ThingPalette>,
	links_q: Query<
		(
			Entity,
			&ChildOf,
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
		let a = get_link_endpoint(source.parent, cycles_q.reborrow());
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
					standard_meshes.one_way_link_tips.clone_weak(),
					standard_meshes.one_way_link_backheads.clone_weak(),
					&digit_atlas,
					palette.link_multiplicity_label,
					palette.inverted_link_multiplicity_label,
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
	children: &mut ChildSpawnerCommands,
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
	children: &mut ChildSpawnerCommands,
	a: Vec2,
	b: Vec2,
	direction: LinkedCycleDirection,
	multiplicity: u64,
	meshes: &mut Assets<Mesh>,
	material: Handle<ColorMaterial>,
	tip_mesh: Handle<Mesh>,
	backhead_mesh: Handle<Mesh>,
	digit_atlas: &DigitAtlas,
	label_color: Color,
	invert_label_color: Color,
) {
	if multiplicity == 0 {
		log::warn!("Skipped drawing a cycle link with zero multiplicity");
		return;
	}
	// Distance between cycle centers
	let d = a.distance(b);
	// Length of the whole arrow, from base to tip
	let arrow_length = d - CYCLE_LINK_END_CUT - ONEWAY_LINK_TARGET_OFFSET;

	// Whether multiplicity should be indicated with a number
	let use_numeric =
		multiplicity > ONEWAY_MULTILINK_MAX_COUNT || direction == LinkedCycleDirection::Inverse;
	let digits;
	// Whether the multiplicity label will be flipped
	// so as to not end up upside down
	let flip_multiplicity_label = Vec2::X.dot(b - a) < 0.0;
	// [`ONEWAY_MULTILINK_TEXT_AFTER`], corrected for kerning
	let padding_after;
	// How many tips the arrow should have (`--->>>`)
	let tip_count;
	// Length of the line that makes up the main body of the arrow
	// Only goes up to the first tip in a multilink arrow
	let line_length;

	if use_numeric {
		let mut digits_string = String::new();
		// Add a minus sign for inverted links
		if direction == LinkedCycleDirection::Inverse {
			digits_string.push('-');
		}
		// Add the actual number
		digits_string.push_str(&multiplicity.to_string());
		// Append a dot so we can tell if it's a 6 or a 9
		if is_number_non_orientable(&digits_string) {
			digits_string.push('.');
		}
		// Save the string
		digits = digits_string;

		// Add kerning to the arrow tip
		padding_after = if !flip_multiplicity_label && digits.ends_with('.') {
			ONEWAY_MULTILINK_TEXT_AFTER - ONEWAY_MULTILINK_DOT_KERNING
		} else if flip_multiplicity_label && digits.starts_with('-') {
			ONEWAY_MULTILINK_TEXT_AFTER - ONEWAY_MULTILINK_MINUS_KERNING
		} else {
			ONEWAY_MULTILINK_TEXT_AFTER
		};

		let text_width = get_number_typeset_width(&digits) * ONEWAY_MULTILINK_DIGIT_SIZE.x;
		tip_count = 1;
		line_length = arrow_length - text_width - ONEWAY_MULTILINK_TEXT_BEFORE - padding_after;
	} else {
		tip_count = multiplicity;
		line_length = arrow_length - ONEWAY_MULTILINK_TIP_SPACING * (tip_count - 1) as f32;
		// Assign to this so we can use it later
		digits = String::new();
		padding_after = 0.0;
	};

	if line_length <= 0.0 {
		// The link cannot be rendered if the cycles are too close
		log::warn!(
			"Skipped drawing a cycle link because the cycles are {d} units apart, need at least {}",
			d - line_length
		);
		return;
	}

	let line_mesh = Rectangle::from_size(Vec2::new(line_length, CYCLE_LINK_WIDTH)).mesh();
	let line_mesh = meshes.add(line_mesh);
	let dir_a_to_b = (b - a).normalize();
	let rotation = Quat::from_rotation_arc_2d(Vec2::X, dir_a_to_b);
	let line_center_distance_from_a = line_length / 2.0 + CYCLE_LINK_END_CUT;
	let line_center_position = dir_a_to_b * line_center_distance_from_a;
	children.spawn((
		Mesh2d(line_mesh),
		Transform::from_rotation(rotation)
			.with_translation(line_center_position.extend(layers::CYCLE_LINKS)),
		MeshMaterial2d(material.clone_weak()),
	));

	// Rotates a tip arm to be parallel to arrow body
	// (arrow body rotation plus 90 degrees)
	let main_tip_rotation = rotation * Quat::from_rotation_z(PI / 2.0);
	// Rotation of the arrow tip to either side of the arrow body
	let relative_tip_rotation = Quat::from_rotation_z(ONEWAY_LINK_TIP_ANGLE);
	// Distance to the farthest tip in case of multiarrow
	let tip_distance_from_a = d - ONEWAY_LINK_TARGET_OFFSET;
	// Displace the tip's rotation center to its end so we can use it easier
	let tip_inner_transform = Transform::from_translation(Vec3::Y * ONEWAY_LINK_TIP_LENGTH / 2.0);
	for i in 0..tip_count {
		let this_tip_distance_from_a =
			tip_distance_from_a - ONEWAY_MULTILINK_TIP_SPACING * i as f32;
		let tip_position = dir_a_to_b * this_tip_distance_from_a;
		children.spawn((
			Mesh2d(tip_mesh.clone_weak()),
			Transform::from_rotation(main_tip_rotation * relative_tip_rotation)
				.with_translation(tip_position.extend(layers::CYCLE_LINKS))
				.mul_transform(tip_inner_transform),
			MeshMaterial2d(material.clone_weak()),
		));
		children.spawn((
			Mesh2d(tip_mesh.clone_weak()),
			Transform::from_rotation(main_tip_rotation * relative_tip_rotation.inverse())
				.with_translation(tip_position.extend(layers::CYCLE_LINKS))
				.mul_transform(tip_inner_transform),
			MeshMaterial2d(material.clone_weak()),
		));
	}

	if use_numeric {
		// Backhead
		let backhead_distance_from_a = CYCLE_LINK_END_CUT + line_length;
		let backhead_position = backhead_distance_from_a * dir_a_to_b;
		children.spawn((
			Mesh2d(backhead_mesh),
			Transform::from_rotation(rotation)
				.with_translation(backhead_position.extend(layers::CYCLE_LINKS)),
			MeshMaterial2d(material.clone_weak()),
		));

		// Use the color corresponding to the direction
		let color = match direction {
			LinkedCycleDirection::Coincident => label_color,
			LinkedCycleDirection::Inverse => invert_label_color,
		};
		// Distance to the caret, i.e. where the most signuficant digit starts
		let caret_distance_from_a;
		let label_rotation;
		// Flip the label if it would be upside down
		if !flip_multiplicity_label {
			caret_distance_from_a = backhead_distance_from_a + ONEWAY_MULTILINK_TEXT_BEFORE;
			label_rotation = rotation;
		} else {
			caret_distance_from_a = tip_distance_from_a - padding_after;
			label_rotation = rotation * Quat::from_rotation_z(PI);
		}
		let caret_position = caret_distance_from_a * dir_a_to_b;
		let caret_transform = Transform::from_rotation(label_rotation)
			.with_translation(caret_position.extend(layers::CYCLE_LINKS));
		typeset_number(
			&digits,
			children,
			caret_transform,
			digit_atlas,
			color,
			ONEWAY_MULTILINK_DIGIT_SIZE,
		);
	}
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
	sprite_atlas: Res<BoxColorSpriteAtlas>,
	digit_atlas: Res<DigitAtlas>,
	palette: Res<ThingPalette>,
	query: Query<(Entity, &LogicalColor), (With<SokoBox>, Added<LogicalColor>)>,
) {
	for (id, color) in &query {
		commands.entity(id).with_children(|children| {
			create_logical_color_sprite(
				children,
				*color,
				palette.box_base,
				&sprite_atlas,
				&digit_atlas,
				Transform::from_translation(Vec3::Z * layers::BOX_COLOR_SPRITES),
			);
		});
	}
}

fn create_button_color_markers(
	mut commands: Commands,
	sprite_atlas: Res<BoxColorSpriteAtlas>,
	digit_atlas: Res<DigitAtlas>,
	meshes: Res<GameObjectMeshes>,
	materials: Res<GameObjectMaterials>,
	palette: Res<ThingPalette>,
	query: Query<
		(Entity, &LogicalColor, &ButtonColorLabelAppearence),
		(With<SokoButton>, Added<LogicalColor>),
	>,
) {
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
				&sprite_atlas,
				&digit_atlas,
				Transform::from_translation(translation.extend(layers::BUTTON_COLOR_SPRITES))
					.with_rotation(Quat::from_rotation_z(sprite_rotation)),
			);
		});
	}
}

fn create_logical_color_sprite(
	children: &mut ChildSpawnerCommands,
	logical_color: LogicalColor,
	sprite_color: Color,
	sprite_atlas: &BoxColorSpriteAtlas,
	digit_atlas: &DigitAtlas,
	transform: Transform,
) {
	if logical_color.is_pictogram {
		children.spawn((
			transform,
			Sprite {
				custom_size: Some(COLOR_SPRITE_SIZE),
				image: sprite_atlas.image.clone_weak(),
				color: sprite_color,
				texture_atlas: Some(TextureAtlas {
					layout: sprite_atlas.layout.clone_weak(),
					index: logical_color.color_index,
				}),
				..default()
			},
		));
	} else {
		let index_str = logical_color.color_index.to_string();
		let number_width = get_number_typeset_width(&index_str);
		let (starting_caret_pos, number_width) = if number_width < 1.0 {
			// Sprites should never grow in width, so cap the width at one
			((1.0 - number_width) / 2.0, 1.0)
		} else {
			(0.0, number_width)
		};
		let base_sprite_size = Vec2::new(COLOR_SPRITE_SIZE.x / number_width, COLOR_SPRITE_SIZE.y);
		let start_offset = (starting_caret_pos - 0.5) * COLOR_SPRITE_SIZE.x;
		typeset_number(
			&index_str,
			children,
			transform * Transform::from_translation(Vec3::X * start_offset),
			digit_atlas,
			sprite_color,
			base_sprite_size,
		);
	}
}

/// Whether a digit string may require additional
/// indication of what way it should be read
fn is_number_non_orientable(digits: &str) -> bool {
	let mirrorred_chars = digits
		.chars()
		.rev()
		// Map pairs of characters at mirrorred positions
		.zip(digits.chars())
		// Only the first half of the string is relevant,
		// the second is guaranteed to be the same
		// Assume only ascii characters
		.take(digits.len().div_ceil(2));
	let mut looks_the_same_upside_down = true;
	for (a, b) in mirrorred_chars {
		match (a, b) {
			// If all characters match their mirror images,
			// the string is not necessarily non-orientable
			('6', '9') | ('9', '6') | ('8', '8') | ('0', '0') => {}
			// If non-orientable characters appear in some other
			// combination, the number may be non-orientable
			('0' | '6' | '8' | '9', '0' | '6' | '8' | '9') => looks_the_same_upside_down = false,
			// If any other character appears, the number
			// is automatically orientable
			_ => return false,
		}
	}
	!looks_the_same_upside_down
}

/// Calculates the width of a number typeset using digit sprites.
/// Width is relative to the size of a single sprite.
fn get_number_typeset_width(digits: &str) -> f32 {
	digits
		.chars()
		.map(|c| DigitAtlas::width_of(c).unwrap_or_default() + DIGIT_SPRITE_SPACING)
		.sum::<f32>()
		- DIGIT_SPRITE_SPACING
}

/// Constructs a number from digit sprites
/// ## Parameters
/// - `digits` - Digits of the number
/// - `children` - Child builder that receives the digit sprites
/// - `start_transform` - Transformation of the number,
///   anchored in the center-start of the number
/// - `atlas` - The sprite sheet
/// - `atlas_layout` - Layout for the sprite sheet
/// - `color` - Color of the sprites
/// - `digit_sprite_size` - Size of the sprite for each digit
fn typeset_number(
	digits: &str,
	children: &mut ChildSpawnerCommands,
	start_transform: Transform,
	digit_atlas: &DigitAtlas,
	color: Color,
	digit_sprite_size: Vec2,
) {
	// How far from `start_transform` the next digit should start,
	// in multiples of sprite size
	let mut caret_offset = 0.0;

	for digit in digits.chars() {
		let current_digit_width = DigitAtlas::width_of(digit)
			.expect("String representation of a number should only be valid characters");
		let sprite_index = DigitAtlas::sprite_index_of(digit)
			.expect("String representation of a number should only be valid characters");
		// Offset of the current digit from `start_transform`, measured
		// to the center of the digit, in multiples of sprite size
		let relative_offset = caret_offset + current_digit_width / 2.0;
		// Move the caret to the next digit
		caret_offset += current_digit_width + DIGIT_SPRITE_SPACING;
		// Transformation of the digit relative to `start_transform`
		let digit_transform =
			Transform::from_translation(Vec3::X * relative_offset * digit_sprite_size.x);
		children.spawn((
			start_transform.mul_transform(digit_transform),
			Sprite {
				custom_size: Some(digit_sprite_size),
				image: digit_atlas.image.clone_weak(),
				color,
				texture_atlas: Some(TextureAtlas {
					layout: digit_atlas.layout.clone_weak(),
					index: sprite_index,
				}),
				..default()
			},
		));
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
