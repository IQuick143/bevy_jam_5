use super::{
	components::*, inputs::CycleInteraction, logic_relay::*, prelude::*, spawn::LastLevelSessionId,
};
use crate::{
	assets::{HandleMap, ImageKey},
	graphics::{
		primitives::{RoundedPentagonArrow, RoundedRectangle},
		*,
	},
	ui::hover::{self, Hoverable},
	AppSet,
};
use bevy::{
	color::palettes,
	math::bounding::Aabb2d,
	platform::collections::{HashMap, HashSet},
	sprite::Anchor,
};

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<GameObjectMaterials>()
		.init_resource::<GameObjectMeshes>()
		.init_resource::<ThingPalette>()
		.add_systems(
			Update,
			(
				goal_unlock_animation_system
					.run_if(resource_exists_and_changed::<LevelCompletionConditions>),
				(
					button_trigger_animation_system,
					cycle_center_turnability_visuals_update_system
						.before(cycle_center_interaction_visuals_update_system),
				)
					.run_if(on_message::<GameLayoutChanged>),
				(
					marker_despawn_system.run_if(on_message::<RotateCycleGroup>),
					cycle_blocked_marker_system.run_if(on_message::<TurnBlockedByGroupConflict>),
				)
					.chain(),
				cycle_center_interaction_visuals_update_system
					.run_if(cycle_interaction_visuals_changed),
			)
				.in_set(AppSet::UpdateVisuals),
		);
}

/// References to entities that make up the visualization of a cycle's ring
#[derive(Component, Clone, Debug, Reflect)]
pub struct CycleRingVisualEntities {
	/// The cycle ring
	pub ring: Entity,
	/// The cycle ring outline
	pub outline: Entity,
}

/// References to entities that make up the visualization of a cycle's center
#[derive(Component, Clone, Debug, Reflect)]
pub struct CycleCenterVisualEntities {
	/// The sprite at the center of the cycle
	pub sprite: Entity,
	/// The arrow that shows up at the center of the cycle
	pub arrow: Entity,
}

/// Reference to entity that indicates a cycle's hitbox
#[derive(Component, Clone, Debug, Reflect, Deref, DerefMut)]
pub struct CycleHitboxVisualEntity(pub Entity);

/// Refernces to entities that make up the visualization of a vertex
#[derive(Component, Clone, Debug, Reflect)]
pub struct VertexVisualEntities {
	/// The vertex node
	pub node: Entity,
	/// The vertex node outline
	pub outline: Entity,
}

/// Contains handles to the materials used to render game objects that are visualized by meshes
#[derive(Resource, Debug, Clone, Reflect)]
pub struct GameObjectMaterials {
	/// Material for cycle rings and vertex dots
	pub cycle_rings_ready: Handle<ColorMaterial>,
	/// Material for rings of cycles that are currently selected
	pub cycle_rings_select: Handle<ColorMaterial>,
	/// Material for rings of cycles that are not selectable
	pub cycle_rings_disabled: Handle<ColorMaterial>,
	/// Material for outlines of cycle rings and vertex dots
	pub cycle_ring_outlines: Handle<ColorMaterial>,
	/// Material for outlines of cycle rings and vertices that are not selectable
	pub cycle_ring_outlines_disabled: Handle<ColorMaterial>,
	/// Material for shadows/fill of cycles that are currently selected
	pub cycle_hitboxes: Handle<ColorMaterial>,
	/// Material for lines that represent links between cycles
	pub link_lines: Handle<ColorMaterial>,
	/// Material for labels that show logical color of buttons
	pub colored_button_labels: Handle<ColorMaterial>,
	/// Material for detectors
	pub detector: Handle<ColorMaterial>,
	/// Material for walls
	pub wall: Handle<ColorMaterial>,
}

impl FromWorld for GameObjectMaterials {
	fn from_world(world: &mut World) -> Self {
		let mut materials = world.resource_mut::<Assets<ColorMaterial>>();

		let cycle_rings_ready = materials.add(ColorMaterial {
			color: palettes::tailwind::SLATE_200.into(),
			..default()
		});
		let cycle_rings_select = materials.add(ColorMaterial {
			color: palettes::tailwind::SLATE_400.into(),
			..default()
		});
		let cycle_rings_disabled = materials.add(ColorMaterial {
			color: palettes::tailwind::SLATE_100.into(),
			..default()
		});
		let cycle_ring_outlines = materials.add(ColorMaterial {
			color: palettes::tailwind::SLATE_700.into(),
			..default()
		});
		let cycle_ring_outlines_disabled = materials.add(ColorMaterial {
			// Roughly Tailwind Slate-350
			color: Srgba::hex("94A3B8").unwrap().into(),
			..default()
		});
		let cycle_hitboxes = materials.add(ColorMaterial {
			color: Srgba::hex("A3B7D1").unwrap().with_alpha(0.3).into(),
			..default()
		});
		let link_lines = materials.add(ColorMaterial {
			color: palettes::tailwind::SLATE_300.into(),
			..default()
		});
		let colored_button_labels = materials.add(ColorMaterial {
			color: Color::BLACK,
			..default()
		});
		// TODO: Textures
		let detector = materials.add(ColorMaterial {
			color: palettes::css::ORANGE.into(),
			..default()
		});
		let wall = materials.add(ColorMaterial {
			color: palettes::css::GRAY.into(),
			..default()
		});

		Self {
			cycle_rings_ready,
			cycle_rings_select,
			cycle_rings_disabled,
			cycle_ring_outlines,
			cycle_ring_outlines_disabled,
			cycle_hitboxes,
			link_lines,
			colored_button_labels,
			detector,
			wall,
		}
	}
}

/// Contains handles to meshes that are commonly used to render game objects
#[derive(Resource, Debug, Clone, Reflect)]
pub struct GameObjectMeshes {
	/// Mesh for vertex nodes
	pub vertices: Handle<Mesh>,
	/// Mesh for outlines of vertex nodes
	pub vertex_outlines: Handle<Mesh>,
	/// Mesh for square labels that show logical colors of buttons
	pub square_labels: Handle<Mesh>,
	/// Mesh for arrow-like labels that show logical colors of buttons
	pub arrow_labels: Handle<Mesh>,
	/// Mesh for tips of arrows that represent one-way links
	pub one_way_link_tips: Handle<Mesh>,
	/// Mesh for back side of tips of arrows for one-way links
	/// with numeric multiplicity labels
	pub one_way_link_backheads: Handle<Mesh>,
	/// Mesh for a thin rectangle, used for detectors and walls
	pub detector_rectangle: Handle<Mesh>,
}

impl FromWorld for GameObjectMeshes {
	fn from_world(world: &mut World) -> Self {
		let mut meshes = world.resource_mut::<Assets<Mesh>>();

		let vertices = meshes.add(Circle::new(NODE_RADIUS).mesh());
		let vertex_outlines = meshes.add(Circle::new(NODE_RADIUS + RING_OUTLINE_WIDTH).mesh());
		let square_labels = meshes.add(
			RoundedRectangle::from(Rectangle::from_length(color_labels::SIZE))
				.corner_radius(color_labels::CORNER_RADIUS)
				.mesh()
				.resolution(color_labels::MESH_RESOLUTION),
		);
		let arrow_labels = meshes.add(
			RoundedPentagonArrow::from(Rectangle::from_length(color_labels::SIZE))
				.corner_radius(color_labels::CORNER_RADIUS)
				.tip_length(color_labels::ARROW_TIP_LENGTH)
				.mesh()
				.resolution(color_labels::MESH_RESOLUTION),
		);
		let one_way_link_tips = meshes.add(
			Capsule2d::new(CYCLE_LINK_WIDTH / 2.0, ONEWAY_LINK_TIP_LENGTH)
				.mesh()
				.resolution(ONEWAY_LINK_TIP_RESOLUTION),
		);
		let one_way_link_backheads = meshes.add(
			Capsule2d::new(CYCLE_LINK_WIDTH / 2.0, ONEWAY_MULTILINK_BACKHEAD_LENGTH)
				.mesh()
				.resolution(ONEWAY_LINK_TIP_RESOLUTION),
		);
		let detector_rectangle =
			meshes.add(Rectangle::from_size(Vec2::new(CYCLE_LINK_WIDTH, SPRITE_LENGTH)).mesh());

		Self {
			vertices,
			vertex_outlines,
			square_labels,
			arrow_labels,
			one_way_link_tips,
			one_way_link_backheads,
			detector_rectangle,
		}
	}
}

/// Contains colors used for rendering objects and glyphs
#[derive(Resource, Debug, Clone, Reflect)]
pub struct ThingPalette {
	pub box_base: Color,
	pub button_base: Color,
	pub button_trigger: Color,
	pub player: Color,
	pub goal_closed: Color,
	pub goal_open: Color,
	pub cycle_disabled: Color,
	pub cycle_ready: Color,
	pub cycle_trigger: Color,
	pub link_multiplicity_label: Color,
	pub inverted_link_multiplicity_label: Color,
	pub warning_sign: Color,
	pub checkmark: Color,
}

impl Default for ThingPalette {
	fn default() -> Self {
		use palettes::tailwind as p;
		Self {
			box_base: p::ORANGE_200.into(),
			button_base: Srgba::hex("CC5151").unwrap().into(),
			button_trigger: p::GREEN_300.into(),
			player: p::SLATE_200.into(),
			goal_closed: p::SLATE_100.into(),
			goal_open: p::GREEN_500.into(),
			cycle_disabled: p::SLATE_300.into(),
			cycle_ready: p::SLATE_300.into(),
			cycle_trigger: p::SLATE_400.into(),
			link_multiplicity_label: p::SLATE_300.into(),
			inverted_link_multiplicity_label: Srgba::hex("F29FA7").unwrap().into(),
			warning_sign: p::RED_400.into(),
			checkmark: p::GREEN_300.into(),
		}
	}
}

/// Enumerates the possible presentable states of a cycle
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
enum CycleStatus {
	/// The cycle is not eligible for an interaction
	#[default]
	Disabled,
	/// The cycle is eligible for an interaction
	Ready,
	/// The cycle is being interacted with
	Selected,
}

fn goal_unlock_animation_system(
	mut sprites_q: Query<&mut Sprite>,
	flags_q: Query<&Children, With<Goal>>,
	palette: Res<ThingPalette>,
	completion: Res<LevelCompletionConditions>,
) {
	let color = if completion.is_goal_unlocked() {
		palette.goal_open
	} else {
		palette.goal_closed
	};
	for children in &flags_q {
		for id in children {
			if let Ok(mut sprite) = sprites_q.get_mut(*id) {
				sprite.color = color;
			}
		}
	}
}

fn button_trigger_animation_system(
	mut sprites_q: Query<&mut Sprite>,
	buttons_q: Query<(&Children, &IsTriggered), (With<SokoButton>, Changed<IsTriggered>)>,
	palette: Res<ThingPalette>,
) {
	for (children, is_triggered) in &buttons_q {
		let color = if is_triggered.0 {
			palette.button_trigger
		} else {
			palette.button_base
		};

		for id in children {
			if let Ok(mut sprite) = sprites_q.get_mut(*id) {
				sprite.color = color;
			}
		}
	}
}

fn cycle_center_turnability_visuals_update_system(
	cycles_q: Query<(&ComputedCycleTurnability, &CycleCenterVisualEntities)>,
	mut arrows_q: Query<&mut Visibility>,
) {
	for (is_turnable, visuals) in &cycles_q {
		let Ok(mut arrow_visibility) = arrows_q.get_mut(visuals.arrow) else {
			log::warn!("Cycle arrow sprite does not have Visibility component");
			continue;
		};
		if is_turnable.0 {
			*arrow_visibility = default();
		} else {
			*arrow_visibility = Visibility::Hidden;
		}
	}
}

fn cycle_interaction_visuals_changed(
	query: Query<(), Or<(Changed<CycleInteraction>, Changed<ComputedCycleTurnability>)>>,
) -> bool {
	!query.is_empty()
}

fn cycle_center_interaction_visuals_update_system(
	cycles_q: Query<(
		&CycleInteraction,
		&Cycle,
		&ComputedCycleTurnability,
		Option<&CycleCenterVisualEntities>,
		Option<&CycleRingVisualEntities>,
		Option<&CycleHitboxVisualEntity>,
	)>,
	entity_index: Res<GameStateEcsIndex>,
	level: PlayingLevelData,
	vertices_q: Query<&VertexVisualEntities>,
	mut sprites_q: Query<&mut Sprite>,
	mut meshes_q: Query<(&mut Transform, &mut MeshMaterial2d<ColorMaterial>)>,
	mut visibility_q: Query<&mut Visibility>,
	palette: Res<ThingPalette>,
	materials: Res<GameObjectMaterials>,
) {
	let Ok(level) = level.get() else {
		log::error!("Non-existent level asset being referenced.");
		return;
	};

	let selected_groups = cycles_q
		.iter()
		.filter(|(interaction, ..)| **interaction != CycleInteraction::None)
		.map(|(_, cycle, ..)| cycle.group_id)
		.collect::<HashSet<_>>();

	let mut meshes_to_repaint = HashMap::<Entity, CycleStatus>::default();
	let mut outlines_to_repaint = HashMap::<Entity, CycleStatus>::default();
	let mut sprites_to_repaint = HashMap::<Entity, CycleStatus>::default();
	let mut hitboxes_to_repaint = HashMap::<Entity, bool>::default();

	for data in &cycles_q {
		let (interaction, cycle, is_turnable, center_visuals, ring_visuals, hitbox_visuals) = data;
		let is_selected = selected_groups.contains(&cycle.group_id);
		let is_directly_selected = *interaction != CycleInteraction::None;

		let cycle_status = if is_selected {
			CycleStatus::Selected
		} else if is_turnable.0 {
			CycleStatus::Ready
		} else {
			CycleStatus::Disabled
		};

		if let Some(visuals) = center_visuals {
			let sprite_status = sprites_to_repaint.entry(visuals.sprite).or_default();
			*sprite_status = (*sprite_status).max(cycle_status);
		}
		if let Some(hitbox) = hitbox_visuals {
			let sprite_status = hitboxes_to_repaint.entry(**hitbox).or_default();
			*sprite_status = *sprite_status || is_directly_selected;
		}

		level.cycles[cycle.id]
			.vertex_indices
			.iter()
			.filter_map(|id| {
				vertices_q
					.get(entity_index.vertices[*id])
					.inspect_err(|e| log::warn!("CycleVertices refers to a non-vertex entity: {e}"))
					.ok()
					.map(|visuals| (visuals.node, visuals.outline))
			})
			.chain(
				ring_visuals
					.into_iter()
					.map(|visuals| (visuals.ring, visuals.outline)),
			)
			.for_each(|(body, outline)| {
				let mesh_status = meshes_to_repaint.entry(body).or_default();
				*mesh_status = (*mesh_status).max(cycle_status);
				let mesh_status = outlines_to_repaint.entry(outline).or_default();
				*mesh_status = (*mesh_status).max(cycle_status);
			});
	}

	for (id, status) in sprites_to_repaint {
		let Ok(mut sprite) = sprites_q.get_mut(id) else {
			log::warn!("Cycle sprite entity does not have Sprite component");
			continue;
		};
		sprite.color = match status {
			CycleStatus::Disabled => palette.cycle_disabled,
			CycleStatus::Ready => palette.cycle_ready,
			CycleStatus::Selected => palette.cycle_trigger,
		};
	}

	for (id, status) in meshes_to_repaint {
		let Ok((mut transform, mut material)) = meshes_q.get_mut(id) else {
			log::warn!(
				"Vertex or cycle mesh entity does not have a Handle<ColorMaterial> component"
			);
			continue;
		};
		match status {
			CycleStatus::Disabled => {
				transform.translation.z = layers::DISABLED_CYCLE_RINGS;
				material.0 = materials.cycle_rings_disabled.clone();
			}
			CycleStatus::Ready => {
				transform.translation.z = layers::CYCLE_RINGS;
				material.0 = materials.cycle_rings_ready.clone();
			}
			CycleStatus::Selected => {
				transform.translation.z = layers::ACTIVE_CYCLE_RINGS;
				material.0 = materials.cycle_rings_select.clone();
			}
		}
	}

	for (id, status) in outlines_to_repaint {
		let Ok((mut transform, mut material)) = meshes_q.get_mut(id) else {
			log::warn!(
				"Vertex or cycle mesh entity does not have a Handle<ColorMaterial> component"
			);
			continue;
		};
		match status {
			CycleStatus::Disabled => {
				transform.translation.z = layers::DISABLED_CYCLE_RING_OUTLINES;
				material.0 = materials.cycle_ring_outlines_disabled.clone();
			}
			CycleStatus::Ready => {
				transform.translation.z = layers::CYCLE_RING_OUTLINES;
				material.0 = materials.cycle_ring_outlines.clone();
			}
			CycleStatus::Selected => {
				transform.translation.z = layers::ACTIVE_CYCLE_RING_OUTLINES;
				material.0 = materials.cycle_ring_outlines.clone();
			}
		}
	}

	for (id, is_directly_selected) in hitboxes_to_repaint {
		let Ok(mut visibility) = visibility_q.get_mut(id) else {
			log::warn!("Cycle hitbox mesh entity does not have a Visibility component");
			continue;
		};
		if is_directly_selected {
			*visibility = default();
		} else {
			*visibility = Visibility::Hidden;
		}
	}
}

/// Despawns entities with [`TemporaryMarker`] during the start of a turn.
fn marker_despawn_system(
	mut commands: Commands,
	marker_entities: Query<Entity, With<TemporaryMarker>>,
) {
	for entity in marker_entities.iter() {
		commands.entity(entity).despawn();
	}
}

fn cycle_blocked_marker_system(
	mut commands: Commands,
	mut events: MessageReader<TurnBlockedByGroupConflict>,
	vertices_q: Query<&Transform, With<Vertex>>,
	entity_index: Res<GameStateEcsIndex>,
	level: PlayingLevelData,
	images: Res<HandleMap<ImageKey>>,
	session: Res<LastLevelSessionId>,
	palette: Res<ThingPalette>,
) {
	let Ok(level) = level.get() else {
		log::error!("Non-existent level asset being referenced.");
		return;
	};

	let mut marked_vertices = HashSet::<_>::default();
	for event in events.read() {
		let Some((_, _, conflicting_vertices)) = level.forbidden_group_pairs.get(event.0) else {
			log::error!("Incorrect level data!?");
			return;
		};
		for &vert in conflicting_vertices.iter() {
			marked_vertices.insert(vert);
		}
	}

	for &vertex in marked_vertices.iter() {
		let Some(vertex_transform) = entity_index
			.vertices
			.get(vertex)
			.and_then(|entity| vertices_q.get(*entity).ok())
		else {
			log::warn!("Nonexistent vertex!");
			continue;
		};
		let size = Vec2::new(SPRITE_LENGTH / 3.0, SPRITE_LENGTH);
		commands.spawn((
			Sprite {
				image: images[&ImageKey::InGameWarning].clone(),
				custom_size: Some(size),
				color: palette.warning_sign,
				..default()
			},
			Anchor::BOTTOM_CENTER, // TODO: Check if this is correct behaviour mimicking
			Transform::from_translation(
				vertex_transform.translation + Vec3::Y * SPRITE_LENGTH * 0.25,
			),
			TemporaryMarker,
			Hoverable {
				hover_text: hover::BLOCKADE_WARNING,
				hover_bounding_circle: None,
				hover_bounding_box: Some(Aabb2d::new(
					Vec2::new(0.0, SPRITE_LENGTH / 2.0),
					size / 2.0,
				)),
			},
			session.get_session(),
		));
	}
}
