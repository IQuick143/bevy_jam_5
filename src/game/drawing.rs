use super::{
	animation::PopupAnimation, components::*, inputs::CycleInteraction, logic_relay::*, prelude::*,
	spawn::LastLevelSessionId,
};
use crate::{
	assets::{HandleMap, ImageKey},
	drawing::*,
	graphics::*,
	ui::hover::{self, HoverHint, HoverHintBoundingRect, HoverPriority},
	AppSet,
};
use bevy::{
	ecs::system::SystemParam,
	math::bounding::Aabb2d,
	platform::collections::{HashMap, HashSet},
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			(
				goal_unlock_animation_system
					.run_if(resource_exists_and_changed::<LevelCompletionConditions>),
				(
					button_trigger_animation_system,
					cycle_center_turnability_visuals_update_system
						.before(cycle_center_interaction_visuals_update_system),
				)
					.run_if(on_message::<GameLayoutChanged>),
				cycle_center_interaction_visuals_update_system
					.run_if(cycle_interaction_visuals_changed),
			)
				.in_set(AppSet::UpdateVisuals),
			(
				cycle_blocked_marker_system.run_if(on_message::<TurnBlockedByGroupConflict>),
				wall_blocked_marker_system.run_if(on_message::<TurnBlockedByWallHit>),
				marker_popdown_system
					.run_if(on_message::<RotateCycleGroup>)
					.before(cycle_blocked_marker_system)
					.before(wall_blocked_marker_system),
				marker_despawn_system,
			)
				.after(AppSet::GameLogic)
				.before(AppSet::UpdateVisuals),
		),
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
	mut sprites_q: Query<&mut SpriteColorKey>,
	flags_q: Query<&Children, With<Goal>>,
	completion: Res<LevelCompletionConditions>,
) {
	let color_key = if completion.is_goal_unlocked() {
		ColorKey::GoalOpen
	} else {
		ColorKey::GoalClosed
	};
	for children in &flags_q {
		for id in children {
			if let Ok(mut sprite_key) = sprites_q.get_mut(*id) {
				**sprite_key = color_key;
			}
		}
	}
}

fn button_trigger_animation_system(
	mut sprites_q: Query<&mut SpriteColorKey>,
	buttons_q: Query<(&Children, &IsTriggered), (With<SokoButton>, Changed<IsTriggered>)>,
) {
	for (children, is_triggered) in &buttons_q {
		let color_key = if is_triggered.0 {
			ColorKey::ButtonTrigger
		} else {
			ColorKey::ButtonBase
		};

		for id in children {
			if let Ok(mut sprite_key) = sprites_q.get_mut(*id) {
				**sprite_key = color_key;
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
	mut sprites_q: Query<&mut SpriteColorKey>,
	mut meshes_q: Query<(&mut Transform, &mut MeshMaterial2d<ColorMaterial>)>,
	mut visibility_q: Query<&mut Visibility>,
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
		let Ok(mut sprite_key) = sprites_q.get_mut(id) else {
			log::warn!("Cycle sprite entity does not have Sprite component");
			continue;
		};
		let color_key = match status {
			CycleStatus::Disabled => ColorKey::CycleDisabled,
			CycleStatus::Ready => ColorKey::CycleReady,
			CycleStatus::Selected => ColorKey::CycleTrigger,
		};
		**sprite_key = color_key;
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
				material.0 = materials[&MaterialKey::CycleRingsDisabled].clone();
			}
			CycleStatus::Ready => {
				transform.translation.z = layers::CYCLE_RINGS;
				material.0 = materials[&MaterialKey::CycleRingsReady].clone();
			}
			CycleStatus::Selected => {
				transform.translation.z = layers::ACTIVE_CYCLE_RINGS;
				material.0 = materials[&MaterialKey::CycleRingsSelect].clone();
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
				material.0 = materials[&MaterialKey::CycleRingOutlinesDisabled].clone();
			}
			CycleStatus::Ready => {
				transform.translation.z = layers::CYCLE_RING_OUTLINES;
				material.0 = materials[&MaterialKey::CycleRingOutlines].clone();
			}
			CycleStatus::Selected => {
				transform.translation.z = layers::ACTIVE_CYCLE_RING_OUTLINES;
				material.0 = materials[&MaterialKey::CycleRingOutlines].clone();
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

const ERROR_POPUP_TIME: f32 = 0.2;
const ERROR_POPUP_INITIAL_SCALE: f32 = 0.6;

#[derive(SystemParam)]
struct BlockMarkerFactory<'w, 's> {
	commands: Commands<'w, 's>,
	images: Res<'w, HandleMap<ImageKey>>,
	session: Res<'w, LastLevelSessionId>,
}

impl BlockMarkerFactory<'_, '_> {
	fn spawn(&mut self, position: Vec2, hover_hint: &'static str) {
		self.commands.spawn((
			Sprite {
				image: self.images[&ImageKey::ErrorX].clone(),
				custom_size: Some(SPRITE_SIZE),
				..default()
			},
			SpriteColorKey(ColorKey::WarningSign),
			PopupAnimation::new(ERROR_POPUP_TIME, ERROR_POPUP_INITIAL_SCALE),
			Transform::from_translation(position.extend(layers::FAIL_MARKERS)),
			TemporaryMarker,
			HoverHint(hover_hint),
			HoverHintBoundingRect(Aabb2d::new(Vec2::ZERO, SPRITE_SIZE * 0.4)),
			HoverPriority(hover::prio::WORLD_UI),
			self.session.get_session(),
		));
	}
}

/// Hides entities with [`TemporaryMarker`] during the start of a turn.
fn marker_popdown_system(mut query: Query<&mut PopupAnimation, With<TemporaryMarker>>) {
	for mut animation in &mut query {
		animation.set_reversed(true);
	}
}

/// Despawns entities with [`TemporaryMarker`] once their [`PopupAnimation`] is finished
fn marker_despawn_system(
	query: Query<(Entity, &PopupAnimation), With<TemporaryMarker>>,
	mut commands: Commands,
) {
	for (id, animation) in &query {
		if animation.is_reversed() && animation.is_finished() {
			commands.entity(id).despawn();
		}
	}
}

fn cycle_blocked_marker_system(
	mut markers: BlockMarkerFactory,
	mut events: MessageReader<TurnBlockedByGroupConflict>,
	vertices_q: Query<&Transform, With<Vertex>>,
	entity_index: Res<GameStateEcsIndex>,
	level: PlayingLevelData,
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
		markers.spawn(vertex_transform.translation.xy(), hover::BLOCKADE_WARNING);
	}
}

fn wall_blocked_marker_system(
	mut markers: BlockMarkerFactory,
	mut events: MessageReader<TurnBlockedByWallHit>,
	walls_q: Query<&Transform, With<Wall>>,
	entity_index: Res<GameStateEcsIndex>,
) {
	for event in events.read() {
		let Some(vertex_transform) = entity_index
			.walls
			.get(&(event.cycle, event.wall))
			.and_then(|entity| walls_q.get(*entity).ok())
		else {
			log::warn!("Nonexistent wall!");
			continue;
		};
		markers.spawn(vertex_transform.translation.xy(), hover::WALL_HIT_WARNING);
	}
}
