use super::{components::*, inputs::CycleInteraction, logic::*, prelude::*};
use crate::{
	graphics::{
		color_labels, layers,
		primitives::{RoundedPentagonArrow, RoundedRectangle},
		NODE_RADIUS, RING_OUTLINE_WIDTH,
	},
	AppSet,
};
use bevy::{color::palettes, utils::hashbrown::HashMap};

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
					.run_if(on_event::<GameLayoutChanged>()),
				cycle_center_interaction_visuals_update_system
					.run_if(cycle_interaction_visuals_changed),
			)
				.in_set(AppSet::UpdateVisuals),
		);
}

/// References to entities that make up the visualization of a cycle
#[derive(Component, Clone, Debug, Reflect)]
pub struct CycleVisualEntities {
	/// The cycle ring
	pub ring: Entity,
	/// The cycle ring outline
	pub outline: Entity,
	/// The sprite at the center of the cycle
	pub center: Entity,
	/// The arrow that shows up at the center of the cycle
	pub arrow: Entity,
}

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
	/// Material for lines that represent links between cycles
	pub link_lines: Handle<ColorMaterial>,
	/// Meterial for labels that show logical color of buttons
	pub colored_button_labels: Handle<ColorMaterial>,
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
		let link_lines = materials.add(ColorMaterial {
			color: palettes::tailwind::SLATE_300.into(),
			..default()
		});
		let colored_button_labels = materials.add(ColorMaterial {
			color: Color::BLACK,
			..default()
		});

		Self {
			cycle_rings_ready,
			cycle_rings_select,
			cycle_rings_disabled,
			cycle_ring_outlines,
			cycle_ring_outlines_disabled,
			link_lines,
			colored_button_labels,
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

		Self {
			vertices,
			vertex_outlines,
			square_labels,
			arrow_labels,
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
	buttons_q: Query<(&Children, &IsTriggered), (With<BoxSlot>, Changed<IsTriggered>)>,
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
	cycles_q: Query<(&ComputedCycleTurnability, &CycleVisualEntities)>,
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
	cycles_q: Query<(&CycleInteraction, &Cycle)>,
	all_cycles_q: Query<(
		&ComputedCycleTurnability,
		&CycleVertices,
		&CycleVisualEntities,
	)>,
	cycle_index: Res<CycleEntities>,
	level_asset: Res<Assets<LevelData>>,
	level_handle: Res<LevelHandle>,
	vertices_q: Query<&VertexVisualEntities>,
	mut sprites_q: Query<&mut Sprite>,
	mut meshes_q: Query<(&mut Transform, &mut Handle<ColorMaterial>)>,
	palette: Res<ThingPalette>,
	materials: Res<GameObjectMaterials>,
) {
	let Some(level) = level_asset.get(&level_handle.0) else {
		log::error!("Non-existent level asset being referenced.");
		return;
	};

	let mut meshes_to_repaint = HashMap::new();
	let mut outlines_to_repaint = HashMap::new();
	let mut sprites_to_repaint = HashMap::new();

	for (interaction, cycle) in &cycles_q {
		let is_selected = *interaction != CycleInteraction::None;
		for cycle_id in level.groups[cycle.group_id]
			.cycles
			.iter()
			.map(|(cycle_id, _)| cycle_index.0[*cycle_id])
		{
			let (is_turnable, vertices, visuals) = match all_cycles_q.get(cycle_id) {
				Ok(x) => x,
				Err(e) => {
					log::warn!("LinkedCycles refers to a non-cycle entity: {e}");
					continue;
				}
			};

			let cycle_status = if is_selected {
				CycleStatus::Selected
			} else if is_turnable.0 {
				CycleStatus::Ready
			} else {
				CycleStatus::Disabled
			};

			let sprite_status = sprites_to_repaint.entry(visuals.center).or_default();
			if *sprite_status < cycle_status {
				*sprite_status = cycle_status
			}

			vertices
				.0
				.iter()
				.filter_map(|id| {
					vertices_q
						.get(*id)
						.inspect_err(|e| {
							log::warn!("CycleVertices refers to a non-vertex entity: {e}")
						})
						.ok()
						.map(|visuals| (visuals.node, visuals.outline))
				})
				.chain(std::iter::once((visuals.ring, visuals.outline)))
				.for_each(|(body, outline)| {
					let mesh_status = meshes_to_repaint.entry(body).or_default();
					if *mesh_status < cycle_status {
						*mesh_status = cycle_status;
					}
					let mesh_status = outlines_to_repaint.entry(outline).or_default();
					if *mesh_status < cycle_status {
						*mesh_status = cycle_status;
					}
				});
		}
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
				*material = materials.cycle_rings_disabled.clone_weak();
			}
			CycleStatus::Ready => {
				transform.translation.z = layers::CYCLE_RINGS;
				*material = materials.cycle_rings_ready.clone_weak();
			}
			CycleStatus::Selected => {
				transform.translation.z = layers::ACTIVE_CYCLE_RINGS;
				*material = materials.cycle_rings_select.clone_weak();
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
				*material = materials.cycle_ring_outlines_disabled.clone_weak();
			}
			CycleStatus::Ready => {
				transform.translation.z = layers::CYCLE_RING_OUTLINES;
				*material = materials.cycle_ring_outlines.clone_weak();
			}
			CycleStatus::Selected => {
				transform.translation.z = layers::ACTIVE_CYCLE_RING_OUTLINES;
				*material = materials.cycle_ring_outlines.clone_weak();
			}
		}
	}
}
