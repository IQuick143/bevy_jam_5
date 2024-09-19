use super::{components::*, inputs::CycleInteraction, level::LogicalColor, logic::*, prelude::*};
use crate::{
	graphics::{
		color_labels,
		primitives::{RoundedPentagonArrow, RoundedRectangle},
		NODE_RADIUS,
	},
	AppSet,
};
use bevy::color::palettes;

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
				cycle_center_interaction_visuals_update_system,
			)
				.in_set(AppSet::UpdateVisuals),
		);
}

/// Contains handles to the materials used to render game objects that are visualized by meshes
#[derive(Resource, Debug, Clone, Reflect)]
pub struct GameObjectMaterials {
	/// Material for cycle rings and vertex dots
	pub cycle_rings: Handle<ColorMaterial>,
	/// Material for lines that represent links between cycles
	pub link_lines: Handle<ColorMaterial>,
	/// Meterial for labels that show logical color of buttons
	pub colored_button_labels: Handle<ColorMaterial>,
}

impl FromWorld for GameObjectMaterials {
	fn from_world(world: &mut World) -> Self {
		let mut materials = world.resource_mut::<Assets<ColorMaterial>>();

		let cycle_rings = materials.add(ColorMaterial {
			color: palettes::tailwind::SLATE_400.into(),
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
			cycle_rings,
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
	/// Mesh for square labels that show logical colors of buttons
	pub square_labels: Handle<Mesh>,
	/// Mesh for arrow-like labels that show logical colors of buttons
	pub arrow_labels: Handle<Mesh>,
}

impl FromWorld for GameObjectMeshes {
	fn from_world(world: &mut World) -> Self {
		let mut meshes = world.resource_mut::<Assets<Mesh>>();

		let vertices = meshes.add(Circle::new(NODE_RADIUS).mesh());
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
			square_labels,
			arrow_labels,
		}
	}
}

/// Contains colors used for rendering objects and glyphs
#[derive(Resource, Debug, Clone, Reflect)]
pub struct ThingPalette {
	pub box_base: Color,
	pub box_trigger: Color,
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
			box_trigger: p::ORANGE_200.into(),
			button_base: Srgba::hex("CC5151").unwrap().into(),
			button_trigger: p::GREEN_300.into(),
			player: p::SLATE_200.into(),
			goal_closed: p::SLATE_100.into(),
			goal_open: p::GREEN_500.into(),
			cycle_disabled: p::SLATE_200.into(),
			cycle_ready: p::SLATE_300.into(),
			cycle_trigger: p::SLATE_400.into(),
		}
	}
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
	buttons_q: Query<(&Children, Option<&LogicalColor>), With<BoxSlot>>,
	boxes_q: Query<(&Children, Option<&LogicalColor>), With<Box>>,
	nodes_q: Query<(&PlacedGlyph, &PlacedObject)>,
	palette: Res<ThingPalette>,
) {
	let mut recolor_sprites = |sprite_ids: &Children, color| {
		for id in sprite_ids {
			if let Ok(mut sprite) = sprites_q.get_mut(*id) {
				sprite.color = color;
			}
		}
	};

	for (glyph_id, object_id) in &nodes_q {
		let button = glyph_id.0.and_then(|id| buttons_q.get(id).ok());
		let object = object_id.0.and_then(|id| boxes_q.get(id).ok());
		// Use trigger color if both things are at the same place
		// and are of the same logical color, otherwise use base color
		match (button, object) {
			(Some((button, button_color)), Some((object, object_color))) => {
				let colors_compatible = object_color
					.and_then(|object_color| {
						button_color.map(|glyph_color| *object_color == *glyph_color)
					})
					// If either thing is colorless, they are considered compatible
					.unwrap_or(true);
				if colors_compatible {
					recolor_sprites(button, palette.button_trigger);
					recolor_sprites(object, palette.box_trigger);
				} else {
					recolor_sprites(button, palette.button_base);
					recolor_sprites(object, palette.box_base);
				}
			}
			(Some((button, _)), None) => {
				recolor_sprites(button, palette.button_base);
			}
			(None, Some((object, _))) => {
				recolor_sprites(object, palette.box_base);
			}
			(None, None) => {}
		}
	}
}

fn cycle_center_turnability_visuals_update_system(
	cycles_q: Query<(&ComputedCycleTurnability, &CycleVisualEntities)>,
	mut sprites_q: Query<&mut Sprite>,
	mut arrows_q: Query<&mut Visibility>,
	palette: Res<ThingPalette>,
) {
	for (is_turnable, visuals) in &cycles_q {
		let Ok(mut sprite) = sprites_q.get_mut(visuals.center) else {
			log::warn!("Cycle center sprite does not have Sprite component");
			continue;
		};
		let Ok(mut arrow_visibility) = arrows_q.get_mut(visuals.arrow) else {
			log::warn!("Cycle arrow sprite does not have Visibility component");
			continue;
		};
		if is_turnable.0 {
			*arrow_visibility = default();
			sprite.color = palette.cycle_ready;
		} else {
			*arrow_visibility = Visibility::Hidden;
			sprite.color = palette.cycle_disabled;
		}
	}
}

fn cycle_center_interaction_visuals_update_system(
	cycles_q: Query<(&CycleInteraction, &LinkedCycles), Changed<CycleInteraction>>,
	all_cycles_q: Query<(&ComputedCycleTurnability, &CycleVisualEntities)>,
	mut sprites_q: Query<&mut Sprite>,
	palette: Res<ThingPalette>,
) {
	for (interaction, links) in &cycles_q {
		let target_sprites = links.0.iter().filter_map(|&(id, _)| {
			all_cycles_q
				.get(id)
				.inspect_err(|e| log::warn!("LinkedCycles refers to a non-cycle entity {e}"))
				.ok()
				.map(|(turnable, visuals)| (visuals.center, turnable.0))
		});
		for (id, is_turnable) in target_sprites {
			let Ok(mut sprite) = sprites_q.get_mut(id) else {
				log::warn!("Cycle sprite entity does not have Sprite component");
				continue;
			};
			sprite.color = if *interaction != CycleInteraction::None {
				palette.cycle_trigger
			} else if is_turnable {
				palette.cycle_ready
			} else {
				palette.cycle_disabled
			};
		}
	}
}
