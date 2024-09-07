use super::{components::*, inputs::CycleInteraction, level::LogicalColor, logic::*, prelude::*};
use crate::AppSet;
use bevy::color::palettes;

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<RingMaterial>()
		.init_resource::<LinkMaterial>()
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

/// Contains a handle to the material used for rendering the cycle rings
#[derive(Resource, Deref, DerefMut, Debug, Clone, Reflect)]
pub struct RingMaterial(pub Handle<ColorMaterial>);

impl FromWorld for RingMaterial {
	fn from_world(world: &mut World) -> Self {
		let mut materials = world
			.get_resource_mut::<Assets<ColorMaterial>>()
			.expect("I'd expect materials to exist pretty please.");

		RingMaterial(materials.add(ColorMaterial {
			color: palettes::tailwind::SLATE_400.into(),
			..default()
		}))
	}
}

/// Contains a handle to the material used for rendering cycle links
#[derive(Resource, Deref, DerefMut, Debug, Clone, Reflect)]
pub struct LinkMaterial(pub Handle<ColorMaterial>);

impl FromWorld for LinkMaterial {
	fn from_world(world: &mut World) -> Self {
		let mut materials = world
			.get_resource_mut::<Assets<ColorMaterial>>()
			.expect("I'd expect materials to exist pretty please.");

		LinkMaterial(materials.add(ColorMaterial {
			color: palettes::tailwind::SLATE_300.into(),
			..default()
		}))
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
			box_trigger: p::GREEN_500.into(),
			button_base: p::ORANGE_200.into(),
			button_trigger: p::GREEN_500.into(),
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
	mut query: Query<&mut Sprite, With<Goal>>,
	palette: Res<ThingPalette>,
	completion: Res<LevelCompletionConditions>,
) {
	let color = if completion.is_goal_unlocked() {
		palette.goal_open
	} else {
		palette.goal_closed
	};
	for mut sprite in &mut query {
		sprite.color = color;
	}
}

fn button_trigger_animation_system(
	mut buttons_q: Query<(&mut Sprite, Option<&LogicalColor>), With<BoxSlot>>,
	mut boxes_q: Query<
		(&mut Sprite, Option<&LogicalColor>),
		(
			With<Box>,
			Without<BoxSlot>, /* To guarantee memory aliasing */
		),
	>,
	nodes_q: Query<(&PlacedGlyph, &PlacedObject)>,
	palette: Res<ThingPalette>,
) {
	for (glyph_id, object_id) in &nodes_q {
		let button = glyph_id.0.and_then(|id| buttons_q.get_mut(id).ok());
		let object = object_id.0.and_then(|id| boxes_q.get_mut(id).ok());
		// Use trigger color if both things are at the same place
		// and are of the same logical color, otherwise use base color
		match (button, object) {
			(Some(mut button), Some(mut object)) => {
				let colors_compatible = object
					.1
					.and_then(|object_color| {
						button.1.map(|glyph_color| *object_color == *glyph_color)
					})
					// If either thing is colorless, they are considered compatible
					.unwrap_or(true);
				if colors_compatible {
					button.0.color = palette.button_trigger;
					object.0.color = palette.box_trigger;
				} else {
					button.0.color = palette.button_base;
					object.0.color = palette.box_base;
				}
			}
			(Some((mut button, _)), None) => {
				button.color = palette.button_base;
			}
			(None, Some((mut object, _))) => {
				object.color = palette.box_base;
			}
			(None, None) => {}
		}
	}
}

fn cycle_center_turnability_visuals_update_system(
	cycles_q: Query<(&ComputedCycleTurnability, &Children)>,
	mut sprites_q: Query<&mut Sprite>,
	mut arrows_q: Query<&mut Visibility>,
	palette: Res<ThingPalette>,
) {
	for (is_turnable, children) in &cycles_q {
		let Ok(mut sprite) = sprites_q.get_mut(children[0]) else {
			log::warn!("Child of cycle entity does not have Sprite component");
			continue;
		};
		let Ok(mut arrow_visibility) = arrows_q.get_mut(children[1]) else {
			log::warn!("Child of cycle entity does not have Visibility component");
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
	cycles_q: Query<
		(
			&CycleInteraction,
			&ComputedCycleTurnability,
			Option<&LinkedCycles>,
			&Children,
		),
		Changed<CycleInteraction>,
	>,
	all_cycles_q: Query<(&ComputedCycleTurnability, &Children)>,
	mut sprites_q: Query<&mut Sprite>,
	palette: Res<ThingPalette>,
) {
	for (interaction, is_turnable, links, children) in &cycles_q {
		let target_sprites = std::iter::once((children[0], is_turnable.0)).chain(
			links
				.into_iter()
				.flat_map(|links| &links.0)
				.filter_map(|&(id, _)| {
					all_cycles_q
						.get(id)
						.inspect_err(|e| {
							log::warn!("LinkedCycles refers to a non-cycle entity {e}")
						})
						.ok()
						.map(|(turnable, children)| (children[0], turnable.0))
				}),
		);
		for (id, is_turnable) in target_sprites {
			let Ok(mut sprite) = sprites_q.get_mut(id) else {
				log::warn!("Child of cycle entity does not have Sprite component");
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
