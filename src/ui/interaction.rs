use super::freeze::ui_not_frozen;
use crate::{assets::SfxKey, audio::sfx::PlaySfx};
use bevy::{
	ecs::{lifecycle::HookContext, world::DeferredWorld},
	prelude::*,
};

pub(super) fn plugin(app: &mut App) {
	app.register_type::<InteractionPalette>();
	app.add_systems(
		Update,
		(
			(
				apply_interaction_palette.in_set(ApplyInteractionPaletteSystems),
				trigger_interaction_sfx,
			)
				.run_if(ui_not_frozen),
			// If the UI is frozen, update only based on a limited set of triggers (enable/disable)
			apply_enable_disable_interaction_palette
				.in_set(ApplyInteractionPaletteSystems)
				.run_if(not(ui_not_frozen)),
			(
				propagate_interaction_color_to_node_widgets,
				propagate_interaction_color_to_sprite_widgets,
			)
				.after(ApplyInteractionPaletteSystems),
		),
	);
}

/// Indicates whether a UI element can be interacted with
///
/// The native bevy equivalent, [`bevy::ui::InteractionDisabled`],
/// is a marker component which makes it a pain to do change detection with,
/// so we roll our own
#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Deref, DerefMut, Reflect)]
#[reflect(Component)]
pub struct InteractionEnabled(pub bool);

impl Default for InteractionEnabled {
	fn default() -> Self {
		// Interactions are enabled by default
		Self(true)
	}
}

pub type InteractionQuery<'w, 's, T, F = ()> = Query<
	'w,
	's,
	(&'static Interaction, Option<&'static InteractionEnabled>, T),
	(Or<(Changed<Interaction>, Changed<InteractionEnabled>)>, F),
>;

/// Palette for widget interactions.
#[derive(Component, Debug, Reflect)]
#[reflect(Component)]
#[require(InteractionColor)]
#[component(on_add = on_add_interaction_palette)]
pub struct InteractionPalette {
	pub none: Color,
	pub hovered: Color,
	pub pressed: Color,
	pub disabled: Color,
}

/// Marker for entities whose [`InteractionPalette`] applies to their children
/// which are expected to be [`ImageNode`]s
///
/// By default, [`InteractionPalette`] applies to the [`BackgroundColor`]
/// of the target entity itself
#[derive(Component, Clone, Copy, Debug, Default, Reflect)]
#[reflect(Component)]
pub struct InteractionPaletteForChildSprites;

/// Cache for a color that is applied to an entity
/// in different ways, depending on presence of other marker components
#[derive(Component, Clone, Copy, PartialEq, Debug, Default, Deref, DerefMut)]
struct InteractionColor(Color);

/// System set where [`InteractionColor`] is updated
#[derive(SystemSet, Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
struct ApplyInteractionPaletteSystems;

fn on_add_interaction_palette(mut world: DeferredWorld, context: HookContext) {
	let mut entity = world.entity_mut(context.entity);
	if let Some(palette) = entity.get::<InteractionPalette>() {
		let clear_color = palette.none;
		if let Some(mut color) = entity.get_mut::<InteractionColor>() {
			**color = clear_color;
		}
	}
}

fn apply_interaction_palette(
	mut palette_query: InteractionQuery<(&InteractionPalette, &mut InteractionColor)>,
) {
	for (interaction, enabled, (palette, mut color)) in &mut palette_query {
		let new_color = if enabled.is_none_or(|e| **e) {
			match interaction {
				Interaction::None => palette.none,
				Interaction::Hovered => palette.hovered,
				Interaction::Pressed => palette.pressed,
			}
		} else {
			palette.disabled
		};
		color.set_if_neq(InteractionColor(new_color));
	}
}

fn apply_enable_disable_interaction_palette(
	mut palette_query: Query<
		(
			&InteractionEnabled,
			&InteractionPalette,
			&mut InteractionColor,
		),
		Changed<InteractionEnabled>,
	>,
) {
	for (enabled, palette, mut color) in &mut palette_query {
		let new_color = if **enabled {
			palette.none
		} else {
			palette.disabled
		};
		color.set_if_neq(InteractionColor(new_color));
	}
}

fn propagate_interaction_color_to_node_widgets(
	mut query: Query<
		(&InteractionColor, &mut BackgroundColor),
		(
			Changed<InteractionColor>,
			Without<InteractionPaletteForChildSprites>,
		),
	>,
) {
	for (color, mut background_color) in &mut query {
		background_color.0 = **color;
	}
}

fn propagate_interaction_color_to_sprite_widgets(
	widget_q: Query<
		(&InteractionColor, &Children),
		(
			Changed<InteractionColor>,
			With<InteractionPaletteForChildSprites>,
		),
	>,
	mut sprite_q: Query<&mut ImageNode>,
) {
	for (color, children) in &widget_q {
		for child_id in children {
			if let Ok(mut image) = sprite_q.get_mut(*child_id) {
				image.color = **color;
			}
		}
	}
}

fn trigger_interaction_sfx(
	mut interactions: Query<(&Interaction, Option<&InteractionEnabled>), Changed<Interaction>>,
	mut commands: Commands,
) {
	for (interaction, enabled) in &mut interactions {
		if enabled.is_none_or(|e| **e) {
			match interaction {
				Interaction::Hovered => commands.trigger(PlaySfx::Effect(SfxKey::ButtonHover)),
				Interaction::Pressed => commands.trigger(PlaySfx::Effect(SfxKey::ButtonPress)),
				_ => (),
			}
		}
	}
}
