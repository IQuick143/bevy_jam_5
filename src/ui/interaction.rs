use bevy::prelude::*;

use crate::{assets::SfxKey, audio::sfx::PlaySfx};

use super::freeze::ui_not_frozen;

pub(super) fn plugin(app: &mut App) {
	app.register_type::<InteractionPalette>();
	app.add_systems(
		Update,
		(
			apply_interaction_palette,
			apply_interaction_palette_to_sprite_widgets,
			trigger_interaction_sfx,
		)
			.run_if(ui_not_frozen),
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

fn apply_interaction_palette(
	mut palette_query: InteractionQuery<
		(&InteractionPalette, &mut BackgroundColor),
		Without<InteractionPaletteForChildSprites>,
	>,
) {
	for (interaction, enabled, (palette, mut background)) in &mut palette_query {
		*background = if enabled.is_none_or(|e| **e) {
			match interaction {
				Interaction::None => palette.none,
				Interaction::Hovered => palette.hovered,
				Interaction::Pressed => palette.pressed,
			}
		} else {
			palette.disabled
		}
		.into();
	}
}

fn apply_interaction_palette_to_sprite_widgets(
	widget_q: InteractionQuery<
		(&InteractionPalette, &Children),
		With<InteractionPaletteForChildSprites>,
	>,
	mut sprite_q: Query<&mut ImageNode>,
) {
	for (interaction, enabled, (palette, children)) in &widget_q {
		let color = if enabled.is_none_or(|e| **e) {
			match interaction {
				Interaction::None => palette.none,
				Interaction::Hovered => palette.hovered,
				Interaction::Pressed => palette.pressed,
			}
		} else {
			palette.disabled
		};
		for child_id in children {
			if let Ok(mut image) = sprite_q.get_mut(*child_id) {
				image.color = color;
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
