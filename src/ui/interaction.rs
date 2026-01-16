use super::freeze::IsUiFrozen;
use crate::{
	assets::SfxKey,
	audio::sfx::PlaySfx,
	drawing::{ColorKey, NodeColorKey},
};
use bevy::{
	ecs::{lifecycle::HookContext, world::DeferredWorld},
	prelude::*,
};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			apply_interaction_palette.in_set(ApplyInteractionPaletteSystems),
			trigger_interaction_sfx,
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
#[derive(Component, Clone, Copy, Debug)]
#[require(InteractionColor)]
#[component(on_add = on_add_interaction_palette)]
pub struct InteractionPalette {
	pub none: ColorKey,
	pub hovered: ColorKey,
	pub pressed: ColorKey,
	pub disabled: ColorKey,
}

impl InteractionPalette {
	pub const COMMON_BUTTON: Self = Self {
		none: ColorKey::NodeBackground,
		hovered: ColorKey::UiButtonHovered,
		pressed: ColorKey::UiButtonPressed,
		disabled: ColorKey::UiButtonDisabled,
	};

	pub const NEW_LEVEL_BUTTON: Self = Self {
		none: ColorKey::NewLevelButton,
		..Self::COMMON_BUTTON
	};

	pub const SPRITE_BUTTON: Self = Self {
		none: ColorKey::SpriteButton,
		hovered: ColorKey::SpriteButtonHovered,
		pressed: ColorKey::SpriteButtonPressed,
		disabled: ColorKey::SpriteButtonDisabled,
	};

	pub const NEXT_LEVEL_BUTTON: Self = Self {
		none: ColorKey::NextLevelButton,
		hovered: ColorKey::NextLevelButtonHovered,
		pressed: ColorKey::NextLevelButtonPressed,
		disabled: ColorKey::NextLevelButton,
	};
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
struct InteractionColor(ColorKey);

/// Marker component for UI entities that are exempt from freezing
/// via [`FreezeUi`](super::freeze::FreezeUi)
#[derive(Component, Clone, Copy, Debug, Default)]
pub struct Unfreeze;

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
	mut palette_query: Query<(
		Ref<Interaction>,
		Option<Ref<InteractionEnabled>>,
		Has<Unfreeze>,
		&InteractionPalette,
		&mut InteractionColor,
	)>,
	is_frozen: IsUiFrozen,
) {
	let freeze_changed = is_frozen.changed();
	let is_frozen = is_frozen.get();

	for (interaction, enabled, unfreeze, palette, mut color) in &mut palette_query {
		let enabled_changed = enabled.as_ref().is_some_and(Ref::is_changed);
		// Can't use a query filter because there is no option to bypass
		// it when the UI freeze status just changed, so we must use this
		if !interaction.is_changed() && !enabled_changed && !freeze_changed {
			continue;
		}
		// If the UI is frozen, do not repaint unless the change is enable/disable
		// or the entity is being initialized
		if is_frozen && !unfreeze && !enabled_changed && !interaction.is_added() {
			continue;
		}

		let new_color = if enabled.is_none_or(|e| **e) {
			match *interaction {
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

fn propagate_interaction_color_to_node_widgets(
	mut query: Query<
		(&InteractionColor, &mut NodeColorKey),
		(
			Changed<InteractionColor>,
			Without<InteractionPaletteForChildSprites>,
		),
	>,
) {
	for (color, mut node_color) in &mut query {
		**node_color = **color;
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
	mut sprite_q: Query<&mut NodeColorKey>,
) {
	for (color, children) in &widget_q {
		for child_id in children {
			if let Ok(mut node_color) = sprite_q.get_mut(*child_id) {
				**node_color = **color;
			}
		}
	}
}

fn trigger_interaction_sfx(
	mut interactions: Query<
		(&Interaction, Option<&InteractionEnabled>, Has<Unfreeze>),
		Changed<Interaction>,
	>,
	mut commands: Commands,
	is_frozen: IsUiFrozen,
) {
	let is_frozen = is_frozen.get();

	for (interaction, enabled, unfreeze) in &mut interactions {
		if enabled.is_none_or(|e| **e) && (!is_frozen || unfreeze) {
			match interaction {
				Interaction::Hovered => commands.trigger(PlaySfx::Effect(SfxKey::ButtonHover)),
				Interaction::Pressed => commands.trigger(PlaySfx::Effect(SfxKey::ButtonPress)),
				_ => (),
			}
		}
	}
}
