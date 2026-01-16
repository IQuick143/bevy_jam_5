//! Star rating UI element for playtesters

use crate::{
	assets::{HandleMap, ImageKey},
	drawing::{ColorKey, NodeColorKey},
	ui::{
		consts::*,
		interaction::{InteractionPaletteForChildSprites, Unfreeze},
		prelude::InteractionPalette,
	},
	AppSet,
};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(
			create_star_children,
			propagate_star_inputs,
			apply_star_interaction_palette.in_set(AppSet::UpdateVisuals),
		),
	);
}

/// Component that marks a star rating UI widget
#[derive(Component, Clone, Copy, Debug)]
pub struct StarRating {
	max_value: u32,
	value: u32,
	/// Propagated index of the star that is currently selected
	/// by the user for the purposes of focus highlighting
	tentative_value: u32,
}

/// Component that marks a single star in a star rating widget
#[derive(Component, Clone, Copy, Debug)]
struct Star {
	index: u32,
}

const STAR_SIZE: Val = Val::Px(SPRITE_BUTTON_HEIGHT);

/// [`Bundle`] of a star rating UI widget
pub fn star_rating_widget(count: u32, value: u32) -> impl Bundle {
	(
		StarRating {
			max_value: count,
			value,
			tentative_value: 0,
		},
		Node::default(),
		InteractionPalette::SPRITE_BUTTON,
	)
}

fn create_star_children(
	mut commands: Commands,
	query: Query<(Entity, &StarRating), Added<StarRating>>,
	image_handles: Res<HandleMap<ImageKey>>,
) {
	for (id, rating) in &query {
		for i in 0..rating.max_value {
			commands.spawn((
				Star { index: i },
				Button,
				Node {
					width: STAR_SIZE,
					height: STAR_SIZE,
					..default()
				},
				ImageNode {
					image: image_handles[&ImageKey::Star].clone(),
					..default()
				},
				NodeColorKey(ColorKey::SpriteButton),
				InteractionPaletteForChildSprites,
				Unfreeze,
				ChildOf(id),
			));
		}
	}
}

fn propagate_star_inputs(
	mut widgets_q: Query<&mut StarRating>,
	stars_q: Query<(&Star, &ChildOf, &Interaction), Changed<Interaction>>,
) {
	// Zero out hover values first
	for (_, &ChildOf(parent), &interaction) in &stars_q {
		if interaction == Interaction::None {
			if let Ok(mut widget) = widgets_q.get_mut(parent) {
				widget.tentative_value = 0;
			}
		}
	}
	// Do a second pass for other values
	// to ensure that non-exit values are handled first
	// when the cursor moves from one star to another
	// in a single frame
	for (&Star { index }, &ChildOf(parent), &interaction) in &stars_q {
		if interaction != Interaction::None {
			if let Ok(mut widget) = widgets_q.get_mut(parent) {
				match interaction {
					Interaction::Pressed => widget.value = index + 1,
					Interaction::Hovered => widget.tentative_value = index + 1,
					Interaction::None => unreachable!(),
				}
			}
		}
	}
}

fn apply_star_interaction_palette(
	widgets_q: Query<(&StarRating, &Children, &InteractionPalette), Changed<StarRating>>,
	mut stars_q: Query<(&Star, &mut NodeColorKey)>,
) {
	for (widget, children, palette) in &widgets_q {
		for id in children {
			if let Ok((&Star { index }, mut color)) = stars_q.get_mut(*id) {
				let selected = index < widget.value;
				let tentative = index < widget.tentative_value;

				let new_color = if selected && tentative {
					palette.hovered
				} else if selected {
					palette.pressed
				} else if tentative {
					palette.hovered
				} else {
					palette.none
				};

				color.set_if_neq(NodeColorKey(new_color));
			}
		}
	}
}
