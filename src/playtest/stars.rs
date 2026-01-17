//! Star rating UI element for playtesters

use crate::{
	assets::{HandleMap, ImageKey},
	drawing::NodeColorKey,
	ui::{consts::*, interaction::Unfreeze, prelude::InteractionPalette},
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
#[component(immutable)]
#[require(
	Node {
		// Fix the height so there is no flicker as the widget resizes
		// when the stars are spawned one tick later
		height: STAR_SIZE,
		..default()
	},
	StarRatingValue,
	StarRatingTentativeValue,
	InteractionPalette::SPRITE_BUTTON,
)]
pub struct StarRating {
	max_value: u32,
}

/// Current value set on a star rating widget
#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut)]
pub struct StarRatingValue(pub u32);

/// Propagated index of the star that is currently selected
/// by the user for the purposes of focus highlighting
#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut)]
struct StarRatingTentativeValue(u32);

/// Component that marks a single star in a star rating widget
#[derive(Component, Clone, Copy, Debug)]
#[component(immutable)]
#[require(
	Button,
	Node {
		width: STAR_SIZE,
		height: STAR_SIZE,
		..default()
	},
	NodeColorKey(InteractionPalette::SPRITE_BUTTON.none),
)]
struct Star {
	index: u32,
}

const STAR_SIZE: Val = Val::Px(SPRITE_BUTTON_HEIGHT);

impl StarRating {
	pub fn new(max_value: u32) -> Self {
		Self { max_value }
	}
}

fn create_star_children(
	mut commands: Commands,
	query: Query<(Entity, &StarRating), Added<StarRating>>,
	image_handles: Res<HandleMap<ImageKey>>,
) {
	for (id, rating) in &query {
		for index in 0..rating.max_value {
			commands.spawn((
				Star { index },
				ImageNode {
					image: image_handles[&ImageKey::Star].clone(),
					..default()
				},
				Unfreeze,
				ChildOf(id),
			));
		}
	}
}

fn propagate_star_inputs(
	mut widgets_q: Query<(&mut StarRatingValue, &mut StarRatingTentativeValue)>,
	stars_q: Query<(&Star, &ChildOf, &Interaction), Changed<Interaction>>,
) {
	// Zero out hover values first
	for (_, &ChildOf(parent), &interaction) in &stars_q {
		if interaction == Interaction::None {
			if let Ok((_, mut tentative)) = widgets_q.get_mut(parent) {
				**tentative = 0;
			}
		}
	}
	// Do a second pass for other values
	// to ensure that non-exit values are handled first
	// when the cursor moves from one star to another
	// in a single frame
	for (&Star { index }, &ChildOf(parent), &interaction) in &stars_q {
		if interaction != Interaction::None {
			if let Ok((mut value, mut tentative)) = widgets_q.get_mut(parent) {
				match interaction {
					Interaction::Pressed => **value = index + 1,
					Interaction::Hovered => **tentative = index + 1,
					Interaction::None => unreachable!(),
				}
			}
		}
	}
}

fn apply_star_interaction_palette(
	widgets_q: Query<
		(
			&StarRatingValue,
			&StarRatingTentativeValue,
			&Children,
			&InteractionPalette,
		),
		Or<(Changed<StarRatingValue>, Changed<StarRatingTentativeValue>)>,
	>,
	mut stars_q: Query<(&Star, &mut NodeColorKey)>,
) {
	for (value, tentative, children, palette) in &widgets_q {
		for id in children {
			if let Ok((&Star { index }, mut color)) = stars_q.get_mut(*id) {
				let selected = index < **value;
				let tentative = index < **tentative;

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
