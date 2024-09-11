use crate::game::prelude::*;
use core::f32;

pub mod primitives;

/// Rectangle centered on (0,0), into which everything should fit in order to guarantee it being rendered.
pub const GAME_AREA: Vec2 = Vec2::new(1600.0, 900.0);

/// Rectangle extents into which the level should fit for layout purposes.
pub const LEVEL_AREA_WIDTH: Vec2 = Vec2::new(1500.0, 650.0);
/// Center of the rectangle into which the level should fit for layout purposes.
/// See [`LEVEL_AREA_WIDTH`]
pub const LEVEL_AREA_CENTER: Vec2 = Vec2::ZERO;

/// Size of the hint text at the bottom of the level
/// Position is glued to the bottom of the [`GAME_AREA`]
pub const HINT_TEXT_SIZE: Vec2 = Vec2::new(1600.0, 100.0);

/// Size of a sprite in world-units.
pub const SPRITE_LENGTH: f32 = 100.0;

pub const RING_HALF_WIDTH: f32 = 0.225 / 4.0 * SPRITE_LENGTH;

pub const NODE_RADIUS: f32 = SPRITE_LENGTH / 8.0;

pub const CYCLE_LINK_WIDTH: f32 = NODE_RADIUS;

/// Spacing between the two "belts" of a cycle link
pub const CYCLE_LINK_SPACING: f32 = SPRITE_LENGTH * 0.7;

/// How much shorter cycle links should be than the distance
/// between the centers of the cycles
pub const CYCLE_LINK_END_CUT: f32 = SPRITE_LENGTH / 2.0;

/// How big should a sprite be.
/// See [`SPRITE_LENGTH`]
pub const SPRITE_SIZE: Vec2 = Vec2::splat(SPRITE_LENGTH);

/// How big logical color sprites should be
pub const COLOR_SPRITE_SIZE: Vec2 = Vec2::splat(SPRITE_LENGTH * 0.265625);
/// How logical color sprites on boxes should be offset vertically from the parent sprite
pub const COLOR_SPRITE_OFFSET: f32 = SPRITE_LENGTH * 0.111328125;
/// How much of the width of a sprite is actually taken up by digits other than 1
pub const DIGIT_SPRITE_WIDTH: f32 = 0.8;
/// How much of the width of a sprite is taken up by the digit 1
pub const DIGIT_ONE_SPRITE_WIDTH: f32 = 0.4;
/// Spacing of digit sprites, relative to full width of a sprite
pub const DIGIT_SPRITE_SPACING: f32 = 0.15;

/// Various dimensions of button color labels.
/// In a separate module, because there are too many of them
pub mod color_labels {
	use super::*;
	use std::f32::consts::PI;

	/// How big the square labels should be
	pub const SIZE: f32 = SPRITE_LENGTH * 0.3828125;
	/// Radius of rounded corners of the labels
	pub const CORNER_RADIUS: f32 = SIZE * 0.1;
	/// Length of the arrow tip of the labels, if present
	pub const ARROW_TIP_LENGTH: f32 = SIZE / 3.0;
	/// Number of vertices used for rounded parts of label meshes
	pub const MESH_RESOLUTION: usize = 4;
	/// Vertical position of the center of the label, relative to the position
	/// of the entity, when it is inside the button
	pub const CENTER_Y_OFFSET: f32 = COLOR_SPRITE_OFFSET;
	/// Vertical position of the center of the label,
	/// relative to the position of the vertex, when it is aligned to the button
	pub const OFFSET_Y_BUTTON_ALIGNED: f32 = -SPRITE_LENGTH / 8.0;
	/// Width of the gap meant to be between the button/box sprite and the label
	pub const GAP_WIDTH: f32 = SPRITE_LENGTH * 0.04;
	/// Half-width of the actual button sprite (without the padding)
	pub const BUTTON_SPRITE_HALF_WIDTH: f32 = SPRITE_LENGTH * 0.33203125;
	/// How far from the center of the box should a square label be placed
	pub const OFFSET_SECONDARY_SQUARE: f32 = BUTTON_SPRITE_HALF_WIDTH + GAP_WIDTH + SIZE / 2.0;
	/// How far from the center of the box should an arrow label be placed
	pub const OFFSET_SECONDARY_ARROW: f32 = OFFSET_SECONDARY_SQUARE + ARROW_TIP_LENGTH;
	/// How much closer can the label be pulled to the box under reduced offset
	pub const SECONDARY_OFFSET_REDUCTION: f32 = SPRITE_LENGTH * 0.083984375;
	/// Maximum angle away from the top where reduced offset can apply
	pub const OFFSET_REDUCTION_THRESHOLD: f32 = PI * 4.0 / 9.0;
	/// How far from the center of the box can a rotated square label be placed
	pub const MAX_ROTATED_DISPLACEMENT_SQUARE: f32 =
		SPRITE_LENGTH * 0.4453125 + GAP_WIDTH + SIZE / 2.0;
	/// How far from the center of the box can a rotated arrow label be placed
	pub const MAX_ROTATED_DISPLACEMENT_ARROW: f32 =
		MAX_ROTATED_DISPLACEMENT_SQUARE + ARROW_TIP_LENGTH;
}

/// Colour into which the screen fades during transitions
pub const FADE_COLOUR: Color = Color::WHITE;

/// Velocity of the background in local background coordinates
pub const BACKGROUND_VELOCITY: Vec2 = Vec2::new(16.0, -8.0 * f32::consts::SQRT_2);
/// Velocity of the background in local background coordinates
pub const BACKGROUND_TILING: f32 = 400.0;

/// Defines Z depth of various objects to layer them properly
pub mod layers {
	pub const TITLE_IMAGE: f32 = -10.0;
	pub const OBJECT_SPRITES: f32 = -10.0;
	pub const GLYPH_SPRITES: f32 = -50.0;
	pub const HINT_TEXT_PANEL: f32 = -100.0;
	pub const CYCLE_NODES: f32 = -100.0;
	pub const CYCLE_RINGS: f32 = -200.0;
	pub const CYCLE_CENTER_ARROWS: f32 = -250.0;
	pub const CYCLE_CENTER_SPRITES: f32 = -300.0;
	pub const CYCLE_LINKS: f32 = -400.0;
	pub const BACKGROUND: f32 = -550.0;
	pub const BOX_COLOR_SPRITES: f32 = 2.0; // Relative to the box sprite
	pub const BUTTON_COLOR_LABELS: f32 = 1.0; // Relative to the button sprite
	pub const BUTTON_COLOR_SPRITES: f32 = 2.0; // Relative to the button sprite
}
