use core::f32;

use crate::game::prelude::*;

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

/// How big logical color labels for buttons should be
pub const COLOR_LABEL_SIZE: f32 = SPRITE_LENGTH * 0.3828125;
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
