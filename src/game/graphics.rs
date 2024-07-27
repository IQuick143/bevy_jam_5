use crate::game::prelude::*;

/// Rectangle centered on (0,0), into which everything should fit in order to guarantee it being rendered.
pub const GAME_AREA: Vec2 = Vec2::new(1600.0, 900.0);

/// Rectangle extents into which the level should fit for layout purposes.
pub const LEVEL_AREA_WIDTH: Vec2 = Vec2::new(1500.0, 800.0);
/// Center of the rectangle into which the level should fit for layout purposes.
/// See [`LEVEL_AREA_HALF_WIDTH`]
pub const LEVEL_AREA_CENTER: Vec2 = Vec2::new(0.0, 0.0);

pub const SPRITE_LENGTH: f32 = 100.0;

pub const RING_HALF_WIDTH: f32 = 0.225 / 4.0 * SPRITE_LENGTH;

pub const NODE_RADIUS: f32 = SPRITE_LENGTH / 8.0;

pub const CYCLE_LINK_WIDTH: f32 = NODE_RADIUS;

/// Spacing between the two "belts" of a cycle link
pub const CYCLE_LINK_SPACING: f32 = SPRITE_LENGTH * 0.7;

/// How much shorter cycle links should be than the distance
/// between the centers of the cycles
pub const CYCLE_LINK_END_CUT: f32 = SPRITE_LENGTH / 2.0;

pub const SPRITE_SIZE: Vec2 = Vec2::splat(SPRITE_LENGTH);

/// Colour into which the screen fades during transitions
pub const FADE_COLOUR: Color = Color::WHITE;

pub fn plugin(app: &mut App) {
	app.init_resource::<RingMaterial>()
		.init_resource::<LinkMaterial>()
		.init_resource::<ThingPalette>();
}
