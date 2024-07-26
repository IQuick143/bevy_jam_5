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

pub const SPRITE_SIZE: Vec2 = Vec2::splat(SPRITE_LENGTH);

pub fn plugin(app: &mut App) {
	app.init_resource::<RingMaterial>()
		.init_resource::<ThingColor>();
}
