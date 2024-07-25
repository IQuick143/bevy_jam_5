use crate::game::prelude::*;

use super::logic;

/// Rectangle centered on (0,0), into which everything should fit in order to guarantee it being rendered.
pub const GAME_AREA: Vec2 = Vec2::new(1600.0, 900.0);

/// Rectangle extents into which the level should fit for layout purposes.
pub const LEVEL_AREA_WIDTH: Vec2 = Vec2::new(1500.0, 800.0);
/// Center of the rectangle into which the level should fit for layout purposes.
/// See [`LEVEL_AREA_HALF_WIDTH`]
pub const LEVEL_AREA_CENTER: Vec2 = Vec2::new(0.0, 0.0);

pub fn plugin(app: &mut App) {
	//	app.add_systems(Update, ());
}
