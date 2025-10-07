//! Global UI measurements

use bevy::prelude::*;
use Val::*;

// Common
pub const HEADING_HEIGHT: Val = Px(65.0);
pub const HEADING_TEXT_SIZE: f32 = 40.0;
pub const MENU_BUTTON_HEIGHT: Val = HEADING_HEIGHT;
pub const MENU_BUTTON_TEXT_SIZE: f32 = HEADING_TEXT_SIZE;
pub const WIDE_BUTTON_WIDTH: Val = Px(200.0);
pub const COMMON_BUTTON_TEXT_SIZE: f32 = 30.0;
pub const LABEL_WIDTH: Val = Px(500.0);
pub const COMMON_GAP_PX: f32 = 10.0;
pub const COMMON_GAP: Val = Px(COMMON_GAP_PX);
pub const COMMON_TEXT_SIZE: f32 = 24.0;

// Double-columned screens
pub const WIDE_GAP: Val = Px(20.0);
pub const INLINE_BUTTON_PADDING: UiRect = UiRect::axes(Px(10.0), Px(5.0));
pub const INLINE_BUTTON_WIDTH: Val = Px(150.0);

// Level select
pub const GRID_BUTTON_HEIGHT_PX: f32 = 45.0;
pub const GRID_BUTTON_HEIGHT: Val = Px(GRID_BUTTON_HEIGHT_PX);
pub const LEVEL_COMPLETED_MARKER_SIZE: Val = Px(30.0);
pub const LEVEL_COMPLETED_MARKER_MARGIN: Val = Px(7.5);

// Playing screen
pub const TOOLBAR_BUTTON_PADDING: UiRect = UiRect::all(Px(10.0));
pub const HOVER_HINT_TEXT_SIZE: f32 = 20.0;
/// Size of the level title label on the playing screen
pub const LEVEL_TITLE_SIZE: f32 = 35.0;
/// Width of the gap between title label and checkmark indicating completion
pub const LEVEL_TITLE_CHECK_GAP: f32 = 15.0;
pub const LEVEL_TITLE_HEIGHT: Val = Vh(10.0);
