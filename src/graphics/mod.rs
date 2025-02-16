use crate::game::prelude::*;
use core::f32;

pub mod primitives;
mod scrolling_texture;

pub use scrolling_texture::ScrollingTextureMaterial;

pub(super) fn plugin(app: &mut App) {
	app.add_plugins(scrolling_texture::plugin);
}

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

/// Half of the width of a cycle perimeter ring in world units
pub const RING_HALF_WIDTH: f32 = 0.15 / 4.0 * SPRITE_LENGTH;

/// Number of vertices necessary for the meshes for cycle perimeter rings
/// ## Parameters
/// - `radius` - Radius of the cycle ring in world units
pub fn cycle_ring_mesh_resolution(radius: f32) -> u32 {
	let resolution = (5.0 * radius.sqrt()) as u32;
	resolution.clamp(8, u32::MAX)
}

/// Radius of the visuals for vertices in world units
pub const NODE_RADIUS: f32 = SPRITE_LENGTH / 8.0;

/// Thickness of the outlines of visuals for cycle rings and vertices, in world units
pub const RING_OUTLINE_WIDTH: f32 = SPRITE_LENGTH / 36.0;

/// Width of the visuals for cycle links in world units
pub const CYCLE_LINK_WIDTH: f32 = NODE_RADIUS;

/// Spacing between the two "belts" of a cycle link in world units
pub const CYCLE_LINK_SPACING: f32 = SPRITE_LENGTH * 0.7;

/// How much shorter cycle links should be than the distance
/// between the centers of the cycles, in world units
pub const CYCLE_LINK_END_CUT: f32 = SPRITE_LENGTH / 2.0;

/// How much space should be left between the end of a one-way link visual
/// and its target cycle's center
pub const ONEWAY_LINK_TARGET_OFFSET: f32 = SPRITE_LENGTH * 0.65;
/// Length of each arm of the tip of a one-way link visual
pub const ONEWAY_LINK_TIP_LENGTH: f32 = SPRITE_LENGTH / 2.0;
/// Angle between a one-way link and the arms of its tip
pub const ONEWAY_LINK_TIP_ANGLE: f32 = f32::consts::PI / 5.0;
/// Number of vertices to use for each end of the tip
pub const ONEWAY_LINK_TIP_RESOLUTION: u32 = 8;
/// For low-multiplicity multi-links, how much space
/// should be between the repeated tips
pub const ONEWAY_MULTILINK_TIP_SPACING: f32 = ONEWAY_LINK_TIP_LENGTH * 0.75;
/// How many arrow tips can be used on a multi-link
/// (anything more will be indicated with text)
pub const ONEWAY_MULTILINK_MAX_COUNT: u64 = 4;
/// Size of each digit for a link multiplicity indicator
pub const ONEWAY_MULTILINK_DIGIT_SIZE: Vec2 = Vec2::splat(CYCLE_LINK_WIDTH * 2.5);
/// Padding before the text in a link multiplicity indicator
pub const ONEWAY_MULTILINK_TEXT_BEFORE: f32 = CYCLE_LINK_WIDTH * 1.25;
/// Padding after the text in a link multiplicity indicator
pub const ONEWAY_MULTILINK_TEXT_AFTER: f32 = ONEWAY_MULTILINK_TIP_SPACING + CYCLE_LINK_WIDTH * 0.25;
/// Length of the backhead line of a link with numeric multiplicity label
pub const ONEWAY_MULTILINK_BACKHEAD_LENGTH: f32 = ONEWAY_LINK_TIP_LENGTH * 0.65;

/// How big should a sprite be in world units.
/// See [`SPRITE_LENGTH`]
pub const SPRITE_SIZE: Vec2 = Vec2::splat(SPRITE_LENGTH);

/// Sprite anchor point for player and flag sprites in sprite-relative coordinates
pub const PLAYER_FLAG_SPRITE_ANCHOR: Vec2 = Vec2::new(0.0, -0.25);

/// How big logical color sprites should be in world units
pub const COLOR_SPRITE_SIZE: Vec2 = Vec2::splat(SPRITE_LENGTH * 0.265625);
/// How logical color sprites on boxes should be offset vertically from the parent sprite in world units
pub const COLOR_SPRITE_OFFSET: f32 = 0.0;
/// How much of the width of a sprite is actually taken up by digits other than 1,
/// relative to width of a full sprite slot
pub const DIGIT_SPRITE_WIDTH: f32 = 0.8;
/// How much of the width of a sprite is taken up by the digit 1
/// relative to width of a full sprite slot
pub const DIGIT_ONE_SPRITE_WIDTH: f32 = 0.4;
/// Spacing of digit sprites, relative to full width of a sprite slot
pub const DIGIT_SPRITE_SPACING: f32 = 0.15;

/// Various dimensions of button color labels.
/// In a separate module, because there are too many of them
pub mod color_labels {
	use super::*;

	/// How big the square labels should be in world units
	pub const SIZE: f32 = SPRITE_LENGTH * 0.3828125;
	/// Radius of rounded corners of the labels in world units
	pub const CORNER_RADIUS: f32 = SIZE * 0.1;
	/// Length of the arrow tip of the labels, if present, in world units
	pub const ARROW_TIP_LENGTH: f32 = SIZE / 3.0;
	/// Number of vertices used for rounded parts of label meshes
	pub const MESH_RESOLUTION: usize = 4;
	/// Vertical position of the center of the label, relative to the position
	/// of the entity, when it is inside the button, in world units
	pub const CENTER_Y_OFFSET: f32 = COLOR_SPRITE_OFFSET;
	/// Width of the gap meant to be between the button/box sprite and the label,
	/// in world units
	pub const GAP_WIDTH: f32 = SPRITE_LENGTH * 0.04;
	/// Half-width of the actual button sprite (without the padding), in world units
	pub const BUTTON_SPRITE_HALF_WIDTH: f32 = SPRITE_LENGTH * 0.33203125;
	/// How far from the center of the box should a square label be placed, in world units
	pub const OFFSET_SECONDARY_SQUARE: f32 = BUTTON_SPRITE_HALF_WIDTH + GAP_WIDTH + SIZE / 2.0;
	/// How far from the center of the box should an arrow label be placed, in world units
	pub const OFFSET_SECONDARY_ARROW: f32 = OFFSET_SECONDARY_SQUARE + ARROW_TIP_LENGTH;
	/// How far from the center of the box can a rotated square label be placed, in world units
	pub const MAX_ROTATED_DISPLACEMENT_SQUARE: f32 =
		SPRITE_LENGTH * 0.4453125 + GAP_WIDTH + SIZE / 2.0;
	/// How far from the center of the box can a rotated arrow label be placed, in world units
	pub const MAX_ROTATED_DISPLACEMENT_ARROW: f32 =
		MAX_ROTATED_DISPLACEMENT_SQUARE + ARROW_TIP_LENGTH;
}

/// Parameters of fade transition effects
pub mod fade {
	use super::*;

	/// Colour into which the screen fades during transitions
	pub const OVERLAY_COLOR: Color = Color::WHITE;

	/// The progress \[0, 1] of the animation at which the transition
	/// should be triggered (events are sent out).
	///
	/// [`fade_opacity_function`] should be 1 at this progress.
	pub const PEAK_OFFSET: f32 = 0.5;

	/// Calculates opacity of the overlay from animation progress \[0, 1]
	pub fn fade_opacity_function(progress: f32) -> f32 {
		const PEAK_OVERFLOW: f32 = 1.5;
		const SMOOTHING_DISTANCE: f32 = 0.1;
		// Linear ascent and descent, from 0 to `PEAK_OVERFLOW`
		let raw_opacity = (1.0 - (2.0 * progress - 1.0).abs()) * PEAK_OVERFLOW;
		// Quadratic smoothing
		let smoothing = 4.0 * SMOOTHING_DISTANCE;
		let h = (smoothing - (raw_opacity - 1.0).abs()).max(0.0) / smoothing;
		raw_opacity.min(1.0) - h * h * SMOOTHING_DISTANCE
	}
}

/// Velocity of the background in local background coordinates
pub const BACKGROUND_VELOCITY: Vec2 = Vec2::new(16.0, -8.0 * f32::consts::SQRT_2);
/// Period of background texture repetition, in background coordinates
pub const BACKGROUND_TILING: f32 = 400.0;
/// Angle by which the background texture should be inclined
pub const BACKGROUND_ROTATION: f32 = f32::consts::PI / 5.0;

/// Defines Z depth of various objects to layer them properly
pub mod layers {
	/// Z depth of the title screen illustration
	pub const TITLE_IMAGE: f32 = -10.0;
	/// Z depth of the secondary/backdrop title screen illistration
	pub const TITLE_BACKDROP: f32 = -20.0;
	/// Z depth of sprites that indicate logical color of boxes
	pub const BOX_COLOR_SPRITES: f32 = -8.0;
	/// Z depth of sprites for players and boxes
	pub const OBJECT_SPRITES: f32 = -10.0;
	/// Z depth of sprites that indicate logical color of buttons
	pub const BUTTON_COLOR_SPRITES: f32 = -48.0;
	/// Z depth of backdrop for labels that indicate logical color of buttons
	pub const BUTTON_COLOR_LABELS: f32 = -49.0;
	/// Z depth of sprites for flags and buttons
	pub const GLYPH_SPRITES: f32 = -50.0;
	/// Z depth of the bottom text box
	pub const HINT_TEXT_PANEL: f32 = -100.0;
	/// Z depth of meshes for perimeters of cycles that are currently selected
	pub const ACTIVE_CYCLE_RINGS: f32 = -150.0;
	/// Z depth of meshes for outlines of perimeters of cycles that are currently selected
	pub const ACTIVE_CYCLE_RING_OUTLINES: f32 = -180.0;
	/// Z depth of meshes for perimeters of cycles
	pub const CYCLE_RINGS: f32 = -200.0;
	/// Z depth of meshes for outlines of perimeters of cycles and vertex nodes
	pub const CYCLE_RING_OUTLINES: f32 = -220.0;
	/// Z depth of meshes for perimeters of cycles that cannot be selected
	pub const DISABLED_CYCLE_RINGS: f32 = -225.0;
	/// Z depth of meshes for outlines of perimeters of cycles that cannot be selected
	pub const DISABLED_CYCLE_RING_OUTLINES: f32 = -230.0;
	/// Z depth of arrow sprites at the center of cycles
	pub const CYCLE_CENTER_ARROWS: f32 = -250.0;
	/// Z depth of sprites for cycle centers
	pub const CYCLE_CENTER_SPRITES: f32 = -300.0;
	/// Z depth of meshes for cycle linkages
	pub const CYCLE_LINKS: f32 = -400.0;
	/// Z depth of the animated background
	pub const BACKGROUND: f32 = -550.0;
}
