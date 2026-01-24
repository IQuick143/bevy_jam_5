use crate::{camera::CameraHarness, screen::Screen};
use bevy::{
	math::bounding::{Aabb2d, BoundingCircle, BoundingVolume},
	prelude::*,
};

pub const BOX: &str = "A box. Can be moved around and placed onto buttons.";
pub const BUTTON: &str = "A button, needs a box.";
pub const FLAG: &str = "A flag, needs a player.";
pub const PLAYER: &str = "It's a you!";
pub const CYCLE_MANUAL: &str = "This cycle can only be turned while you stand on it.";
pub const CYCLE_AUTOMATIC: &str = "This cycle can be turned by left/right clicking its center.";
pub const CYCLE_STILL: &str = "This cycle cannot be turned on its own.";
pub const WALL: &str = "A wall, does not let objects through.";
pub const DETECTOR: &str = "A detector, triggers oneways when something goes through";

pub const UI_EXIT: &str = "Exit [Esc]";
pub const UI_RESET: &str = "Reset [R]";
pub const UI_UNDO: &str = "Undo [Z]";
pub const UI_NEXT: &str = "Next level [N]";

pub const BLOCKADE_WARNING: &str =
	"The last turn did not execute because multiple cycles tried to move this vertex, resulting in a conflict that jammed the system.";
pub const WALL_HIT_WARNING: &str =
	"The last turn did not execute because this wall would've been hit by an object on this cycle.";

/// Priority markers that indicate which hoverable entity
/// should be selected if the pointer overlaps more of them at once
pub mod prio {
	/// True UI
	pub const STATIC_UI: u32 = 10;
	/// World space UI
	pub const WORLD_UI: u32 = 20;
	/// Object sprites
	pub const OBJECT: u32 = 30;
	/// Detectors and walls
	pub const DETECTOR: u32 = 35;
	/// Glyph sprites
	pub const GLYPH: u32 = 40;
	/// Cycles
	pub const CYCLE: u32 = 50;
}

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<HintText>().add_systems(
		Update,
		(
			update_hover_state,
			update_hover_state_from_interaction,
			update_hover_text_cache,
			update_hover_text.run_if(resource_changed::<HintText>),
		),
	);
}

/// Marker component for a text node that displays the hover hint
#[derive(Component, Clone, Copy, Debug, Reflect)]
#[require(Text)]
pub struct HoverText;

/// Indicates whether an entity is currently hovered.
/// Not to be confused with [`bevy::picking::hover::Hovered`],
/// which has sliiightly different semantics. Most notably,
/// only one entity of some kinds can be [`IsHovered`] at once
/// (it is more of a matter of whether the entity is "selected").
///
/// Systems can update this component themselves to provide
/// customized hover detection for their entity kinds, or add
/// the [`HoverHintBoundingRect`] and [`HoverHintBoundingCircle`]
/// components to the entities that automatically update [`IsHovered`]
/// for entities in world space.
#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Default, Deref, DerefMut)]
pub struct IsHovered(pub bool);

/// Component that indicates that the entity provides a hint when hovered
#[derive(Component, Clone, Copy, Debug, Deref, DerefMut)]
#[require(IsHovered)]
pub struct HoverHint(pub &'static str);

/// If [`IsHovered`] entities overlap and multiple are hovered
/// simultaneously, the one with lowest [`HoverPriority`] is chosen
#[derive(Component, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Deref, DerefMut)]
pub struct HoverPriority(pub u32);

/// [`IsHovered`] entities whose hover hitbox is a rectangle
#[derive(Component, Clone, Copy, Debug, Deref, DerefMut)]
pub struct HoverHintBoundingRect(pub Aabb2d);

/// [`IsHovered`] entities whose hover hitbox is a circle
#[derive(Component, Clone, Copy, Debug, Deref, DerefMut)]
pub struct HoverHintBoundingCircle(pub BoundingCircle);

/// [`IsHovered`] entities whose hover state is comuted from [`Interaction`]
#[derive(Component, Clone, Copy, Debug, Default)]
#[require(IsHovered, Interaction)]
pub struct UseHoverFromInteraction;

/// Contains an overview of conditions that are needed to complete the level
#[derive(Resource, Debug, Clone, Reflect, Default)]
pub struct HintText {
	pub hover_text: Option<String>,
	pub hint_text: Option<String>,
}

fn update_hover_state_from_interaction(
	mut query: Query<
		(&mut IsHovered, &Interaction),
		(With<UseHoverFromInteraction>, Changed<Interaction>),
	>,
) {
	for (mut is_hovered, interaction) in &mut query {
		**is_hovered = *interaction != Interaction::None;
	}
}

fn update_hover_state(
	query: Query<
		(
			Entity,
			&GlobalTransform,
			Option<&HoverPriority>,
			Option<&HoverHintBoundingRect>,
			Option<&HoverHintBoundingCircle>,
		),
		Or<(With<HoverHintBoundingRect>, With<HoverHintBoundingCircle>)>,
	>,
	mut hovered_query: Query<
		(Entity, &mut IsHovered),
		Or<(With<HoverHintBoundingRect>, With<HoverHintBoundingCircle>)>,
	>,
	window: Single<&Window>,
	camera: Single<(&Camera, &GlobalTransform), With<CameraHarness>>,
) {
	let (camera, camera_transform) = *camera;
	let cursor_pos = window
		.cursor_position()
		.and_then(|p| camera.viewport_to_world_2d(camera_transform, p).ok());
	if let Some(cursor_pos) = cursor_pos {
		let mut closest_hoverable: Option<Entity> = None;
		let mut closest_priority_and_distance = (u32::MAX, f32::MAX);
		for (entity, transform, priority, bounding_rect, bounding_circle) in query.iter() {
			let translation = transform.translation();
			// This is a formula to extract the z rotation from the quaternion without trig functions
			// Trust me
			let rotation = transform.rotation();
			let rotation = Rot2 {
				cos: rotation.w,
				sin: rotation.z,
			}
			.try_normalize()
			.unwrap_or_default();
			let rotation = rotation * rotation;
			// Cursor position in local coordinates
			let transformed_cursor = rotation.inverse() * (cursor_pos - translation.xy());
			let priority = priority.copied().as_deref().copied().unwrap_or(u32::MAX);
			let mut hovered = false;
			let mut distance = f32::MAX;
			if let Some(bounding_circle) = bounding_circle {
				let circle_distance = (transformed_cursor - bounding_circle.center).length();
				if circle_distance < bounding_circle.radius() {
					hovered = true;
					distance = distance.min(circle_distance);
				}
			}
			if let Some(bounding_box) = bounding_rect {
				if Vec2::cmplt(bounding_box.min, transformed_cursor).all()
					&& Vec2::cmpgt(bounding_box.max, transformed_cursor).all()
				{
					hovered = true;
					// Evil hack approximation, does not actually compute the box inside distance
					let box_distance = (transformed_cursor - bounding_box.center())
						.abs()
						.max_element();
					distance = distance.min(box_distance);
				}
			}

			if hovered && (priority, distance) < closest_priority_and_distance {
				closest_priority_and_distance = (priority, distance);
				closest_hoverable = Some(entity);
			}
		}

		// Set the nearest entity we hit as hovered,
		// all others as not hovered
		for (id, mut is_hovered) in &mut hovered_query {
			**is_hovered = closest_hoverable == Some(id);
		}
	}
}

fn update_hover_text_cache(
	query: Query<(&HoverHint, &IsHovered, Option<&HoverPriority>)>,
	mut hint_text: ResMut<HintText>,
) {
	// Find the text on the entity with highest priority (lowest priority number)
	let hover_text = query
		.iter()
		.filter(|(_, is_hovered, _)| ***is_hovered)
		.min_by_key(|(_, _, prio)| prio.copied().unwrap_or(HoverPriority(u32::MAX)))
		.map(|(hint, _, _)| **hint);

	// Copy the string (and update) only if not equal
	if hint_text.hover_text.as_deref() != hover_text {
		hint_text.hover_text = hover_text.map(str::to_owned);
	}
}

fn update_hover_text(
	mut text_query: Query<(&mut Text, &mut Visibility), With<HoverText>>,
	hint_text: Res<HintText>,
	state: Res<State<Screen>>,
) {
	let should_be_visible = *state.get() == Screen::Playing;

	let chosen_text = match hint_text.clone() {
		HintText {
			hover_text: Some(hover),
			hint_text: _,
		} => hover,
		HintText {
			hover_text: None,
			hint_text: Some(hint),
		} => hint,
		_ => "".into(),
	};
	for (mut text, mut visibility) in text_query.iter_mut() {
		*visibility = match should_be_visible {
			true => Visibility::Inherited,
			false => Visibility::Hidden,
		};
		text.0.clone_from(&chosen_text);
	}
}
