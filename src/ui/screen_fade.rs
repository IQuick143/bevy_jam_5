//! An overlay which changes opacity in order to create a fading in/out effect for the game for screen transitions.

use super::freeze::FreezeUi;
use crate::graphics::fade::*;
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		Update,
		(update_fade_animations, despawn_expired_fade_animations).chain(),
	);
}

/// Component that designates an entity as a fade-in/out animation.
///
/// When the animation is complete, the entity is despawned.
///
/// Do not spawn this directly, use [`FadeAnimationBundle`] instead.
///
/// Any event components on the entity that had been previously registered
/// with [`add_fade_event`](AddFadeEvent::add_fade_event) will be sent
/// via [`EventWriter`]s when the animation reaches maximum fade-in.
#[derive(Component, Clone, Copy, Debug, Reflect)]
pub struct FadeAnimation {
	/// Time taken by the full fade-in and -out, in seconds
	pub total_time: f32,
	/// Current progress of the animation, in seconds
	time_elapsed: f32,
	/// Progress of the animation on previous frame, in seconds.
	/// Used to figure out when we need to send out delayed events.
	prev_time_elapsed: f32,
}

impl FadeAnimation {
	pub fn new(total_time: f32) -> Self {
		Self {
			total_time,
			time_elapsed: 0.0,
			prev_time_elapsed: 0.0,
		}
	}

	/// Advances the elapsed time on the animation
	/// and updates previous frame timestamp
	pub fn add_elapsed_time(&mut self, delta_seconds: f32) {
		self.prev_time_elapsed = self.time_elapsed;
		self.time_elapsed += delta_seconds;
	}

	/// Relative progress of the animation \[0, 1]
	pub fn progress(&self) -> f32 {
		(self.time_elapsed / self.total_time).clamp(0.0, 1.0)
	}

	/// Relative progress of the animation on previous frame \[0, 1]
	pub fn prev_progress(&self) -> f32 {
		(self.prev_time_elapsed / self.total_time).clamp(0.0, 1.0)
	}

	pub fn is_completed(&self) -> bool {
		self.time_elapsed >= self.total_time
	}
}

impl Default for FadeAnimation {
	fn default() -> Self {
		Self {
			total_time: 1.0,
			time_elapsed: 0.0,
			prev_time_elapsed: 0.0,
		}
	}
}

/// [`FadeAnimation`] bundled with other components that commonly accompany it:
/// - [`Node`] that will be the transition overlay node.
/// - [`FreezeUi`] to disable the UI while the animation is playing.
#[derive(Bundle, Clone, Debug)]
pub struct FadeAnimationBundle {
	pub animation: FadeAnimation,
	pub node: Node,
	pub freeze: FreezeUi,
	pub background_color: BackgroundColor,
	pub z_index: ZIndex,
}

impl FadeAnimationBundle {
	pub fn from_time(total_time: f32) -> Self {
		Self {
			animation: FadeAnimation::new(total_time),
			..default()
		}
	}
}

impl Default for FadeAnimationBundle {
	fn default() -> Self {
		Self {
			animation: default(),
			node: Node {
				width: Val::Percent(100.0),
				height: Val::Percent(100.0),
				..default()
			},
			background_color: BackgroundColor(OVERLAY_COLOR.with_alpha(0.0)),
			z_index: ZIndex(65000),
			freeze: FreezeUi,
		}
	}
}

/// Extension trait for [`App`] to allow simple registration
/// of [`Event`]s that can be used with [`FadeAnimation`].
pub trait AddFadeEvent {
	/// Registers an [`Event`] type and enables forwarding
	/// of components of this type from [`FadeAnimation`] entities.
	fn add_fade_event<E: Event + Clone>(&mut self) -> &mut Self;
}

impl AddFadeEvent for App {
	fn add_fade_event<E: Event + Clone>(&mut self) -> &mut Self {
		self.add_event::<E>().add_systems(
			Update,
			send_delayed_fade_events::<E>
				.after(update_fade_animations)
				.before(despawn_expired_fade_animations),
		)
	}
}

fn update_fade_animations(
	mut query: Query<(&mut FadeAnimation, &mut BackgroundColor)>,
	time: Res<Time<Real>>,
) {
	for (mut animation, mut overlay) in &mut query {
		animation.add_elapsed_time(time.delta_secs());
		let alpha = fade_opacity_function(animation.progress());
		overlay.0.set_alpha(alpha);
	}
}

fn despawn_expired_fade_animations(mut commands: Commands, query: Query<(Entity, &FadeAnimation)>) {
	for (id, animation) in &query {
		if animation.is_completed() {
			commands.entity(id).despawn();
		}
	}
}

fn send_delayed_fade_events<E: Event + Clone>(
	mut events: EventWriter<E>,
	query: Query<(&FadeAnimation, &E)>,
) {
	for (animation, event) in &query {
		if animation.prev_progress() < PEAK_OFFSET && animation.progress() >= PEAK_OFFSET {
			events.send(event.clone());
		}
	}
}
