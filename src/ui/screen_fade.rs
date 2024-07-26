//! An overlay which changes opacity in order to create a fading in/out effect for the game for screen transitions.

use crate::game::graphics::FADE_COLOUR;

use super::*;

pub(super) fn plugin(app: &mut App) {
	app.insert_resource(Fader {
		progress: 1.0,
		fade_time: 0.5,
		target: FadeTarget::FadeIn,
	})
	.add_systems(Startup, spawn_fader)
	.add_systems(Update, update_fader);
}

/// Marker component for the root node of HUD
#[derive(Component)]
pub struct ScreenFader;

/// Resource that controls the visibility of the HUD UI
#[derive(Resource)]
pub struct Fader {
	progress: f32,
	pub fade_time: f32,
	pub target: FadeTarget,
}

impl Fader {
	pub fn start_fade(&mut self, target: FadeTarget) {
		self.progress = 0.0;
		self.target = target;
	}

	pub fn progress(&self) -> f32 {
		self.progress
	}

	pub fn is_faded_in(&self) -> bool {
		self.is_complete() && self.target == FadeTarget::FadeIn
	}

	pub fn is_faded_out(&self) -> bool {
		self.is_complete() && self.target == FadeTarget::FadeOut
	}

	pub fn is_complete(&self) -> bool {
		self.progress >= 1.0
	}
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FadeTarget {
	FadeOut,
	FadeIn,
}

fn spawn_fader(mut commands: Commands) {
	commands.spawn((
		ScreenFader,
		NodeBundle {
			style: Style {
				width: Val::Percent(100.0),
				height: Val::Percent(100.0),
				..default()
			},
			background_color: BackgroundColor(FADE_COLOUR),
			z_index: ZIndex::Global(65000),
			..default()
		},
	));
}

fn update_fader(
	mut query: Query<&mut BackgroundColor, With<ScreenFader>>,
	mut fader: ResMut<Fader>,
	time: Res<Time<Real>>,
) {
	if fader.progress <= 1.0 {
		fader.progress += time.delta_seconds() / fader.fade_time;
		fader.progress = fader.progress.clamp(0.0, 1.0);
	}

	let (start_opacity, end_opacity) = match fader.target {
		FadeTarget::FadeOut => (0.0, 1.0),
		FadeTarget::FadeIn => (1.0, 0.0),
	};
	// TODO: Better curve
	let target_alpha = f32::lerp(start_opacity, end_opacity, fader.progress);

	for mut cover in query.iter_mut() {
		cover.0.set_alpha(target_alpha);
	}
}
