//! Development tools for the game. This plugin is only enabled in dev builds.

use crate::game::prelude::*;
use bevy::{color::palettes, dev_tools::states::log_transitions, utils::hashbrown::HashMap};

use crate::screen::Screen;

pub(super) fn plugin(app: &mut App) {
	// Print state transitions in dev builds
	app.add_systems(Update, log_transitions::<Screen>);
//	app.add_systems(Update, simulate_vertices);
	app.add_systems(Update, gizmo_draw);
}

pub fn gizmo_draw(
	vertices: Query<&Transform, With<Vertex>>,
	circles: Query<(&CycleVertices, &Transform)>,
	mut gizmos: Gizmos,
) {
	for transform in vertices.iter() {
		gizmos.sphere(
			transform.translation,
			Quat::IDENTITY,
			1.0,
			palettes::tailwind::BLUE_300,
		);
	}

	for (vertex_ids, circle_transform) in circles.iter() {
		gizmos.sphere(
			circle_transform.translation,
			Quat::IDENTITY,
			1.0,
			palettes::tailwind::AMBER_100,
		);
		let mut positions: Vec<Vec3> = vertex_ids
			.0
			.iter()
			.map(|entity| vertices.get(*entity).unwrap().translation)
			.collect();
		positions.push(positions[0]);
		let spline = CubicCardinalSpline::new(0.5, positions).to_curve();
		let samples = spline.iter_positions(32);
		gizmos.linestrip(samples, palettes::tailwind::AMBER_900)
	}
}
