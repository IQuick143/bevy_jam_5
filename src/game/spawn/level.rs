//! Spawn the main level by triggering other observers.

use crate::game::{
	level::{CycleData, ValidLevelData},
	prelude::*,
};

use rand::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.observe(spawn_level);
}

#[derive(Event, Debug)]
pub struct SpawnLevel(pub ValidLevelData);

fn spawn_level(trigger: Trigger<SpawnLevel>, mut commands: Commands) {
	println!("Spawning!"); //TODO: debug
	let data = trigger.event().0 .0.clone();

	let vertices: Vec<Entity> = data
		.vertices
		.iter()
		.map(|_data| {
			let vertex = spawn_vertex(commands.reborrow());
			vertex
		})
		.collect();

	let _: Vec<()> = data
		.cycles
		.iter()
		.map(|data| {
			let _cycle = spawn_cycle(commands.reborrow(), data, &vertices);
		})
		.collect();

	()
}

fn spawn_vertex(mut commands: Commands) -> Entity {
	let mut rng = thread_rng();
	commands
		.spawn((
			Vertex,
			Transform::from_translation(Vec3::new(
				rng.gen_range(-100.0..100.0),
				rng.gen_range(-100.0..100.0),
				0.0,
			)),
		))
		.id()
}

fn spawn_cycle(mut commands: Commands, data: &CycleData, vertex_entities: &Vec<Entity>) -> Entity {
	let mut rng = thread_rng();
	commands
		.spawn((
			CycleVertices(
				data.vertex_indices
					.iter()
					.map(|i| *vertex_entities.get(*i).unwrap())
					.collect(),
			),
			Transform::from_translation(Vec3::new(
				rng.gen_range(-100.0..100.0),
				rng.gen_range(-100.0..100.0),
				0.0,
			)),
		))
		.id()
}
