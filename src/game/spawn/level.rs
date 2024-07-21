//! Spawn the main level by triggering other observers.

use crate::game::{
	level::{CycleData, ValidLevelData, VertexData},
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
		.map(|data| {
			let vertex = spawn_vertex(commands.reborrow(), data);
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
}

fn spawn_vertex(mut commands: Commands, data: &VertexData) -> Entity {
	let mut rng = thread_rng();
	let vertex_id = commands
		.spawn((
			Vertex,
			PlacedGlyph(None),
			PlacedObject(None),
			Transform::from_translation(Vec3::new(
				rng.gen_range(-100.0..100.0),
				rng.gen_range(-100.0..100.0),
				0.0,
			)),
		))
		.id();

	if data.object.is_some() {
		let object_id = commands.spawn((Object, VertexPosition(vertex_id))).id();
		commands
			.entity(vertex_id)
			.insert(PlacedObject(Some(object_id)));
	}
	if data.glyph.is_some() {
		let glyph_id = commands.spawn((Glyph, VertexPosition(vertex_id))).id();
		commands
			.entity(vertex_id)
			.insert(PlacedGlyph(Some(glyph_id)));
	}

	vertex_id
}

fn spawn_cycle(mut commands: Commands, data: &CycleData, vertex_entities: &Vec<Entity>) -> Entity {
	let mut rng = thread_rng();
	commands
		.spawn((
			data.cycle_turnability,
			ComputedCycleTurnability(true),
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
