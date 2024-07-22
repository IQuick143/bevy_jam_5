//! Spawn the main level by triggering other observers.

use crate::game::{
	level::{CycleData, GlyphType, ObjectType, ValidLevelData, VertexData},
	prelude::*,
};

use rand::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.observe(spawn_level);
}

#[derive(Event, Debug)]
pub struct SpawnLevel(pub ValidLevelData);

fn spawn_level(
	trigger: Trigger<SpawnLevel>,
	mut events: EventWriter<GameLayoutChanged>,
	mut commands: Commands,
) {
	println!("Spawning!"); //TODO: debug
	let data = trigger.event().0.clone();

	let vertices: Vec<Entity> = data
		.vertices
		.iter()
		.map(|data| {
			let vertex = spawn_vertex(commands.reborrow(), data);
			vertex
		})
		.collect();

	let cycle_ids = data
		.cycles
		.iter()
		.map(|data| spawn_cycle(commands.reborrow(), data, &vertices))
		.collect::<Vec<_>>();

	for (i, cycle_id) in cycle_ids.iter().copied().enumerate() {
		let linked_cycles = data
			.cycles_linked_to(i)
			.map(|(j, dir)| (cycle_ids[j], dir))
			.collect::<Vec<_>>();
		if !linked_cycles.is_empty() {
			commands
				.entity(cycle_id)
				.insert(LinkedCycles(linked_cycles));
		}
	}

	commands.init_resource::<LevelCompletionConditions>();
	events.send(GameLayoutChanged);
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

	if let Some(object_type) = data.object {
		let object_id = match object_type {
			ObjectType::Player => commands
				.spawn((Object, Player, VertexPosition(vertex_id)))
				.id(),
			ObjectType::Box => commands
				.spawn((Object, Box, VertexPosition(vertex_id)))
				.id(),
		};
		commands
			.entity(vertex_id)
			.insert(PlacedObject(Some(object_id)));
	}
	if let Some(glyph_type) = data.glyph {
		let glyph_id = match glyph_type {
			GlyphType::Button => commands
				.spawn((Glyph, BoxSlot, VertexPosition(vertex_id)))
				.id(),
			GlyphType::Flag => commands
				.spawn((Glyph, Goal, VertexPosition(vertex_id)))
				.id(),
		};
		commands
			.entity(vertex_id)
			.insert(PlacedGlyph(Some(glyph_id)));
	}

	vertex_id
}

fn spawn_cycle(mut commands: Commands, data: &CycleData, vertex_entities: &[Entity]) -> Entity {
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
