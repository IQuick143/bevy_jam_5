//! Spawn the main level by triggering other observers.

use crate::game::{
	level::{
		layout::{CyclePlacement, LevelLayout},
		CycleData, GlyphType, ObjectType, ValidLevelData, VertexData,
	},
	prelude::*,
};

use itertools::Itertools;

pub(super) fn plugin(app: &mut App) {
	app.observe(spawn_level);
}

#[derive(Event, Debug)]
pub struct SpawnLevel(pub ValidLevelData, pub LevelLayout);

fn spawn_level(
	trigger: Trigger<SpawnLevel>,
	mut events: EventWriter<GameLayoutChanged>,
	mut commands: Commands,
) {
	println!("Spawning!"); //TODO: debug
	let data = trigger.event().0.clone();
	let layout = &trigger.event().1;

	let vertices: Vec<Entity> = data
		.vertices
		.iter()
		.zip_eq(&layout.vertices)
		.map(|(data, pos)| spawn_vertex(commands.reborrow(), data, *pos))
		.collect();

	let cycle_ids = data
		.cycles
		.iter()
		.zip_eq(&layout.cycles)
		.map(|(data, pos)| spawn_cycle(commands.reborrow(), data, *pos, &vertices))
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

fn spawn_vertex(mut commands: Commands, data: &VertexData, position: Vec2) -> Entity {
	let vertex_id = commands
		.spawn((
			Vertex,
			PlacedGlyph(None),
			PlacedObject(None),
			Transform::from_translation(position.extend(0.0)),
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

fn spawn_cycle(
	mut commands: Commands,
	data: &CycleData,
	placement: CyclePlacement,
	vertex_entities: &[Entity],
) -> Entity {
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
			Transform::from_translation(placement.position.extend(0.0)),
		))
		.id()
}
