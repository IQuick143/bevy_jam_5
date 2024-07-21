use super::prelude::*;

pub fn plugin(app: &mut App) {
	app.observe(cycle_rotation_observer);
}

fn cycle_rotation_observer(
	event: Trigger<RotateCycle>,
	cycles_q: Query<&CycleVertices>,
	mut vertices_q: Query<&mut PlacedObject>,
	mut objects_q: Query<&mut VertexPosition>,
) {
	if let Ok(cycle_vertices) = cycles_q
		.get(event.entity())
		.inspect_err(|e| log::warn!("{e}"))
	{
		// Visit the first vertex again at the end to close the loop
		let vertex_ids_wrapped = cycle_vertices
			.0
			.iter()
			.copied()
			.chain(std::iter::once(cycle_vertices.0[0]));
		// Messy but simple way of chosing the iterator direction at run time
		// https://stackoverflow.com/a/52064434
		let vertex_ids_wrapped = match event.event() {
			RotateCycle::Nominal => Some(vertex_ids_wrapped)
				.into_iter()
				.flatten()
				.chain(None.into_iter().flatten()),
			RotateCycle::Reverse => None
				.into_iter()
				.flatten()
				.chain(Some(vertex_ids_wrapped.rev()).into_iter().flatten()),
		};
		let mut cached_object_id = None;
		for vertex_id in vertex_ids_wrapped {
			if let Some(cached_object_id) = cached_object_id {
				if let Ok(mut object_pos) = objects_q
					.get_mut(cached_object_id)
					.inspect_err(|e| log::warn!("{e}"))
				{
					object_pos.0 = vertex_id;
				}
			}
			if let Ok(mut placed_object_id) = vertices_q
				.get_mut(vertex_id)
				.inspect_err(|e| log::warn!("{e}"))
			{
				std::mem::swap(&mut cached_object_id, &mut placed_object_id.0);
			}
		}
	}
}
