//! Tools for exploring the state space

mod graph;
mod presentation;

use crate::game::level::backend::builder::parse_and_run;
use graph::StateGraph;
use std::io::Write;

pub use graph::StateExplorerOptions;

pub fn run_state_explorer(
	level_source: &str,
	output: &mut impl Write,
	options: StateExplorerOptions,
) -> Result<(), String> {
	let loaded =
		parse_and_run(level_source, |_| {}).map_err(|e| format!("Could not build level: {e}"))?;
	loaded.errors.log_with_bevy();
	let graph = StateGraph::traverse_state_graph(&loaded.level, options);
	graph
		.wrap_in_html_page(output)
		.map_err(|e| format!("Could not save explorer result: {e}"))
}
