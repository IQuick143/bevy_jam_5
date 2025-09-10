//! Defines the presentation of level state graphs

use super::graph::{GameStateAttributes, StateGraph};
use crate::game::{level::ObjectData, logic::GameState};
use std::io::Write;

impl GameState {
	fn state_id(&self) -> String {
		let mut buf = std::io::BufWriter::new(Vec::new());
		for object in &self.objects {
			match object {
				None => write!(buf, "n").unwrap(),
				Some(ObjectData::Player) => write!(buf, "p").unwrap(),
				Some(ObjectData::Box(None)) => write!(buf, "b").unwrap(),
				Some(ObjectData::Box(Some(color))) => {
					if color.is_pictogram {
						write!(buf, "c{}", color.color_index).unwrap();
					} else {
						write!(buf, "b{}", color.color_index).unwrap();
					}
				}
			}
		}
		let bytes = buf.into_inner().unwrap();
		String::from_utf8(bytes).unwrap()
	}
}

impl StateGraph {
	const PAGE_HEADER: &str = r#"<!doctype html>
<html>
    <head>
        <!-- https://github.com/vasturiano/3d-force-graph -->
        <script src="https://cdn.jsdelivr.net/npm/3d-force-graph"></script>
        <script>
            addEventListener('load', () => {
                const main = document.getElementById('main')
                const graph = new ForceGraph3D(main);
                graph.linkOpacity(0.5);
                graph.graphData("#;

	const PAGE_FOOTER: &str = r#");
            });
        </script>
        <style>
            body, html {
                margin: 0;
            }
            #main {
                height: 100%;
            }
        </style>
    </head>
    <body>
        <div id="main"></div>
    </body>
</html>"#;

	pub fn wrap_in_html_page(&self, writer: &mut impl Write) -> std::io::Result<()> {
		writeln!(writer, "{}{{\n\tnodes: [", Self::PAGE_HEADER)?;
		for (node, attrs) in &self.reachable_states {
			let id = node.state_id();
			writeln!(
				writer,
				"\t\t{{ id: '{id}', name: '{id}', color: '{}' }},",
				state_attributes_to_color(attrs)
			)?;
		}
		writeln!(writer, "\t],\n\tlinks: [")?;
		for ([from, to], group) in &self.moves {
			writeln!(
				writer,
				"\t\t{{ source: '{}', target: '{}', color: '{}' }},",
				from.state_id(),
				to.state_id(),
				group_index_to_color(*group)
			)?;
		}
		writeln!(writer, "\t]\n}}{}", Self::PAGE_FOOTER)?;
		Ok(())
	}
}

fn group_index_to_color(group: usize) -> &'static str {
	const GROUP_TO_COLOR: &[&str] = &[
		"white", "#6bb1cf", "#ea9fd1", "#80FFE8", "#FF3A20", "#d7ef86", "#29339B", "#FF8552",
		"#75ed8f", "#a57de1",
	];
	GROUP_TO_COLOR[group % GROUP_TO_COLOR.len()]
}

fn state_attributes_to_color(attrs: &GameStateAttributes) -> &'static str {
	if attrs.is_initial {
		"red"
	} else if attrs.is_solution {
		"lime"
	} else if attrs.is_closed {
		""
	} else {
		"darkslateblue"
	}
}
