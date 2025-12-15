//! Global non-asset graphical repository

use crate::graphics::{primitives::*, *};
use bevy::{
	color::palettes::{self, tailwind::*},
	prelude::*,
};

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<GameObjectMaterials>()
		.init_resource::<GameObjectMeshes>()
		.init_resource::<ThingPalette>();
}

/// Contains handles to the materials used to render game objects that are visualized by meshes
#[derive(Resource, Debug, Clone, Reflect)]
pub struct GameObjectMaterials {
	/// Material for cycle rings and vertex dots
	pub cycle_rings_ready: Handle<ColorMaterial>,
	/// Material for rings of cycles that are currently selected
	pub cycle_rings_select: Handle<ColorMaterial>,
	/// Material for rings of cycles that are not selectable
	pub cycle_rings_disabled: Handle<ColorMaterial>,
	/// Material for outlines of cycle rings and vertex dots
	pub cycle_ring_outlines: Handle<ColorMaterial>,
	/// Material for outlines of cycle rings and vertices that are not selectable
	pub cycle_ring_outlines_disabled: Handle<ColorMaterial>,
	/// Material for shadows/fill of cycles that are currently selected
	pub cycle_hitboxes: Handle<ColorMaterial>,
	/// Material for lines that represent links between cycles
	pub link_lines: Handle<ColorMaterial>,
	/// Material for labels that show logical color of buttons
	pub colored_button_labels: Handle<ColorMaterial>,
}

impl FromWorld for GameObjectMaterials {
	fn from_world(world: &mut World) -> Self {
		let mut materials = world.resource_mut::<Assets<ColorMaterial>>();

		let cycle_rings_ready = materials.add(ColorMaterial {
			color: SLATE_200.into(),
			..default()
		});
		let cycle_rings_select = materials.add(ColorMaterial {
			color: SLATE_400.into(),
			..default()
		});
		let cycle_rings_disabled = materials.add(ColorMaterial {
			color: SLATE_100.into(),
			..default()
		});
		let cycle_ring_outlines = materials.add(ColorMaterial {
			color: SLATE_700.into(),
			..default()
		});
		let cycle_ring_outlines_disabled = materials.add(ColorMaterial {
			// Roughly Tailwind Slate-350
			color: Srgba::hex("94A3B8").unwrap().into(),
			..default()
		});
		let cycle_hitboxes = materials.add(ColorMaterial {
			color: Srgba::hex("A3B7D1").unwrap().with_alpha(0.3).into(),
			..default()
		});
		let link_lines = materials.add(ColorMaterial {
			color: SLATE_300.into(),
			..default()
		});
		let colored_button_labels = materials.add(ColorMaterial {
			color: Color::BLACK,
			..default()
		});

		Self {
			cycle_rings_ready,
			cycle_rings_select,
			cycle_rings_disabled,
			cycle_ring_outlines,
			cycle_ring_outlines_disabled,
			cycle_hitboxes,
			link_lines,
			colored_button_labels,
		}
	}
}

/// Contains handles to meshes that are commonly used to render game objects
#[derive(Resource, Debug, Clone, Reflect)]
pub struct GameObjectMeshes {
	/// Mesh for vertex nodes
	pub vertices: Handle<Mesh>,
	/// Mesh for outlines of vertex nodes
	pub vertex_outlines: Handle<Mesh>,
	/// Mesh for square labels that show logical colors of buttons
	pub square_labels: Handle<Mesh>,
	/// Mesh for arrow-like labels that show logical colors of buttons
	pub arrow_labels: Handle<Mesh>,
	/// Mesh for tips of arrows that represent one-way links
	pub one_way_link_tips: Handle<Mesh>,
	/// Mesh for back side of tips of arrows for one-way links
	/// with numeric multiplicity labels
	pub one_way_link_backheads: Handle<Mesh>,
}

impl FromWorld for GameObjectMeshes {
	fn from_world(world: &mut World) -> Self {
		let mut meshes = world.resource_mut::<Assets<Mesh>>();

		let vertices = meshes.add(Circle::new(NODE_RADIUS).mesh());
		let vertex_outlines = meshes.add(Circle::new(NODE_RADIUS + RING_OUTLINE_WIDTH).mesh());
		let square_labels = meshes.add(
			RoundedRectangle::from(Rectangle::from_length(color_labels::SIZE))
				.corner_radius(color_labels::CORNER_RADIUS)
				.mesh()
				.resolution(color_labels::MESH_RESOLUTION),
		);
		let arrow_labels = meshes.add(
			RoundedPentagonArrow::from(Rectangle::from_length(color_labels::SIZE))
				.corner_radius(color_labels::CORNER_RADIUS)
				.tip_length(color_labels::ARROW_TIP_LENGTH)
				.mesh()
				.resolution(color_labels::MESH_RESOLUTION),
		);
		let one_way_link_tips = meshes.add(
			Capsule2d::new(CYCLE_LINK_WIDTH / 2.0, ONEWAY_LINK_TIP_LENGTH)
				.mesh()
				.resolution(ONEWAY_LINK_TIP_RESOLUTION),
		);
		let one_way_link_backheads = meshes.add(
			Capsule2d::new(CYCLE_LINK_WIDTH / 2.0, ONEWAY_MULTILINK_BACKHEAD_LENGTH)
				.mesh()
				.resolution(ONEWAY_LINK_TIP_RESOLUTION),
		);

		Self {
			vertices,
			vertex_outlines,
			square_labels,
			arrow_labels,
			one_way_link_tips,
			one_way_link_backheads,
		}
	}
}

/// Contains colors used for rendering objects and glyphs
#[derive(Resource, Debug, Clone, Reflect)]
pub struct ThingPalette {
	pub box_base: Color,
	pub button_base: Color,
	pub button_trigger: Color,
	pub player: Color,
	pub goal_closed: Color,
	pub goal_open: Color,
	pub cycle_disabled: Color,
	pub cycle_ready: Color,
	pub cycle_trigger: Color,
	pub link_multiplicity_label: Color,
	pub inverted_link_multiplicity_label: Color,
	pub warning_sign: Color,
	pub checkmark: Color,
	pub wall: Color,
	pub detector: Color,
}

impl Default for ThingPalette {
	fn default() -> Self {
		Self {
			box_base: ORANGE_200.into(),
			button_base: Srgba::hex("CC5151").unwrap().into(),
			button_trigger: GREEN_300.into(),
			player: SLATE_200.into(),
			goal_closed: SLATE_100.into(),
			goal_open: GREEN_500.into(),
			cycle_disabled: SLATE_300.into(),
			cycle_ready: SLATE_300.into(),
			cycle_trigger: SLATE_400.into(),
			link_multiplicity_label: SLATE_300.into(),
			inverted_link_multiplicity_label: Srgba::hex("F29FA7").unwrap().into(),
			warning_sign: RED_400.into(),
			checkmark: GREEN_300.into(),
			wall: Srgba::hex("DBACAB").unwrap().into(),
			detector: palettes::css::ORANGE.into(),
		}
	}
}
