//! Global non-asset graphical repository

use crate::graphics::{primitives::*, *};
use bevy::{
	color::palettes::{self, tailwind::*},
	platform::collections::HashMap,
	prelude::*,
};

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<ThingPalette>()
		.init_resource::<GameObjectMaterials>()
		.init_resource::<GameObjectMeshes>()
		.add_systems(
			Update,
			update_color_keys.run_if(resource_changed::<ThingPalette>),
		);
}

/// Identifies a color used somewhere in the game
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum ColorKey {
	BoxBase,
	ButtonBase,
	ButtonTrigger,
	Player,
	GoalClosed,
	GoalOpen,
	CycleDisabled,
	CycleReady,
	CycleTrigger,
	LinkMultiplicityLabel,
	InvertedLinkMultiplicityLabel,
	WarningSign,
	Checkmark,
	Wall,
	Detector,
	CycleRingsReady,
	CycleRingsSelect,
	CycleRingsDisabled,
	CycleRingOutlines,
	CycleRingOutlinesDisabled,
	CycleHitboxes,
	LinkLines,
	ColoredButtonLabels,
}

/// Identifies a material used somewhere in the game
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum MaterialKey {
	/// Material for cycle rings and vertex dots
	CycleRingsReady,
	/// Material for rings of cycles that are currently selected
	CycleRingsSelect,
	/// Material for rings of cycles that are not selectable
	CycleRingsDisabled,
	/// Material for outlines of cycle rings and vertex dots
	CycleRingOutlines,
	/// Material for outlines of cycle rings and vertices that are not selectable
	CycleRingOutlinesDisabled,
	/// Material for shadows/fill of cycles that are currently selected
	CycleHitboxes,
	/// Material for lines that represent links between cycles
	LinkLines,
	/// Material for labels that show logical color of buttons
	ColoredButtonLabels,
}

/// Identifies a mesh used somewhere in the game
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum MeshKey {
	/// Mesh for vertex nodes
	Vertices,
	/// Mesh for outlines of vertex nodes
	VertexOutlines,
	/// Mesh for square labels that show logical colors of buttons
	SquareLabels,
	/// Mesh for arrow-like labels that show logical colors of buttons
	ArrowLabels,
	/// Mesh for tips of arrows that represent one-way links
	OneWayLinkTips,
	/// Mesh for back side of tips of arrows for one-way links
	/// with numeric multiplicity labels
	OneWayLinkBackheads,
}

impl MaterialKey {
	pub fn to_color_key(self) -> ColorKey {
		match self {
			Self::CycleRingsReady => ColorKey::CycleRingsReady,
			Self::CycleRingsSelect => ColorKey::CycleRingsSelect,
			Self::CycleRingsDisabled => ColorKey::CycleRingsDisabled,
			Self::CycleRingOutlines => ColorKey::CycleRingOutlines,
			Self::CycleRingOutlinesDisabled => ColorKey::CycleRingOutlinesDisabled,
			Self::CycleHitboxes => ColorKey::CycleHitboxes,
			Self::LinkLines => ColorKey::LinkLines,
			Self::ColoredButtonLabels => ColorKey::ColoredButtonLabels,
		}
	}
}

/// Contains handles to the materials used to render game objects that are visualized by meshes
#[derive(Resource, Debug, Clone, Deref, DerefMut)]
pub struct GameObjectMaterials(pub HashMap<MaterialKey, Handle<ColorMaterial>>);

impl FromWorld for GameObjectMaterials {
	fn from_world(world: &mut World) -> Self {
		let material_keys = [
			MaterialKey::CycleRingsReady,
			MaterialKey::CycleRingsSelect,
			MaterialKey::CycleRingsDisabled,
			MaterialKey::CycleRingOutlines,
			MaterialKey::CycleRingOutlinesDisabled,
			MaterialKey::CycleHitboxes,
			MaterialKey::LinkLines,
			MaterialKey::ColoredButtonLabels,
		];

		let palette = world.resource::<ThingPalette>();
		let material_to_color = material_keys.map(|key| (key, palette[&key.to_color_key()]));

		let mut materials = world.resource_mut::<Assets<ColorMaterial>>();
		Self(
			material_to_color
				.into_iter()
				.map(|(key, color)| (key, materials.add(ColorMaterial { color, ..default() })))
				.collect(),
		)
	}
}

/// Contains handles to meshes that are commonly used to render game objects
#[derive(Resource, Debug, Clone, Deref, DerefMut)]
pub struct GameObjectMeshes(pub HashMap<MeshKey, Handle<Mesh>>);

impl FromWorld for GameObjectMeshes {
	fn from_world(world: &mut World) -> Self {
		let mut meshes = world.resource_mut::<Assets<Mesh>>();

		Self(HashMap::from_iter([
			(
				MeshKey::Vertices,
				meshes.add(Circle::new(NODE_RADIUS).mesh()),
			),
			(
				MeshKey::VertexOutlines,
				meshes.add(Circle::new(NODE_RADIUS + RING_OUTLINE_WIDTH).mesh()),
			),
			(
				MeshKey::SquareLabels,
				meshes.add(
					RoundedRectangle::from(Rectangle::from_length(color_labels::SIZE))
						.corner_radius(color_labels::CORNER_RADIUS)
						.mesh()
						.resolution(color_labels::MESH_RESOLUTION),
				),
			),
			(
				MeshKey::ArrowLabels,
				meshes.add(
					RoundedPentagonArrow::from(Rectangle::from_length(color_labels::SIZE))
						.corner_radius(color_labels::CORNER_RADIUS)
						.tip_length(color_labels::ARROW_TIP_LENGTH)
						.mesh()
						.resolution(color_labels::MESH_RESOLUTION),
				),
			),
			(
				MeshKey::OneWayLinkTips,
				meshes.add(
					Capsule2d::new(CYCLE_LINK_WIDTH / 2.0, ONEWAY_LINK_TIP_LENGTH)
						.mesh()
						.resolution(ONEWAY_LINK_TIP_RESOLUTION),
				),
			),
			(
				MeshKey::OneWayLinkBackheads,
				meshes.add(
					Capsule2d::new(CYCLE_LINK_WIDTH / 2.0, ONEWAY_MULTILINK_BACKHEAD_LENGTH)
						.mesh()
						.resolution(ONEWAY_LINK_TIP_RESOLUTION),
				),
			),
		]))
	}
}

/// Contains colors used for rendering various visuals
#[derive(Resource, Clone, Debug, Deref, DerefMut)]
pub struct ThingPalette(pub HashMap<ColorKey, Color>);

impl Default for ThingPalette {
	fn default() -> Self {
		Self(HashMap::from_iter([
			(ColorKey::BoxBase, ORANGE_200.into()),
			(ColorKey::ButtonBase, Srgba::hex("CC5151").unwrap().into()),
			(ColorKey::ButtonTrigger, GREEN_300.into()),
			(ColorKey::Player, SLATE_200.into()),
			(ColorKey::GoalClosed, SLATE_100.into()),
			(ColorKey::GoalOpen, GREEN_500.into()),
			(ColorKey::CycleDisabled, SLATE_300.into()),
			(ColorKey::CycleReady, SLATE_300.into()),
			(ColorKey::CycleTrigger, SLATE_400.into()),
			(ColorKey::LinkMultiplicityLabel, SLATE_300.into()),
			(
				ColorKey::InvertedLinkMultiplicityLabel,
				Srgba::hex("F29FA7").unwrap().into(),
			),
			(ColorKey::WarningSign, RED_400.into()),
			(ColorKey::Checkmark, GREEN_300.into()),
			(ColorKey::Wall, Srgba::hex("DBACAB").unwrap().into()),
			(ColorKey::Detector, palettes::css::ORANGE.into()),
			(ColorKey::CycleRingsReady, SLATE_200.into()),
			(ColorKey::CycleRingsSelect, SLATE_400.into()),
			(ColorKey::CycleRingsDisabled, SLATE_100.into()),
			(ColorKey::CycleRingOutlines, SLATE_700.into()),
			(
				ColorKey::CycleRingOutlinesDisabled,
				Srgba::hex("94A3B8").unwrap().into(),
			),
			(
				ColorKey::CycleHitboxes,
				Srgba::hex("A3B7D1").unwrap().with_alpha(0.3).into(),
			),
			(ColorKey::LinkLines, SLATE_300.into()),
			(ColorKey::ColoredButtonLabels, Color::BLACK),
		]))
	}
}

fn update_color_keys(
	palette: Res<ThingPalette>,
	materials: Res<GameObjectMaterials>,
	mut material_assets: ResMut<Assets<ColorMaterial>>,
) {
	for (material_key, material_handle) in materials.iter() {
		if let Some(material) = material_assets.get_mut(material_handle) {
			material.color = palette[&material_key.to_color_key()];
		}
	}
}
