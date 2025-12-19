//! Global non-asset graphical repository

use crate::{
	graphics::{primitives::*, *},
	AppSet,
};
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
			(
				update_material_colors.run_if(resource_changed::<ThingPalette>),
				(update_sprite_colors, update_node_colors).after(AppSet::UpdateVisuals),
			),
		);
}

/// Identifies a color used somewhere in the game
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
pub enum ColorKey {
	#[default]
	Blank,
	BoxBase,
	ButtonBase,
	ButtonTrigger,
	Player,
	GoalClosed,
	GoalOpen,
	CycleDisabled,
	CycleReady,
	CycleTrigger,
	CycleCentralArrow,
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
	NodeBackground,
	UiButtonHovered,
	UiButtonPressed,
	UiButtonDisabled,
	SpriteButton,
	SpriteButtonHovered,
	SpriteButtonPressed,
	SpriteButtonDisabled,
	NextLevelButton,
	NextLevelButtonHovered,
	NextLevelButtonPressed,
	NewLevelButton,
	SliderOutline,
	SliderFill,
	SliderHoveredFill,
	SliderPressedFill,
	SliderDisabledFill,
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

/// Support component for a [`Sprite`] that has a [`ColorKey`] color
#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Deref, DerefMut)]
pub struct SpriteColorKey(pub ColorKey);

/// Support component for a [`Node`] that has a [`ColorKey`] color
///
/// The color is applied to the [`Node`]'s [`ImageNode`] if it has one,
/// [`BackgroundColor`] otherwise
#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Deref, DerefMut)]
pub struct NodeColorKey(pub ColorKey);

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
			(ColorKey::Blank, Color::NONE),
			(ColorKey::BoxBase, ORANGE_200.into()),
			(ColorKey::ButtonBase, Srgba::hex("CC5151").unwrap().into()),
			(ColorKey::ButtonTrigger, GREEN_300.into()),
			(ColorKey::Player, SLATE_200.into()),
			(ColorKey::GoalClosed, SLATE_100.into()),
			(ColorKey::GoalOpen, GREEN_500.into()),
			(ColorKey::CycleDisabled, SLATE_300.into()),
			(ColorKey::CycleReady, SLATE_300.into()),
			(ColorKey::CycleTrigger, SLATE_400.into()),
			(ColorKey::CycleCentralArrow, SLATE_300.into()),
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
			(
				ColorKey::NewLevelButton,
				Color::Srgba(Srgba::rgb(0.4862745, 0.54509807, 0.6313726)),
			),
			(ColorKey::UiButtonHovered, Color::Srgba(SLATE_500)),
			(ColorKey::UiButtonPressed, Color::Srgba(SLATE_300)),
			(ColorKey::UiButtonDisabled, Color::Srgba(SLATE_300)),
			(ColorKey::NodeBackground, Color::Srgba(SLATE_400)),
			(ColorKey::SpriteButton, Color::Srgba(SLATE_200)),
			(ColorKey::SpriteButtonHovered, Color::Srgba(SLATE_400)),
			(ColorKey::SpriteButtonPressed, Color::Srgba(SLATE_500)),
			(
				ColorKey::SpriteButtonDisabled,
				Color::Srgba(Srgba {
					alpha: 0.5,
					..SLATE_200
				}),
			),
			(ColorKey::NextLevelButton, Color::Srgba(GREEN_400)),
			(ColorKey::NextLevelButtonHovered, Color::Srgba(GREEN_500)),
			(ColorKey::NextLevelButtonPressed, Color::Srgba(GREEN_600)),
			(ColorKey::SliderOutline, Color::Srgba(SLATE_700)),
			(ColorKey::SliderFill, Color::Srgba(SLATE_200)),
			(ColorKey::SliderHoveredFill, Color::Srgba(SLATE_400)),
			(ColorKey::SliderPressedFill, Color::Srgba(SLATE_100)),
			(ColorKey::SliderDisabledFill, Color::Srgba(SLATE_100)),
		]))
	}
}

fn update_material_colors(
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

fn update_sprite_colors(
	palette: Res<ThingPalette>,
	mut query: Query<(&mut Sprite, Ref<SpriteColorKey>)>,
) {
	for (mut sprite, color_key) in &mut query {
		if palette.is_changed() || color_key.is_changed() {
			sprite.color = palette[&**color_key];
		}
	}
}

fn update_node_colors(
	palette: Res<ThingPalette>,
	mut query: Query<(
		Option<&mut ImageNode>,
		&mut BackgroundColor,
		Ref<NodeColorKey>,
	)>,
) {
	for (node, mut background, color_key) in &mut query {
		if palette.is_changed() || color_key.is_changed() {
			if let Some(mut node) = node {
				node.color = palette[&**color_key];
			} else {
				background.0 = palette[&**color_key];
			}
		}
	}
}
