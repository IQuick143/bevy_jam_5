//! Helper traits for creating common widgets.

use bevy::{ecs::system::EntityCommands, prelude::*, ui::Val::*};

use super::{interaction::InteractionPalette, palette::*};

/// An extension trait for spawning UI widgets.
pub trait Widgets {
	/// Spawn a simple button with text.
	fn button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands;

	/// Spawn a compact button. Smaller than [`Widgets::button`].
	fn small_button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands;

	/// Spawn a very compact toolbar button. Smaller than [`Widgets::small_button`]
	fn tool_button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands;

	/// Spawn a simple header label. Bigger than [`Widgets::label`].
	fn header(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands;

	/// Spawn a simple text label.
	fn label(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands;

	/// Spawn an aligned text label with automatic width
	fn text(
		&mut self,
		text: impl Into<String>,
		align: JustifyContent,
		font: Handle<Font>,
	) -> EntityCommands;
}

impl<T: Spawn> Widgets for T {
	fn button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands {
		let mut entity = self.spawn((
			Name::new("Button"),
			ButtonBundle {
				style: Style {
					width: Px(200.0),
					height: Px(65.0),
					justify_content: JustifyContent::Center,
					align_items: AlignItems::Center,
					..default()
				},
				background_color: BackgroundColor(NODE_BACKGROUND),
				..default()
			},
			InteractionPalette {
				none: NODE_BACKGROUND,
				hovered: BUTTON_HOVERED_BACKGROUND,
				pressed: BUTTON_PRESSED_BACKGROUND,
			},
		));
		entity.with_children(|children| {
			children.spawn((
				Name::new("Button Text"),
				TextBundle::from_section(
					text,
					TextStyle {
						font_size: 40.0,
						color: BUTTON_TEXT,
						font,
					},
				),
			));
		});
		entity
	}

	fn small_button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands {
		let mut entity = self.spawn((
			Name::new("Button"),
			ButtonBundle {
				style: Style {
					width: Px(200.0),
					height: Px(45.0),
					justify_content: JustifyContent::Center,
					align_items: AlignItems::Center,
					..default()
				},
				background_color: BackgroundColor(NODE_BACKGROUND),
				..default()
			},
			InteractionPalette {
				none: NODE_BACKGROUND,
				hovered: BUTTON_HOVERED_BACKGROUND,
				pressed: BUTTON_PRESSED_BACKGROUND,
			},
		));
		entity.with_children(|children| {
			children.spawn((
				Name::new("Button Text"),
				TextBundle::from_section(
					text,
					TextStyle {
						font_size: 30.0,
						color: BUTTON_TEXT,
						font,
					},
				),
			));
		});
		entity
	}

	fn tool_button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands {
		let mut entity = self.spawn((
			Name::new("Button"),
			ButtonBundle {
				style: Style {
					padding: UiRect::all(Px(10.0)),
					align_items: AlignItems::Center,
					..default()
				},
				background_color: BackgroundColor(NODE_BACKGROUND),
				..default()
			},
			InteractionPalette {
				none: NODE_BACKGROUND,
				hovered: BUTTON_HOVERED_BACKGROUND,
				pressed: BUTTON_PRESSED_BACKGROUND,
			},
		));
		entity.with_children(|children| {
			children.spawn((
				Name::new("Button Text"),
				TextBundle::from_section(
					text,
					TextStyle {
						font_size: 30.0,
						color: BUTTON_TEXT,
						font,
					},
				),
			));
		});
		entity
	}

	fn header(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands {
		let mut entity = self.spawn((
			Name::new("Header"),
			NodeBundle {
				style: Style {
					width: Px(500.0),
					height: Px(65.0),
					justify_content: JustifyContent::Center,
					align_items: AlignItems::Center,
					..default()
				},
				background_color: BackgroundColor(NODE_BACKGROUND),
				..default()
			},
		));
		entity.with_children(|children| {
			children.spawn((
				Name::new("Header Text"),
				TextBundle::from_section(
					text,
					TextStyle {
						font_size: 40.0,
						color: HEADER_TEXT,
						font,
					},
				),
			));
		});
		entity
	}

	fn label(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands {
		let mut entity = self.spawn((
			Name::new("Label"),
			NodeBundle {
				style: Style {
					width: Px(500.0),
					justify_content: JustifyContent::Center,
					align_items: AlignItems::Center,
					..default()
				},
				..default()
			},
		));
		entity.with_children(|children| {
			children.spawn((
				Name::new("Label Text"),
				TextBundle::from_section(
					text,
					TextStyle {
						font_size: 24.0,
						color: LABEL_TEXT,
						font,
					},
				),
			));
		});
		entity
	}

	fn text(
		&mut self,
		text: impl Into<String>,
		align: JustifyContent,
		font: Handle<Font>,
	) -> EntityCommands {
		let mut entity = self.spawn((
			Name::new("Text"),
			NodeBundle {
				style: Style {
					justify_content: align,
					align_items: AlignItems::Center,
					..default()
				},
				..default()
			},
		));
		entity.with_children(|children| {
			children.spawn((
				Name::new("Text Content"),
				TextBundle::from_section(
					text,
					TextStyle {
						font_size: 24.0,
						color: LABEL_TEXT,
						font,
					},
				),
			));
		});
		entity
	}
}

/// An extension trait for spawning UI containers.
pub trait Containers {
	/// Spawns a root node that covers the full screen
	/// and centers its content horizontally and vertically.
	fn ui_root(&mut self) -> EntityCommands {
		self.ui_root_justified(JustifyContent::Center)
	}

	fn ui_root_justified(&mut self, justify_content: JustifyContent) -> EntityCommands;
}

impl Containers for Commands<'_, '_> {
	fn ui_root_justified(&mut self, justify_content: JustifyContent) -> EntityCommands {
		self.spawn((
			Name::new("UI Root"),
			NodeBundle {
				style: Style {
					width: Percent(100.0),
					height: Percent(100.0),
					justify_content,
					align_items: AlignItems::Center,
					flex_direction: FlexDirection::Column,
					row_gap: Px(10.0),
					position_type: PositionType::Absolute,
					..default()
				},
				..default()
			},
		))
	}
}

/// An internal trait for types that can spawn entities.
/// This is here so that [`Widgets`] can be implemented on all types that
/// are able to spawn entities.
/// Ideally, this trait should be [part of Bevy itself](https://github.com/bevyengine/bevy/issues/14231).
trait Spawn {
	fn spawn<B: Bundle>(&mut self, bundle: B) -> EntityCommands;
}

impl Spawn for Commands<'_, '_> {
	fn spawn<B: Bundle>(&mut self, bundle: B) -> EntityCommands {
		self.spawn(bundle)
	}
}

impl Spawn for ChildBuilder<'_> {
	fn spawn<B: Bundle>(&mut self, bundle: B) -> EntityCommands {
		self.spawn(bundle)
	}
}
