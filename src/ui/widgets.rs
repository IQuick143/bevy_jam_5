//! Helper traits for creating common widgets.

use bevy::{
	ecs::{relationship::RelatedSpawnerCommands, system::EntityCommands},
	prelude::*,
	ui::Val::*,
};

use super::{interaction::InteractionPalette, palette::*};

/// An extension trait for spawning UI widgets.
pub trait Widgets {
	/// Spawn a simple button with text.
	fn button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands<'_>;

	/// Spawn a compact button. Smaller than [`Widgets::button`].
	fn small_button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands<'_>;

	/// Spawn a very compact toolbar button. Smaller than [`Widgets::small_button`]
	fn tool_button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands<'_>;

	/// Spawn a very compact button. Its size matches that of [`Widgets::text`]
	fn inline_button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands<'_>;

	/// Spawn a simple header label. Bigger than [`Widgets::label`].
	fn header(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands<'_>;

	/// Spawn a simple text label.
	fn label(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands<'_>;

	/// Spawn an aligned text label with automatic width
	fn text(
		&mut self,
		text: impl Into<String>,
		align: JustifyContent,
		font: Handle<Font>,
	) -> EntityCommands<'_>;
}

impl<T: Spawn> Widgets for T {
	fn button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands<'_> {
		let mut entity = self.spawn_internal((
			Name::new("Button"),
			Button,
			Node {
				width: Px(200.0),
				height: Px(65.0),
				justify_content: JustifyContent::Center,
				align_items: AlignItems::Center,
				..default()
			},
			BackgroundColor(NODE_BACKGROUND),
			InteractionPalette {
				none: NODE_BACKGROUND,
				hovered: BUTTON_HOVERED_BACKGROUND,
				pressed: BUTTON_PRESSED_BACKGROUND,
			},
		));
		entity.with_children(|children| {
			children.spawn((
				Name::new("Button Text"),
				Text::new(text),
				TextFont {
					font_size: 40.0,
					font,
					..default()
				},
				TextColor(BUTTON_TEXT),
			));
		});
		entity
	}

	fn small_button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands<'_> {
		let mut entity = self.spawn_internal((
			Name::new("Button"),
			Button,
			Node {
				width: Px(200.0),
				height: Px(45.0),
				justify_content: JustifyContent::Center,
				align_items: AlignItems::Center,
				..default()
			},
			BackgroundColor(NODE_BACKGROUND),
			InteractionPalette {
				none: NODE_BACKGROUND,
				hovered: BUTTON_HOVERED_BACKGROUND,
				pressed: BUTTON_PRESSED_BACKGROUND,
			},
		));
		entity.with_children(|children| {
			children.spawn((
				Name::new("Button Text"),
				Text::new(text),
				TextFont {
					font_size: 30.0,
					font,
					..default()
				},
				TextColor(BUTTON_TEXT),
			));
		});
		entity
	}

	fn tool_button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands<'_> {
		let mut entity = self.spawn_internal((
			Name::new("Button"),
			Button,
			Node {
				padding: UiRect::all(Px(10.0)),
				align_items: AlignItems::Center,
				..default()
			},
			BackgroundColor(NODE_BACKGROUND),
			InteractionPalette {
				none: NODE_BACKGROUND,
				hovered: BUTTON_HOVERED_BACKGROUND,
				pressed: BUTTON_PRESSED_BACKGROUND,
			},
		));
		entity.with_children(|children| {
			children.spawn((
				Name::new("Button Text"),
				Text::new(text),
				TextFont {
					font_size: 30.0,
					font,
					..default()
				},
				TextColor(BUTTON_TEXT),
			));
		});
		entity
	}

	fn inline_button(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands<'_> {
		let mut entity = self.spawn_internal((
			Name::new("Button"),
			Button,
			Node {
				padding: UiRect::axes(Px(10.0), Px(5.0)),
				align_items: AlignItems::Center,
				..default()
			},
			BackgroundColor(NODE_BACKGROUND),
			InteractionPalette {
				none: NODE_BACKGROUND,
				hovered: BUTTON_HOVERED_BACKGROUND,
				pressed: BUTTON_PRESSED_BACKGROUND,
			},
		));
		entity.with_children(|children| {
			children.spawn((
				Name::new("Button Text"),
				Text::new(text),
				TextFont {
					font_size: 24.0,
					font,
					..default()
				},
				TextColor(BUTTON_TEXT),
			));
		});
		entity
	}

	fn header(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands<'_> {
		let mut entity = self.spawn_internal((
			Name::new("Header"),
			Node {
				width: Px(500.0),
				height: Px(65.0),
				justify_content: JustifyContent::Center,
				align_items: AlignItems::Center,
				..default()
			},
			BackgroundColor(NODE_BACKGROUND),
		));
		entity.with_children(|children| {
			children.spawn((
				Name::new("Header Text"),
				Text::new(text),
				TextFont {
					font_size: 40.0,
					font,
					..default()
				},
				TextColor(HEADER_TEXT),
			));
		});
		entity
	}

	fn label(&mut self, text: impl Into<String>, font: Handle<Font>) -> EntityCommands<'_> {
		let mut entity = self.spawn_internal((
			Name::new("Label"),
			Node {
				width: Px(500.0),
				justify_content: JustifyContent::Center,
				align_items: AlignItems::Center,
				..default()
			},
		));
		entity.with_children(|children| {
			children.spawn((
				Name::new("Label Text"),
				Text::new(text),
				TextFont {
					font_size: 24.0,
					font,
					..default()
				},
				TextColor(LABEL_TEXT),
			));
		});
		entity
	}

	fn text(
		&mut self,
		text: impl Into<String>,
		align: JustifyContent,
		font: Handle<Font>,
	) -> EntityCommands<'_> {
		let mut entity = self.spawn_internal((
			Name::new("Text"),
			Node {
				justify_content: align,
				align_items: AlignItems::Center,
				..default()
			},
		));
		entity.with_children(|children| {
			children.spawn((
				Name::new("Text Content"),
				Text::new(text),
				TextFont {
					font_size: 24.0,
					font,
					..default()
				},
				TextColor(LABEL_TEXT),
			));
		});
		entity
	}
}

/// An extension trait for spawning UI containers.
pub trait Containers {
	/// Spawns a root node that covers the full screen
	/// and centers its content horizontally and vertically.
	fn ui_root(&mut self) -> EntityCommands<'_> {
		self.ui_root_justified(JustifyContent::Center)
	}

	fn ui_root_justified(&mut self, justify_content: JustifyContent) -> EntityCommands<'_>;
}

impl Containers for Commands<'_, '_> {
	fn ui_root_justified(&mut self, justify_content: JustifyContent) -> EntityCommands<'_> {
		self.spawn((
			Name::new("UI Root"),
			Node {
				width: Percent(100.0),
				height: Percent(100.0),
				justify_content,
				align_items: AlignItems::Center,
				flex_direction: FlexDirection::Column,
				row_gap: Px(10.0),
				position_type: PositionType::Absolute,
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
	fn spawn_internal<B: Bundle>(&mut self, bundle: B) -> EntityCommands<'_>;
}

impl Spawn for Commands<'_, '_> {
	fn spawn_internal<B: Bundle>(&mut self, bundle: B) -> EntityCommands<'_> {
		self.spawn(bundle)
	}
}

impl Spawn for RelatedSpawnerCommands<'_, ChildOf> {
	fn spawn_internal<B: Bundle>(&mut self, bundle: B) -> EntityCommands<'_> {
		self.spawn(bundle)
	}
}
