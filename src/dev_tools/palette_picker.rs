//! Quick palette editing for developers

use crate::{
	drawing::{ColorKey, ThingPalette},
	ui::{char_input_pressed, interaction::InteractionQuery},
	AppSet,
};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.init_state::<SelectedColorKey>()
		.add_message::<ColorPickerButton>()
		.add_systems(Startup, spawn_picker_ui)
		.add_systems(
			Update,
			(
				toggle_color_picker_display.run_if(char_input_pressed('c')),
				collect_picker_button_inputs.in_set(AppSet::RecordInput),
				handle_picker_button_inputs.in_set(AppSet::ExecuteInput),
				update_picker_preview.in_set(AppSet::UpdateVisuals),
				update_key_button_colors
					.run_if(resource_changed::<ThingPalette>)
					.in_set(AppSet::UpdateVisuals),
				update_current_color_from_key.run_if(state_changed::<SelectedColorKey>),
				update_palette_entry,
			),
		);
}

#[derive(Component, Clone, Copy, Debug, Default)]
struct ColorPickerWidget;

#[derive(Component, Clone, Copy, Debug)]
struct ColorPickerCurrentColor {
	r: u8,
	g: u8,
	b: u8,
	a: u8,
}

impl Default for ColorPickerCurrentColor {
	fn default() -> Self {
		Self {
			r: 255,
			g: 255,
			b: 255,
			a: 255,
		}
	}
}

impl ColorPickerCurrentColor {
	fn to_srgba(self) -> Srgba {
		Srgba {
			red: self.r as f32 / 255.0,
			green: self.g as f32 / 255.0,
			blue: self.b as f32 / 255.0,
			alpha: self.a as f32 / 255.0,
		}
	}
}

fn yiq_lightness(color: Srgba) -> f32 {
	0.299 * color.red + 0.587 * color.green + 0.114 * color.blue
}

fn contrasting_text_color(background_color: Srgba) -> Color {
	if yiq_lightness(background_color) > 0.5 {
		Color::BLACK
	} else {
		Color::WHITE
	}
}

#[derive(Component, Message, Clone, Copy, PartialEq, Eq, Debug)]
enum ColorPickerButton {
	EditColor(ColorChannel, i8),
	ColorKey(ColorKey),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ColorChannel {
	Red,
	Green,
	Blue,
	Alpha,
}

#[derive(States, Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct SelectedColorKey(ColorKey);

impl Default for SelectedColorKey {
	fn default() -> Self {
		// It can be any key, we just need one
		Self(ColorKey::BoxBase)
	}
}

const KEY_PICKER_FONT_SIZE: f32 = 10.0;

const COLOR_MANIPULATOR_FONT_SIZE: f32 = 15.0;

const COLOR_MANIPULATOR_PADDING: UiRect = UiRect::horizontal(Val::Px(5.0));

const COLOR_PICKER_PADDING: Val = Val::Px(5.0);

fn spawn_picker_ui(mut commands: Commands, palette: Res<ThingPalette>) {
	let root = commands
		.spawn((
			Node {
				display: Display::None,
				align_items: AlignItems::End,
				width: Val::Percent(100.0),
				height: Val::Percent(100.0),
				padding: UiRect::all(COLOR_PICKER_PADDING),
				column_gap: COLOR_PICKER_PADDING,
				..default()
			},
			ColorPickerWidget,
		))
		.id();

	let key_container = commands
		.spawn((
			Node {
				flex_grow: 1.0,
				flex_wrap: FlexWrap::Wrap,
				column_gap: COLOR_PICKER_PADDING,
				row_gap: COLOR_PICKER_PADDING,
				..default()
			},
			ChildOf(root),
		))
		.id();

	let mut key_to_color = palette.iter().map(|(k, v)| (*k, *v)).collect::<Vec<_>>();
	key_to_color.sort_by_key(|(key, _)| *key);
	for (key, color) in key_to_color {
		commands.spawn((
			Button,
			Text::new(format!("{key:?}")),
			TextFont {
				font_size: KEY_PICKER_FONT_SIZE,
				..default()
			},
			ColorPickerButton::ColorKey(key),
			TextColor(contrasting_text_color(color.to_srgba())),
			BackgroundColor(color),
			ChildOf(key_container),
		));
	}

	commands.spawn((
		Node {
			flex_direction: FlexDirection::Column,
			..default()
		},
		ChildOf(root),
		children![
			(
				Node::default(),
				children![
					color_manipulator_button("+", ColorChannel::Red, 1),
					color_manipulator_button("++", ColorChannel::Red, 16),
					color_manipulator_button("+", ColorChannel::Green, 1),
					color_manipulator_button("++", ColorChannel::Green, 16),
					color_manipulator_button("+", ColorChannel::Blue, 1),
					color_manipulator_button("++", ColorChannel::Blue, 16),
					color_manipulator_button("+", ColorChannel::Alpha, 1),
					color_manipulator_button("++", ColorChannel::Alpha, 16),
				],
			),
			(Text::default(), ColorPickerCurrentColor::default()),
			(
				Node::default(),
				children![
					color_manipulator_button("-", ColorChannel::Red, -1),
					color_manipulator_button("--", ColorChannel::Red, -16),
					color_manipulator_button("-", ColorChannel::Green, -1),
					color_manipulator_button("--", ColorChannel::Green, -16),
					color_manipulator_button("-", ColorChannel::Blue, -1),
					color_manipulator_button("--", ColorChannel::Blue, -16),
					color_manipulator_button("-", ColorChannel::Alpha, -1),
					color_manipulator_button("--", ColorChannel::Alpha, -16),
				],
			),
		],
	));
}

fn color_manipulator_button(
	label: impl Into<String>,
	channel: ColorChannel,
	amount: i8,
) -> impl Bundle {
	let (color, text_color) = match channel {
		ColorChannel::Red => (Srgba::RED.into(), Color::WHITE),
		ColorChannel::Green => (Srgba::GREEN.into(), Color::WHITE),
		ColorChannel::Blue => (Srgba::BLUE.into(), Color::WHITE),
		ColorChannel::Alpha => (Srgba::WHITE.into(), Color::BLACK),
	};
	(
		Button,
		Node {
			padding: COLOR_MANIPULATOR_PADDING,
			..default()
		},
		BackgroundColor(color),
		ColorPickerButton::EditColor(channel, amount),
		children![(
			Text::new(label),
			TextColor(text_color),
			TextFont {
				font_size: COLOR_MANIPULATOR_FONT_SIZE,
				..default()
			},
		)],
	)
}

fn toggle_color_picker_display(mut query: Query<&mut Node, With<ColorPickerWidget>>) {
	for mut node in &mut query {
		node.display = if node.display == Display::None {
			Display::Flex
		} else {
			Display::None
		}
	}
}

fn collect_picker_button_inputs(
	query: InteractionQuery<&ColorPickerButton>,
	mut writer: MessageWriter<ColorPickerButton>,
) {
	for (interaction, enabled, action) in &query {
		if enabled.is_none_or(|x| **x) && *interaction == Interaction::Pressed {
			writer.write(*action);
		}
	}
}

fn handle_picker_button_inputs(
	mut reader: MessageReader<ColorPickerButton>,
	mut new_selected_color: ResMut<NextState<SelectedColorKey>>,
	mut current_color: Single<&mut ColorPickerCurrentColor>,
) {
	for action in reader.read() {
		match action {
			ColorPickerButton::EditColor(channel, delta) => {
				let target = match channel {
					ColorChannel::Red => &mut current_color.r,
					ColorChannel::Green => &mut current_color.g,
					ColorChannel::Blue => &mut current_color.b,
					ColorChannel::Alpha => &mut current_color.a,
				};
				*target = target.saturating_add_signed(*delta);
			}
			ColorPickerButton::ColorKey(color_key) => {
				new_selected_color.set(SelectedColorKey(*color_key))
			}
		}
	}
}

fn update_picker_preview(
	mut query: Query<
		(
			&ColorPickerCurrentColor,
			&mut TextColor,
			&mut BackgroundColor,
			&mut Text,
		),
		Changed<ColorPickerCurrentColor>,
	>,
) {
	for (current, mut text, mut background, mut text_content) in &mut query {
		let current = current.to_srgba();
		background.0 = current.into();
		**text = contrasting_text_color(current);
		**text_content = current.to_hex();
	}
}

fn update_key_button_colors(
	mut query: Query<(&ColorPickerButton, &mut TextColor, &mut BackgroundColor)>,
	palette: Res<ThingPalette>,
) {
	for (button, mut text, mut background) in &mut query {
		if let ColorPickerButton::ColorKey(key) = button {
			let color = palette[key];
			background.0 = color;
			**text = contrasting_text_color(color.to_srgba());
		}
	}
}

fn update_current_color_from_key(
	key: Res<State<SelectedColorKey>>,
	palette: Res<ThingPalette>,
	mut query: Query<&mut ColorPickerCurrentColor>,
) {
	for mut color in &mut query {
		let current = palette[&key.get().0].to_srgba();
		color.r = (current.red * 255.0) as u8;
		color.g = (current.green * 255.0) as u8;
		color.b = (current.blue * 255.0) as u8;
		color.a = (current.alpha * 255.0) as u8;
	}
}

fn update_palette_entry(
	key: Res<State<SelectedColorKey>>,
	query: Query<&ColorPickerCurrentColor, Changed<ColorPickerCurrentColor>>,
	mut palette: ResMut<ThingPalette>,
) {
	if let Some(current) = query.iter().last() {
		let current = current.to_srgba();
		palette.insert(key.get().0, current.into());
	}
}
