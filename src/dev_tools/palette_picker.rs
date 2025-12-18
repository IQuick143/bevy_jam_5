//! Quick palette editing for developers

use crate::{
	drawing::{ColorKey, ThingPalette},
	ui::{
		char_input_pressed, interaction::InteractionQuery, palette::NODE_BACKGROUND, slider::Slider,
	},
	AppSet,
};
use bevy::prelude::*;

pub(super) fn plugin(app: &mut App) {
	app.init_state::<SelectedColorKey>()
		.add_message::<ColorPickerButton>()
		.init_resource::<ColorPickerClipboard>()
		.add_systems(Startup, spawn_picker_ui)
		.add_systems(
			Update,
			(
				toggle_color_picker_display.run_if(char_input_pressed('c')),
				collect_picker_button_inputs.in_set(AppSet::RecordInput),
				(handle_picker_button_inputs, handle_picker_slider_inputs)
					.in_set(AppSet::ExecuteInput),
				(update_picker_preview, update_slider_values).in_set(AppSet::UpdateVisuals),
				update_key_button_colors
					.run_if(resource_changed::<ThingPalette>)
					.in_set(AppSet::UpdateVisuals),
				update_current_color_from_key.run_if(state_changed::<SelectedColorKey>),
				update_palette_entry,
			),
		);
}

#[derive(Resource, Clone, Copy, PartialEq, Eq, Debug, Default, Deref, DerefMut)]
struct ColorPickerClipboard(SrgbaU8);

#[derive(Component, Clone, Copy, Debug, Default)]
struct ColorPickerWidget;

#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut)]
struct ColorPickerCurrentColor(SrgbaU8);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct SrgbaU8 {
	r: u8,
	g: u8,
	b: u8,
	a: u8,
}

impl Default for SrgbaU8 {
	fn default() -> Self {
		Self {
			r: 255,
			g: 255,
			b: 255,
			a: 255,
		}
	}
}

impl SrgbaU8 {
	fn from_srgba(color: Srgba) -> Self {
		Self {
			r: (color.red * 255.0) as u8,
			g: (color.green * 255.0) as u8,
			b: (color.blue * 255.0) as u8,
			a: (color.alpha * 255.0) as u8,
		}
	}

	fn to_srgba(self) -> Srgba {
		Srgba {
			red: self.r as f32 / 255.0,
			green: self.g as f32 / 255.0,
			blue: self.b as f32 / 255.0,
			alpha: self.a as f32 / 255.0,
		}
	}

	fn get_channel(&self, channel: ColorChannel) -> &u8 {
		match channel {
			ColorChannel::Red => &self.r,
			ColorChannel::Green => &self.g,
			ColorChannel::Blue => &self.b,
			ColorChannel::Alpha => &self.a,
		}
	}

	fn get_channel_mut(&mut self, channel: ColorChannel) -> &mut u8 {
		match channel {
			ColorChannel::Red => &mut self.r,
			ColorChannel::Green => &mut self.g,
			ColorChannel::Blue => &mut self.b,
			ColorChannel::Alpha => &mut self.a,
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
	Reset,
	Copy,
	Paste,
}

#[derive(Component, Message, Clone, Copy, PartialEq, Eq, Debug)]
struct ColorPickerSlider(ColorChannel);

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

const COLOR_MANIPULATOR_WIDTH: Val = Val::Px(400.0);

const COLOR_MANIPULATOR_PADDING: UiRect = UiRect::all(Val::Px(5.0));

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
			width: COLOR_MANIPULATOR_WIDTH,
			..default()
		},
		ChildOf(root),
		children![
			color_manipulator_slider(ColorChannel::Red),
			color_manipulator_slider(ColorChannel::Green),
			color_manipulator_slider(ColorChannel::Blue),
			color_manipulator_slider(ColorChannel::Alpha),
			(
				Node::default(),
				children![
					(
						Node {
							flex_grow: 1.0,
							..default()
						},
						Text::default(),
						ColorPickerCurrentColor::default()
					),
					(
						color_manipulator_button_base("R", Color::WHITE),
						BackgroundColor(NODE_BACKGROUND),
						ColorPickerButton::Reset,
					),
					(
						color_manipulator_button_base("C", Color::WHITE),
						BackgroundColor(NODE_BACKGROUND),
						ColorPickerButton::Copy,
					),
					(
						color_manipulator_button_base("V", Color::WHITE),
						BackgroundColor(NODE_BACKGROUND),
						ColorPickerButton::Paste,
					)
				]
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
		color_manipulator_button_base(label, text_color),
		BackgroundColor(color),
		ColorPickerButton::EditColor(channel, amount),
	)
}

fn color_manipulator_button_base(label: impl Into<String>, text_color: Color) -> impl Bundle {
	(
		Button,
		Node {
			padding: COLOR_MANIPULATOR_PADDING,
			..default()
		},
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

fn color_manipulator_slider(channel: ColorChannel) -> impl Bundle {
	(
		Node::default(),
		children![
			color_manipulator_button("<", channel, -16),
			color_manipulator_button("-", channel, -1),
			(
				Node {
					flex_grow: 1.0,
					..default()
				},
				Slider::new(255, 0),
				ColorPickerSlider(channel),
			),
			color_manipulator_button("+", channel, 1),
			color_manipulator_button(">", channel, 16),
		],
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
	selected_color: Res<State<SelectedColorKey>>,
	mut new_selected_color: ResMut<NextState<SelectedColorKey>>,
	mut clipboard: ResMut<ColorPickerClipboard>,
	mut current_color: Single<&mut ColorPickerCurrentColor>,
) {
	for action in reader.read() {
		match action {
			ColorPickerButton::EditColor(channel, delta) => {
				let target = current_color.get_channel_mut(*channel);
				*target = target.saturating_add_signed(*delta);
			}
			ColorPickerButton::ColorKey(color_key) => {
				new_selected_color.set(SelectedColorKey(*color_key))
			}
			ColorPickerButton::Reset => {
				***current_color =
					SrgbaU8::from_srgba(ThingPalette::default()[&selected_color.0].to_srgba())
			}
			ColorPickerButton::Copy => **clipboard = ***current_color,
			ColorPickerButton::Paste => ***current_color = **clipboard,
		}
	}
}

fn handle_picker_slider_inputs(
	query: Query<(Ref<Slider>, &ColorPickerSlider), Changed<Slider>>,
	mut current_color: Single<&mut ColorPickerCurrentColor>,
) {
	for (slider, ColorPickerSlider(channel)) in &query {
		if slider.is_added() {
			// Do not handle the false input after the slider is created
			continue;
		}
		*current_color.get_channel_mut(*channel) = slider.position as u8;
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

fn update_slider_values(
	color_q: Query<&ColorPickerCurrentColor, Changed<ColorPickerCurrentColor>>,
	mut slider_q: Query<(&mut Slider, &ColorPickerSlider)>,
) {
	for current in &color_q {
		for (mut slider, ColorPickerSlider(channel)) in &mut slider_q {
			let new_value = *current.get_channel(*channel) as u32;
			if new_value != slider.position {
				slider.position = new_value;
			}
		}
	}
}

fn update_current_color_from_key(
	key: Res<State<SelectedColorKey>>,
	palette: Res<ThingPalette>,
	mut query: Query<&mut ColorPickerCurrentColor>,
) {
	for mut color in &mut query {
		**color = SrgbaU8::from_srgba(palette[&key.get().0].to_srgba());
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
