//! Quick palette editing for developers

use super::input_just_pressed;
use crate::{
	AppSet,
	drawing::{ColorKey, NodeColorKey, ThingPalette},
	ui::{interaction::InteractionQuery, slider::Slider},
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
				toggle_color_picker_display.run_if(input_just_pressed(KeyCode::KeyC)),
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
struct ColorPickerClipboard(OklchaU16);

#[derive(Component, Clone, Copy, Debug, Default)]
struct ColorPickerWidget;

#[derive(Component, Clone, Copy, Debug, Default, Deref, DerefMut)]
struct ColorPickerCurrentColor(OklchaU16);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct OklchaU16 {
	h: u16,
	c: u16,
	l: u16,
	a: u16,
}

impl Default for OklchaU16 {
	fn default() -> Self {
		Self {
			h: 360,
			c: 255,
			l: 255,
			a: 255,
		}
	}
}

impl OklchaU16 {
	fn from_oklcha(color: Oklcha) -> Self {
		Self {
			h: color.hue as u16,
			c: (color.chroma * 255.0) as u16,
			l: (color.lightness * 255.0) as u16,
			a: (color.alpha * 255.0) as u16,
		}
	}

	fn to_oklcha(self) -> Oklcha {
		Oklcha {
			hue: self.h as f32,
			chroma: self.c as f32 / 255.0,
			lightness: self.l as f32 / 255.0,
			alpha: self.a as f32 / 255.0,
		}
	}

	fn get_channel(&self, channel: ColorChannel) -> &u16 {
		match channel {
			ColorChannel::Hue => &self.h,
			ColorChannel::Chroma => &self.c,
			ColorChannel::Lightness => &self.l,
			ColorChannel::Alpha => &self.a,
		}
	}

	fn get_channel_mut(&mut self, channel: ColorChannel) -> &mut u16 {
		match channel {
			ColorChannel::Hue => &mut self.h,
			ColorChannel::Chroma => &mut self.c,
			ColorChannel::Lightness => &mut self.l,
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
	EditColor(ColorChannel, i16),
	ColorKey(ColorKey),
	Reset,
	Copy,
	Paste,
	Print,
}

#[derive(Component, Message, Clone, Copy, PartialEq, Eq, Debug)]
struct ColorPickerSlider(ColorChannel);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ColorChannel {
	Hue,
	Chroma,
	Lightness,
	Alpha,
}

impl ColorChannel {
	fn max_value(self) -> u16 {
		if self == ColorChannel::Hue { 360 } else { 255 }
	}
}

#[derive(States, Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
struct SelectedColorKey(ColorKey);

const KEY_PICKER_FONT_SIZE: f32 = 10.0;

const COLOR_MANIPULATOR_FONT_SIZE: f32 = 15.0;

const COLOR_MANIPULATOR_WIDTH: Val = Val::Px(250.0);

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
			min_width: COLOR_MANIPULATOR_WIDTH,
			..default()
		},
		ChildOf(root),
		children![
			color_manipulator_slider(ColorChannel::Hue),
			color_manipulator_slider(ColorChannel::Chroma),
			color_manipulator_slider(ColorChannel::Lightness),
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
						NodeColorKey(ColorKey::NodeBackground),
						ColorPickerButton::Reset,
					),
					(
						color_manipulator_button_base("C", Color::WHITE),
						NodeColorKey(ColorKey::NodeBackground),
						ColorPickerButton::Copy,
					),
					(
						color_manipulator_button_base("V", Color::WHITE),
						NodeColorKey(ColorKey::NodeBackground),
						ColorPickerButton::Paste,
					),
					(
						color_manipulator_button_base("P", Color::WHITE),
						NodeColorKey(ColorKey::NodeBackground),
						ColorPickerButton::Print,
					),
				],
			),
		],
	));
}

fn color_manipulator_button(
	label: impl Into<String>,
	channel: ColorChannel,
	amount: i16,
) -> impl Bundle {
	let (color, text_color) = (Srgba::WHITE.into(), Color::BLACK);
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
	let channel_label = match channel {
		ColorChannel::Hue => "h",
		ColorChannel::Chroma => "c",
		ColorChannel::Lightness => "l",
		ColorChannel::Alpha => "a",
	};
	(
		Node::default(),
		children![
			(Text::new(channel_label), TextColor(Color::BLACK),),
			color_manipulator_button("<", channel, -16),
			color_manipulator_button("-", channel, -1),
			(
				Node {
					flex_grow: 1.0,
					..default()
				},
				Slider::new(channel.max_value().into(), 0),
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
	palette: Res<ThingPalette>,
) {
	for action in reader.read() {
		match action {
			ColorPickerButton::EditColor(channel, delta) => {
				let target = current_color.get_channel_mut(*channel);
				*target = target
					.saturating_add_signed(*delta)
					.min(channel.max_value());
			}
			ColorPickerButton::ColorKey(color_key) => {
				new_selected_color.set(SelectedColorKey(*color_key))
			}
			ColorPickerButton::Reset => {
				***current_color =
					OklchaU16::from_oklcha(ThingPalette::default()[&selected_color.0].into())
			}
			ColorPickerButton::Copy => **clipboard = ***current_color,
			ColorPickerButton::Paste => ***current_color = **clipboard,
			ColorPickerButton::Print => eprintln!("{palette:?}"),
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
		*current_color.get_channel_mut(*channel) = slider.position as u16;
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
		let current = current.to_oklcha();
		background.0 = current.into();
		let srgba = current.into();
		**text = contrasting_text_color(srgba);
		**text_content = srgba.to_hex();
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
		**color = OklchaU16::from_oklcha(palette[&key.get().0].into());
	}
}

fn update_palette_entry(
	key: Res<State<SelectedColorKey>>,
	query: Query<Ref<ColorPickerCurrentColor>, Changed<ColorPickerCurrentColor>>,
	mut palette: ResMut<ThingPalette>,
) {
	if let Some(current) = query.iter().filter(|r| !r.is_added()).last() {
		let current = current.to_oklcha();
		palette.insert(key.get().0, current.into());
	}
}
