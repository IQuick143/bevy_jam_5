//! Main HUD UI that appears while playing a level and possibly in other places

use super::*;

pub(super) fn plugin(app: &mut App) {
	app.insert_resource(HudVisibility(true))
		.add_systems(Startup, spawn_hud)
		.add_systems(
			Update,
			update_hud_display.run_if(resource_changed::<HudVisibility>),
		);
}

#[allow(clippy::assigning_clones)]
/// Shorthand to create a system that sets the content of the main text area of the HUD UI
pub fn set_main_text_area(value: &'static str) -> impl Fn(Query<&mut Text, With<HudTextbox>>) {
	|mut q| {
		let mut text = q.single_mut();
		text.sections[0].value = value.to_owned();
	}
}

/// Marker component for the root node of HUD
#[derive(Component)]
struct HudRoot;

/// Marker component for the main text node of the HUD
#[derive(Component)]
pub struct HudTextbox;

/// Marker component for the tool/status bar node of the hud
#[derive(Component)]
pub struct HudToolbar;

/// Resource that controls the visibility of the HUD UI
#[derive(Resource)]
pub struct HudVisibility(pub bool);

fn spawn_hud(mut commands: Commands) {
	commands
		.spawn((
			HudRoot,
			NodeBundle {
				style: Style {
					width: Val::Percent(100.0),
					height: Val::Percent(100.0),
					justify_content: JustifyContent::End,
					flex_direction: FlexDirection::Column,
					..default()
				},
				..default()
			},
		))
		.with_children(|parent| {
			parent.spawn((
				HudToolbar,
				NodeBundle {
					style: Style {
						width: Val::Percent(100.0),
						height: Val::Px(36.0),
						justify_content: JustifyContent::Start,
						padding: UiRect::all(Val::Px(6.0)),
						..default()
					},
					background_color: BackgroundColor(bevy::color::palettes::css::MAROON.into()),
					..default()
				},
			));
			parent
				.spawn(NodeBundle {
					style: Style {
						display: Display::Block,
						width: Val::Percent(100.0),
						height: Val::Px(80.0),
						padding: UiRect::all(Val::Px(6.0)),
						..default()
					},
					background_color: BackgroundColor(
						bevy::color::palettes::css::MIDNIGHT_BLUE.into(),
					),
					..default()
				})
				.with_children(|parent| {
					parent
						.spawn(NodeBundle {
							style: Style {
								display: Display::Block,
								height: Val::Percent(100.0),
								padding: UiRect::all(Val::Px(6.0)),
								border: UiRect::all(Val::Px(1.0)),
								..default()
							},
							border_color: BorderColor(bevy::color::palettes::css::BLUE.into()),
							..default()
						})
						.with_children(|parent| {
							parent.spawn((
								HudTextbox,
								TextBundle::from_section(
									"",
									TextStyle {
										// TODO: font
										font: default(),
										font_size: 16.0,
										color: Color::WHITE,
									},
								),
							));
						});
				});
		});
}

fn update_hud_display(mut query: Query<&mut Style, With<HudRoot>>, is_visible: Res<HudVisibility>) {
	query.single_mut().display = if is_visible.0 {
		Display::Flex
	} else {
		Display::None
	};
}
