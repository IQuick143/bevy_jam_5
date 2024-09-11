//! Spawn the main level by triggering other observers.

use std::f32::consts::{PI, SQRT_2, TAU};

use crate::{
	assets::{BoxColorSpriteAtlasLayout, HandleMap, ImageKey, BOX_COLOR_SPRITE_PICTOGRAM_OFFSET},
	game::{
		animation::*, components::*, drawing::*, inputs::*, level::*,
		logic::ComputedCycleTurnability, prelude::*,
	},
	graphics::*,
	screen::Screen,
	ui::hover::{self, HintText, Hoverable},
};

use bevy::{
	ecs::system::EntityCommands,
	math::{
		bounding::{Aabb2d, BoundingCircle},
		primitives,
	},
	sprite::Anchor::Custom,
};
use rand::Rng;

pub(super) fn plugin(app: &mut App) {
	app.observe(spawn_level);
}

/// Trigger event that spawns the content entities of a level
#[derive(Event, Debug)]
pub struct SpawnLevel(pub Handle<LevelData>);

fn spawn_level(
	trigger: Trigger<SpawnLevel>,
	mut events: EventWriter<GameLayoutChanged>,
	mut commands: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	levels: Res<Assets<LevelData>>,
	mut is_level_completed: ResMut<IsLevelCompleted>,
	mut move_history: ResMut<MoveHistory>,
	materials: Res<GameObjectMaterials>,
	common_meshes: Res<GameObjectMeshes>,
	palette: ResMut<ThingPalette>,
	image_handles: Res<HandleMap<ImageKey>>,
	logical_color_atlas_layout: Res<BoxColorSpriteAtlasLayout>,
	mut hint_text: ResMut<HintText>,
) {
	println!("Spawning!"); //TODO: debug
	let SpawnLevel(level) = trigger.event();
	let level = levels
		.get(level)
		.expect("Got an invalid handle to a level asset");

	let vertices: Vec<Entity> = level
		.vertices
		.iter()
		.map(|data| {
			spawn_vertex(
				commands.reborrow(),
				data,
				materials.cycle_rings.clone(),
				materials.colored_button_labels.clone(),
				&common_meshes,
				palette.as_ref(),
				image_handles.as_ref(),
				&logical_color_atlas_layout,
			)
		})
		.collect();

	let cycle_ids = level
		.cycles
		.iter()
		.map(|data| {
			spawn_cycle(
				commands.reborrow(),
				meshes.reborrow(),
				materials.cycle_rings.clone(),
				&palette,
				&image_handles,
				data,
				&vertices,
			)
		})
		.collect::<Vec<_>>();

	for (i, cycle_id) in cycle_ids.iter().copied().enumerate() {
		let linked_cycles = level.cycles[i]
			.link_closure
			.iter()
			.map(|&(j, dir)| (cycle_ids[j], dir))
			.collect::<Vec<_>>();
		commands
			.entity(cycle_id)
			.insert(LinkedCycles(linked_cycles));
	}

	for link in &level.declared_links {
		let a = level.cycles[link.source_cycle].placement.position;
		let b = level.cycles[link.dest_cycle].placement.position;
		let d_sq = a.distance_squared(b);
		if d_sq <= CYCLE_LINK_SPACING.powi(2) {
			// The link cannot be rendered if the cycles are too close
			log::warn!("Skipped drawing a cycle link because the cycles are {} units apart, need at least {CYCLE_LINK_SPACING}", d_sq.sqrt());
			continue;
		}
		let connector_length = match link.direction {
			LinkedCycleDirection::Coincident => a.distance(b),
			LinkedCycleDirection::Inverse => (d_sq - CYCLE_LINK_SPACING.powi(2)).sqrt(),
		};
		let mesh = primitives::Rectangle::from_size(Vec2::new(
			connector_length - CYCLE_LINK_END_CUT,
			CYCLE_LINK_WIDTH,
		))
		.mesh();
		let mesh = meshes.add(mesh);
		let dir_from_a_to_b = (a - b).normalize();
		let rotation = Quat::from_rotation_arc_2d(Vec2::X, dir_from_a_to_b);
		let position = a.lerp(b, 0.5);
		let offset = match link.direction {
			LinkedCycleDirection::Coincident => dir_from_a_to_b.perp() * CYCLE_LINK_SPACING / 2.0,
			LinkedCycleDirection::Inverse => Vec2::ZERO,
		};
		let extra_rotation = match link.direction {
			LinkedCycleDirection::Coincident => Quat::IDENTITY,
			LinkedCycleDirection::Inverse => Quat::from_rotation_arc_2d(
				Vec2::X,
				Vec2::new(d_sq.sqrt(), CYCLE_LINK_SPACING).normalize(),
			),
		};
		commands.spawn((
			StateScoped(Screen::Playing),
			LevelScoped,
			ColorMesh2dBundle {
				mesh: bevy::sprite::Mesh2dHandle(mesh.clone_weak()),
				transform: Transform::from_rotation(rotation.mul_quat(extra_rotation))
					.with_translation((position + offset).extend(layers::CYCLE_LINKS)),
				material: materials.link_lines.clone_weak(),
				..default()
			},
		));
		commands.spawn((
			StateScoped(Screen::Playing),
			LevelScoped,
			ColorMesh2dBundle {
				mesh: bevy::sprite::Mesh2dHandle(mesh),
				transform: Transform::from_rotation(rotation.mul_quat(extra_rotation.inverse()))
					.with_translation((position - offset).extend(layers::CYCLE_LINKS)),
				material: materials.link_lines.clone_weak(),
				..default()
			},
		));
	}

	is_level_completed.0 = false;
	move_history.clear();
	events.send(GameLayoutChanged);

	hint_text.hint_text.clone_from(&level.hint);
}

fn spawn_vertex(
	mut commands: Commands,
	data: &VertexData,
	base_material: Handle<ColorMaterial>,
	colored_button_label_material: Handle<ColorMaterial>,
	common_meshes: &GameObjectMeshes,
	palette: &ThingPalette,
	image_handles: &HandleMap<ImageKey>,
	logical_color_atlas_layout: &BoxColorSpriteAtlasLayout,
) -> Entity {
	let transform =
		TransformBundle::from_transform(Transform::from_translation(data.position.extend(0.0)));
	let vertex_id = commands
		.spawn((
			Vertex,
			StateScoped(Screen::Playing),
			LevelScoped,
			PlacedGlyph(None),
			PlacedObject(None),
			transform,
		))
		.id();
	commands.spawn((
		StateScoped(Screen::Playing),
		LevelScoped,
		ColorMesh2dBundle {
			transform: Transform::from_translation(data.position.extend(layers::CYCLE_NODES)),
			mesh: bevy::sprite::Mesh2dHandle(common_meshes.vertices.clone_weak()),
			material: base_material,
			..default()
		},
	));
	if let Some(object_data) = data.object {
		let object_type = object_data.into();
		let thing_type = ThingType::Object(object_type);
		let entity = match object_data {
			ObjectData::Player => commands.spawn((
				StateScoped(Screen::Playing),
				LevelScoped,
				Object,
				Player,
				VertexPosition(vertex_id),
				thing_type,
				SpriteBundle {
					sprite: Sprite {
						color: palette.player,
						custom_size: Some(SPRITE_SIZE),
						anchor: Custom(Vec2::new(0.0, -0.25)),
						..default()
					},
					texture: image_handles[&ImageKey::Object(thing_type)].clone_weak(),
					transform: Transform::from_translation(
						data.position.extend(layers::OBJECT_SPRITES),
					),
					..Default::default()
				},
				AnimatedObject::default(),
				Hoverable {
					hover_text: hover::PLAYER,
					hover_bounding_circle: None,
					hover_bounding_box: Some(Aabb2d::new(
						SPRITE_LENGTH * Vec2::new(0.0, 0.25),
						SPRITE_LENGTH * Vec2::new(0.25, 0.4),
					)),
				},
			)),
			ObjectData::Box(color) => {
				let mut entity = commands.spawn((
					StateScoped(Screen::Playing),
					LevelScoped,
					Object,
					Box,
					VertexPosition(vertex_id),
					thing_type,
					SpriteBundle {
						sprite: Sprite {
							color: palette.box_base,
							custom_size: Some(SPRITE_SIZE),
							anchor: Custom(Vec2::new(0.0, -0.25)),
							..default()
						},
						texture: image_handles[&ImageKey::Object(thing_type)].clone_weak(),
						transform: Transform::from_translation(
							data.position.extend(layers::OBJECT_SPRITES),
						),
						..Default::default()
					},
					AnimatedObject::default(),
					Hoverable {
						hover_text: hover::BOX,
						hover_bounding_circle: None,
						hover_bounding_box: Some(Aabb2d::new(
							SPRITE_LENGTH * Vec2::new(0.0, 0.125),
							SPRITE_LENGTH * Vec2::new(0.25, 0.25),
						)),
					},
				));
				if let Some(color) = color {
					entity.insert(color);
					spawn_box_color_sprites(
						entity.reborrow(),
						color,
						palette.box_base,
						image_handles[&ImageKey::BoxSpriteAtlas].clone_weak(),
						logical_color_atlas_layout.0.clone_weak(),
						Vec3::new(0.0, COLOR_SPRITE_OFFSET, layers::BOX_COLOR_SPRITES),
						0.0,
					);
				}
				entity
			}
		};
		let object_id = entity.id();
		commands
			.entity(vertex_id)
			.insert(PlacedObject(Some(object_id)));
	}
	if let Some(glyph_data) = data.glyph {
		let glyph_type = glyph_data.into();
		let thing_type = ThingType::Glyph(glyph_type);
		let entity = match glyph_data {
			GlyphData::Button(color) => {
				let mut entity = commands.spawn((
					StateScoped(Screen::Playing),
					LevelScoped,
					Glyph,
					BoxSlot,
					VertexPosition(vertex_id),
					thing_type,
					SpriteBundle {
						sprite: Sprite {
							color: palette.button_base,
							custom_size: Some(SPRITE_SIZE),
							anchor: Custom(Vec2::new(0.0, -0.25)),
							..default()
						},
						texture: image_handles[&ImageKey::Object(thing_type)].clone_weak(),
						transform: Transform::from_translation(
							data.position.extend(layers::GLYPH_SPRITES),
						),
						..Default::default()
					},
					Hoverable {
						hover_text: hover::BUTTON,
						hover_bounding_circle: None,
						hover_bounding_box: Some(Aabb2d::new(
							SPRITE_LENGTH * Vec2::new(0.0, -0.125),
							SPRITE_LENGTH * Vec2::new(0.375, 0.125),
						)),
					},
				));
				if let Some((color, label_style)) = color {
					entity.insert(color);
					let label_mesh = if label_style.has_arrow_tip {
						common_meshes.arrow_labels.clone_weak()
					} else {
						common_meshes.square_labels.clone_weak()
					};
					let (label_displacement, label_rotation, sprite_rotation) =
						get_button_color_label_placement(&label_style);
					entity.with_children(|children| {
						children.spawn(ColorMesh2dBundle {
							mesh: bevy::sprite::Mesh2dHandle(label_mesh),
							material: colored_button_label_material.clone_weak(),
							transform: Transform::from_rotation(Quat::from_rotation_z(
								label_rotation,
							))
							.with_translation(
								label_displacement.extend(layers::BUTTON_COLOR_LABELS),
							),
							..default()
						});
					});
					spawn_box_color_sprites(
						entity.reborrow(),
						color,
						palette.button_base,
						image_handles[&ImageKey::BoxSpriteAtlas].clone_weak(),
						logical_color_atlas_layout.0.clone_weak(),
						label_displacement.extend(layers::BUTTON_COLOR_SPRITES),
						sprite_rotation,
					);
				}
				entity
			}
			GlyphData::Flag => commands.spawn((
				StateScoped(Screen::Playing),
				LevelScoped,
				Glyph,
				Goal,
				VertexPosition(vertex_id),
				thing_type,
				SpriteBundle {
					sprite: Sprite {
						color: palette.goal_closed,
						custom_size: Some(SPRITE_SIZE),
						anchor: Custom(Vec2::new(0.0, -0.25)),
						..default()
					},
					texture: image_handles[&ImageKey::Object(thing_type)].clone_weak(),
					transform: Transform::from_translation(
						data.position.extend(layers::GLYPH_SPRITES),
					),
					..Default::default()
				},
				Hoverable {
					hover_text: hover::FLAG,
					hover_bounding_circle: None,
					hover_bounding_box: Some(Aabb2d::new(
						SPRITE_LENGTH * Vec2::new(0.0, 0.125),
						SPRITE_LENGTH * Vec2::new(0.25, 0.30),
					)),
				},
			)),
		};
		let glyph_id = entity.id();
		commands
			.entity(vertex_id)
			.insert(PlacedGlyph(Some(glyph_id)));
	}

	vertex_id
}

fn spawn_cycle(
	mut commands: Commands,
	mut meshes: Mut<Assets<Mesh>>,
	material: Handle<ColorMaterial>,
	palette: &ThingPalette,
	image_handles: &HandleMap<ImageKey>,
	data: &CycleData,
	vertex_entities: &[Entity],
) -> Entity {
	let mesh = primitives::Annulus::new(
		data.placement.radius - RING_HALF_WIDTH,
		data.placement.radius + RING_HALF_WIDTH,
	)
	.mesh()
	.resolution(64)
	.build();

	commands
		.spawn((
			data.turnability,
			StateScoped(Screen::Playing),
			LevelScoped,
			ComputedCycleTurnability(true),
			CycleVertices(
				data.vertex_indices
					.iter()
					.map(|i| *vertex_entities.get(*i).unwrap())
					.collect(),
			),
			CycleInterationRadius(data.placement.radius),
			CycleInteraction::default(),
			TransformBundle::from_transform(Transform::from_translation(
				data.placement.position.extend(0.0),
			)),
			VisibilityBundle::default(),
		))
		.with_children(|parent| {
			parent.spawn((
				SpriteBundle {
					sprite: Sprite {
						custom_size: Some(SPRITE_SIZE),
						color: palette.cycle_ready,
						..default()
					},
					texture: image_handles[&ImageKey::CycleCenter(data.turnability)].clone_weak(),
					transform: Transform::from_translation(
						Vec2::ZERO.extend(layers::CYCLE_CENTER_SPRITES),
					),
					..default()
				},
				JumpTurnAnimation::default(),
				Hoverable {
					hover_text: match data.turnability {
						CycleTurnability::Always => hover::CYCLE_AUTOMATIC,
						CycleTurnability::WithPlayer => hover::CYCLE_MANUAL,
						CycleTurnability::Never => hover::CYCLE_STILL,
					},
					hover_bounding_circle: Some(BoundingCircle::new(
						Vec2::ZERO,
						SPRITE_LENGTH / 2.0,
					)),
					hover_bounding_box: None,
				},
			));
			parent.spawn((
				SpriteBundle {
					sprite: Sprite {
						custom_size: Some(SPRITE_SIZE * 2.0),
						color: palette.cycle_ready,
						..default()
					},
					texture: image_handles[&ImageKey::CycleRotationArrow].clone_weak(),
					transform: Transform::from_translation(Vec3::Z * layers::CYCLE_CENTER_ARROWS),
					..default()
				},
				SpinAnimation {
					current_phase: rand::thread_rng().gen_range(0.0..std::f32::consts::TAU),
					..default()
				},
			));
			parent.spawn(ColorMesh2dBundle {
				mesh: bevy::sprite::Mesh2dHandle(meshes.add(mesh)),
				material,
				transform: Transform::from_translation(Vec2::ZERO.extend(layers::CYCLE_RINGS)),
				..default()
			});
		})
		.id()
}

fn spawn_box_color_sprites(
	mut box_entity: EntityCommands,
	logical_color: LogicalColor,
	color: Color,
	atlas: Handle<Image>,
	atlas_layout: Handle<TextureAtlasLayout>,
	displacement: Vec3,
	rotation: f32,
) {
	box_entity.with_children(|children| {
		let mut spawn_sprite = |index, x_offset, x_scale| {
			let transform = Transform::from_translation(displacement)
				.mul_transform(Transform::from_rotation(Quat::from_rotation_z(rotation)))
				.mul_transform(
					Transform::from_translation(Vec3::X * x_offset)
						.with_scale(Vec3::new(x_scale, 1.0, 1.0)),
				);
			children.spawn((
				InheritSpriteColor,
				SpriteBundle {
					transform,
					texture: atlas.clone_weak(),
					sprite: Sprite {
						color,
						custom_size: Some(COLOR_SPRITE_SIZE),
						..default()
					},
					..default()
				},
				TextureAtlas {
					layout: atlas_layout.clone_weak(),
					index,
				},
			));
		};

		if logical_color.is_pictogram {
			let sprite_index = logical_color.color_index + BOX_COLOR_SPRITE_PICTOGRAM_OFFSET;
			spawn_sprite(sprite_index, 0.0, 1.0);
		} else {
			let index_str = logical_color.color_index.to_string();
			// Calculate the width of the written number. Digit 1 needs less space than the others.
			let number_width = index_str
				.chars()
				.map(|c| {
					if c == '1' {
						DIGIT_ONE_SPRITE_WIDTH + DIGIT_SPRITE_SPACING
					} else {
						DIGIT_SPRITE_WIDTH + DIGIT_SPRITE_SPACING
					}
				})
				.sum::<f32>() - DIGIT_SPRITE_SPACING;
			let (mut width_progress, number_width) = if number_width < 1.0 {
				// Sprites should never grow in width, so cap the width at one
				((1.0 - number_width) / 2.0, 1.0)
			} else {
				(0.0, number_width)
			};
			for digit in index_str.chars() {
				let digit = digit
					.to_digit(10)
					.expect("String representation of a number should only be digits");
				let current_width = if digit == 1 {
					DIGIT_ONE_SPRITE_WIDTH
				} else {
					DIGIT_SPRITE_WIDTH
				};
				let relative_offset = (width_progress + current_width / 2.0) / number_width - 0.5;
				width_progress += current_width + DIGIT_SPRITE_SPACING;
				spawn_sprite(
					digit as usize,
					relative_offset * COLOR_SPRITE_SIZE.x,
					1.0 / number_width,
				);
			}
		}
	});
}

/// Calculates the actual placement for a button color label
/// ## Return Value
/// The position of the center of the label, rotation of the label marker
/// and rotation of the color sprite
fn get_button_color_label_placement(style: &ButtonColorLabelAppearence) -> (Vec2, f32, f32) {
	use color_labels::*;
	use ButtonColorLabelPosition::*;
	let secondary_offset = if style.has_arrow_tip {
		OFFSET_SECONDARY_ARROW
	} else {
		OFFSET_SECONDARY_SQUARE
	};
	match style.position {
		Inside => (Vec2::Y * CENTER_Y_OFFSET, 0.0, 0.0),
		LeftButton => (
			Vec2::new(-secondary_offset, OFFSET_Y_BUTTON_ALIGNED),
			-PI / 2.0,
			0.0,
		),
		RightButton => (
			Vec2::new(secondary_offset, OFFSET_Y_BUTTON_ALIGNED),
			PI / 2.0,
			0.0,
		),
		AnglePlaced(angle) | AngleRotated(angle) => {
			// Clamp angle to the [0, 2pi] range
			let angle = angle.rem_euclid(TAU);
			// If we are above the box, reduce the secondary offset
			// This is to bring the label closer to the box when there is no button in the way
			let use_reduced_offset =
				angle <= OFFSET_REDUCTION_THRESHOLD || angle >= TAU - OFFSET_REDUCTION_THRESHOLD;
			let secondary_offset = if use_reduced_offset {
				secondary_offset - SECONDARY_OFFSET_REDUCTION
			} else {
				secondary_offset
			};
			let (relative_position, quadrant_rotation) = match angle / PI {
				// Above
				0.0..0.25 | 1.75..=2.0 => (
					Vec2::new(angle.tan() * secondary_offset, secondary_offset),
					PI,
				),
				// Right
				0.25..0.75 => (
					Vec2::new(
						secondary_offset,
						-(angle - PI / 2.0).tan() * secondary_offset,
					),
					PI / 2.0,
				),
				// Below
				0.75..1.25 => (
					Vec2::new(-angle.tan() * secondary_offset, -secondary_offset),
					0.0,
				),
				// Left
				1.25..1.75 => (
					Vec2::new(
						-secondary_offset,
						(angle - PI / 2.0).tan() * secondary_offset,
					),
					-PI / 2.0,
				),
				_ => unreachable!("The angle should be clamped to [0, 2pi]"),
			};
			let position = relative_position + Vec2::Y * CENTER_Y_OFFSET;
			// Rotated placements should propagate any rotation angle
			if matches!(style.position, AngleRotated(_)) {
				let label_rotation = PI - angle;
				let sprite_rotation = PI - angle - quadrant_rotation;
				// If we have a rotated placement near a corner,
				// we bring it closer to the box to avoid a gap
				let base_max_displacement = if style.has_arrow_tip {
					MAX_ROTATED_DISPLACEMENT_ARROW
				} else {
					MAX_ROTATED_DISPLACEMENT_SQUARE
				};
				let max_distance = if use_reduced_offset {
					base_max_displacement - SECONDARY_OFFSET_REDUCTION * SQRT_2
				} else {
					base_max_displacement
				};
				let cut_position = if relative_position.length_squared() >= max_distance.powi(2) {
					Vec2::from_angle(PI / 2.0 - angle) * max_distance + Vec2::Y * CENTER_Y_OFFSET
				} else {
					position
				};
				(cut_position, label_rotation, sprite_rotation)
			} else {
				(position, quadrant_rotation, 0.0)
			}
		}
	}
}
