//! Hierarchical relations of Cyl widgets

use crate::ui::cyl::prelude::CylNavigator;

use super::{CylSystems, CylWidget};
use bevy::{ecs::system::SystemParam, prelude::*};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(
		PostUpdate,
		copy_from_bevy_hierarchy.in_set(CylSystems::HierarchySetup),
	);
}

#[derive(Component, Deref, DerefMut)]
#[relationship(relationship_target = CylSubWidget)]
#[require(CylWidget)]
pub struct CylParentWidget(pub Entity);

#[derive(Component, Deref, Debug)]
#[relationship_target(relationship = CylParentWidget)]
#[require(CylWidget)]
pub struct CylSubWidget(Vec<Entity>);

/// Convenience [`SystemParam`] for accessing data about hiearchies of widgets.
#[derive(SystemParam)]
pub(super) struct CylWidgetData<'w, 's> {
	widget_query: Query<
		'w,
		's,
		(
			Option<&'static CylSubWidget>,
			Option<&'static CylParentWidget>,
			Option<&'static CylNavigator>,
		),
		With<CylWidget>,
	>,
	widget_root_query: Query<'w, 's, Entity, (With<CylWidget>, Without<CylParentWidget>)>,
}

impl CylWidgetData<'_, '_> {
	/// Tests if an entity has a [`CylWidget`] marker
	pub fn is_widget(&self, entity: Entity) -> bool {
		self.widget_query.contains(entity)
	}

	/// Finds a parent widget for the entity, if exists
	pub fn get_parent(&self, entity: Entity) -> Option<Entity> {
		self.widget_query
			.get(entity)
			.ok()
			.and_then(|(_, parent, _)| parent)
			.map(|CylParentWidget(entity)| entity)
			.copied()
	}

	/// Find a subwidget that should be navigated to, respecting subwidget_override if possible.
	pub fn find_navigation_subwidget(&self, entity: Entity, prefer_last: bool) -> Option<Entity> {
		let Ok((Some(subwidgets), _, navigator)) = self.widget_query.get(entity) else {
			return None;
		};
		let strict_override =
			navigator.is_some_and(|over| over.avoid_implicitly_changing_down_widget);
		let override_entity =
			navigator
				.and_then(|over| over.down_widget)
				.filter(|override_entity| {
					// Check that the override is actually a subwidget
					if let Ok((_, Some(parent), _)) = self.widget_query.get(*override_entity) {
						parent.0 == entity
					} else {
						false
					}
				});
		if strict_override || override_entity.is_some() {
			return override_entity;
		}
		// No override was found, so we find our own candidate.
		if prefer_last {
			subwidgets.last().copied()
		} else {
			subwidgets.first().copied()
		}
	}

	/// Selects a root widget.
	pub fn pick_root(&self) -> Option<Entity> {
		self.widget_root_query.iter().next()
	}
}

/// Sets up Cyl hierarchy relationships to mimic
/// default Bevy hierarchy by default
fn copy_from_bevy_hierarchy(
	mut commands: Commands,
	widget_q: Query<(Entity, &ChildOf), (Added<CylWidget>, Without<CylParentWidget>)>,
	parent_q: Query<(Has<CylWidget>, Option<&ChildOf>)>,
) {
	for (id, ChildOf(parent)) in &widget_q {
		let mut parent = Some(*parent);

		while let Some(p) = parent {
			let Ok((is_cyl_widget, next_parent)) = parent_q.get(p) else {
				break;
			};
			if is_cyl_widget {
				commands.entity(id).insert(CylParentWidget(p));
				break;
			} else {
				parent = next_parent.map(|p| p.0);
			}
		}
	}
}
