//! Navigation design:
//! Entities that can be navigated by Cyl must be [`CylWidget`]'s
//! and should be structured in a hiearchy of [`CylParentWidget`]
use super::{CylSystems, hierarchy::CylWidgetData, prelude::*};
use bevy::{ecs::system::SystemId, prelude::*};
use itertools::Itertools as _;

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<CylCurrentFocusedWidget>()
		.add_observer(set_focus)
		.add_observer(suspend_focus)
		.add_observer(update_interaction)
		.add_observer(|e: On<CylFocusChanged>| info!("{e:?}"))
		.add_observer(|e: On<NavigationInput>, mut commands: Commands| {
			commands.run_system_cached_with(navigation_system, **e);
		})
		.add_systems(
			PostUpdate,
			set_default_focus_to_root_widget.in_set(CylSystems::RootSetup),
		);
}

/// Trigger this on an entity to signal to Cyl that
/// the entity has gained focus.
///
/// Cyl will update its index and notify all affected
/// entities that they have gained/lost focus
#[derive(EntityEvent)]
pub struct CylSetFocus {
	#[event_target]
	pub entity: Entity,
	pub reason: SetFocusReason,
}

/// Trigger this to signal to Cyl that it should
/// suspend focus on any currently focused widget.
///
/// The widget will remain invisibly focused and will regain
/// focus if interacted with using key inputs.
#[derive(Event)]
pub struct CylSuspendFocus;

/// Sent to an entity when its focus state changes
#[derive(EntityEvent, Debug)]
pub struct CylFocusChanged {
	#[event_target]
	pub entity: Entity,
	pub reason: SetFocusReason,
	pub change_type: FocusChangeType,
}

/// Enumerates the inputs that can cause focus to be gained
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SetFocusReason {
	Navigate(NavigationDirection),
	PointerOver,
	Suspended,
	Interact,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FocusChangeType {
	/// The target entity has been directly focused
	Gained,
	/// A child of the target entity has taken focus
	GainedTransitive,
	/// The target entity has been unfocused
	Lost,
	/// The target entity has lost direct focus
	/// to its descendant
	LostToChild,
	/// The target entity remains in focus,
	/// but is kept on hold to quickly regain focus
	/// if it is interacted with
	Suspended,
}

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum CylInteraction {
	#[default]
	None,
	Focused,
	Hot,
}

impl From<Interaction> for CylInteraction {
	fn from(value: Interaction) -> Self {
		match value {
			Interaction::Pressed => Self::Hot,
			Interaction::Hovered => Self::Focused,
			Interaction::None => Self::None,
		}
	}
}

/// Resource for keeping track of focus state.
#[derive(Resource, Default)]
pub struct CylCurrentFocusedWidget {
	/// Focus of the cursor.
	///
	/// The root widget always has focus implicitly ([`Self::focus_entities`]
	/// is false). This is only empty if there are no widgets on screen.
	pub(super) focus_entities: FocusHiearchy,
	/// Whether the entity is actively focused and hovered, or the focus is just there to be able to regain focus.
	pub(super) hard_focus: bool,
}

/// A hiearchy of entities representing a focused object in the widget tree.
/// The first entity is the true desired entity to be focused, but if it is absent, entities higher up the tree can be used to regain focus.
#[derive(Default)]
pub struct FocusHiearchy(pub(super) Vec<Entity>);

impl CylCurrentFocusedWidget {
	/// Gets the entity that should currently be hovered.
	/// `None` if [`Self::hard_focus`] is `false` or there are no focus_entities.
	pub fn hovered_widget(&self) -> Option<Entity> {
		if self.hard_focus {
			self.focus_entities.0.first().copied()
		} else {
			None
		}
	}

	pub fn focused_or_suspended_widget(&self) -> Option<Entity> {
		self.focus_entities.0.first().copied()
	}

	/// Clears all focus, removing all entities from the focus hiearchy and disables hard focus.
	pub fn clear_focus(&mut self) {
		self.focus_entities.0.clear();
		self.hard_focus = false;
	}

	/// Find an entity that is currently the closest to being focused.
	pub(super) fn find_focusable(&self, widget_data: &CylWidgetData) -> Option<Entity> {
		let mut entity_to_start_focus = None;
		// Look for best match - first existing entity from the end of the list.
		for &entity in self.focus_entities.0.iter() {
			if widget_data.is_widget(entity) {
				entity_to_start_focus = Some(entity);
				break;
			}
		}
		// If nothing was found, try to look for a root node to start from.
		if entity_to_start_focus.is_none() {
			entity_to_start_focus = widget_data.pick_root();
		}
		entity_to_start_focus
	}

	/// Tries re-obtaining a valid widget to focus at and enables hard_focus iff it succeeds.
	/// `prefer_last` dictates whether the first or last entity should be preferred when trying to pick a path down a tree.
	///
	/// Returns whether the focused entity or `hard_focus` changed
	pub(super) fn refocus(&mut self, prefer_last: bool, widget_data: &CylWidgetData) -> bool {
		let initial_focus = self.focus_entities.0.first().copied();
		let initial_hard_focus = self.hard_focus;

		let entity_to_start_focus = self.find_focusable(widget_data);
		// Clear current focus data.
		// Give up, no focus.
		let Some(mut entity_to_focus) = entity_to_start_focus else {
			self.clear_focus();
			// Focus changed iff there was some at the start.
			return initial_focus.is_some() || initial_hard_focus;
		};

		// Travel down the tree to find a suitable leaf to inherit the focus throne
		for _i in 0..255 {
			if let Some(entity) =
				widget_data.find_navigation_subwidget(entity_to_focus, prefer_last)
			{
				entity_to_focus = entity;
			} else {
				break;
			}
		}

		self.focus(entity_to_focus, widget_data);

		initial_focus == self.focus_entities.0.first().copied()
			|| initial_hard_focus == self.hard_focus
	}

	/// Focuses the given widget and enables `hard_focus`
	pub(super) fn focus(&mut self, entity: Entity, widget_data: &CylWidgetData) {
		self.clear_focus();
		// We have something to focus, so we will end up with hard focus enabled.
		self.hard_focus = true;
		// Travel up the tree to note the focus entity with its noble ancestry
		let mut entity_to_focus = entity;
		for _i in 0..255 {
			self.focus_entities.0.push(entity_to_focus);
			if let Some(entity) = widget_data.get_parent(entity_to_focus) {
				entity_to_focus = entity;
			} else {
				break;
			}
		}
	}

	/// Checks if the provided entity is being hovered/hard focused, and if so, disables hard focus on it.
	pub fn unhover(&mut self, entity_losing_focus: Entity) {
		if self.hovered_widget() == Some(entity_losing_focus) {
			self.hard_focus = false;
		}
	}
}

/// Cyl [`event`] describing that the user is trying to navigate from this entity to another one
/// Automatically propagates up the widget hiearchy until stopped
#[derive(EntityEvent, Debug, Clone, Copy)]
#[entity_event(propagate = &'static CylParentWidget, auto_propagate)]
pub struct CylNavigate {
	#[event_target]
	pub entity: Entity,
	pub navigation_direction: NavigationDirection,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum NavigationDirection {
	Up,
	Down,
	Left,
	Right,
}

impl NavigationDirection {
	pub fn is_vertical(&self) -> bool {
		match self {
			Self::Up => true,
			Self::Down => true,
			Self::Left => false,
			Self::Right => false,
		}
	}
}

/// Split of [`NavigationDirection`] axes into a main and a sub axis.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct AxisSplit {
	/// Whether the main axis is the vertical one.
	/// Default is true
	pub main_direction_vertical: bool,
	/// Whether the main axis value is inverted
	/// Non-inverted is [`NavigationDirection::Down`], [`NavigationDirection::Right`] = positive
	/// Non-inverted is [`NavigationDirection::Up`], [`NavigationDirection::Left`] = negative
	/// Default is false
	pub main_axis_inverted: bool,
	/// Whether the sub axis value is inverted
	/// Non-inverted is [`NavigationDirection::Down`], [`NavigationDirection::Right`] = positive
	/// Non-inverted is [`NavigationDirection::Up`], [`NavigationDirection::Left`] = negative
	/// Default is false
	pub sub_axis_inverted: bool,
}

impl Default for AxisSplit {
	fn default() -> Self {
		Self {
			main_direction_vertical: true,
			main_axis_inverted: false,
			sub_axis_inverted: false,
		}
	}
}

impl AxisSplit {
	/// Returns whether the direction is a main or sub axis and whether it's a positive (true) or negative (false) navigation.
	pub fn split(&self, direction: NavigationDirection) -> (bool, bool) {
		let (vertical, default_positive) = match direction {
			NavigationDirection::Up => (true, false),
			NavigationDirection::Down => (true, true),
			NavigationDirection::Left => (false, false),
			NavigationDirection::Right => (false, true),
		};
		let is_main = vertical == self.main_direction_vertical;
		let inverted = if is_main {
			self.main_axis_inverted
		} else {
			self.sub_axis_inverted
		};
		(is_main, default_positive != inverted)
	}

	/// Returns the subnavigation value of the direction, 0 if the direction is not a subnavigation, +-1 if it's positive/negative.
	pub fn subnavigation_value(&self, direction: NavigationDirection) -> i32 {
		match self.split(direction) {
			(true, _) => 0,
			(false, true) => 1,
			(false, false) => -1,
		}
	}
}

#[derive(Default, Clone, Copy)]
pub enum SubnavigationMode {
	/// Capture and converts subnavigation events and converts them to interactions ([`super::events::CylInteract`]).
	SubNavigateToInteract,
	/// Capture the subnavigation event, default for a widget.
	#[default]
	SubNavigateCapture,
	/// Send the event up the hiearchy
	SubNavigatePropagate,
}

/// Cyl [`Event`] describing a change in focus, usually emitted by [`navigation_system`]
pub struct CylChangeFocus(pub Entity);

fn set_default_focus_to_root_widget(
	mut focus: ResMut<CylCurrentFocusedWidget>,
	roots: Query<Entity, (Added<CylWidget>, Without<CylParentWidget>)>,
	mut removed: RemovedComponents<CylWidget>,
) {
	// If the previous root widget was removed, clear focus entirely
	if focus
		.focus_entities
		.0
		.last()
		.is_some_and(|root| removed.read().contains(root))
	{
		focus.clear_focus();
	}

	// If there are multiple root widgets at a time, do not do anything
	let count = roots.count() + (!focus.focus_entities.0.is_empty() as usize);
	if count > 1 {
		error!("Multiple root CylWidgets are present");
		return;
	}

	// Focus should be cleared by now if there is a new root widget
	if let Ok(root) = roots.single() {
		focus.focus_entities.0.push(root);
	}
}

fn set_focus(
	event: On<CylSetFocus>,
	mut commands: Commands,
	mut focus: ResMut<CylCurrentFocusedWidget>,
	query: Query<&CylParentWidget>,
) {
	// Travel up the tree to note the focus entity with its noble ancestry
	let mut new_ancestry_chain = Vec::new();
	let mut entity_to_focus = event.entity;
	for _i in 0..255 {
		new_ancestry_chain.push(entity_to_focus);
		if let Ok(parent) = query.get(entity_to_focus) {
			entity_to_focus = **parent;
		} else {
			break;
		}
	}

	// Send focus events to all affected entities
	for (new, old) in new_ancestry_chain
		.iter()
		.rev()
		.zip_longest(focus.focus_entities.0.iter().rev())
		.map(itertools::EitherOrBoth::left_and_right)
		.map(|(a, b)| (a.copied(), b.copied()))
	{
		// Send hard focus event to the newly focused entity,
		// even if it was already focused (but not if it was hard-focused)
		if new == Some(event.entity) && (old != new || !focus.hard_focus) {
			commands.trigger(CylFocusChanged {
				entity: event.entity,
				reason: event.reason,
				change_type: FocusChangeType::Gained,
			});
		}
		if new == old {
			// Notify the entity if it lost focus to a descendant
			if new != Some(event.entity) {
				commands.trigger(CylFocusChanged {
					entity: new.expect("new == old and they can't both be None"),
					reason: event.reason,
					change_type: FocusChangeType::LostToChild,
				});
			}
			// Entities whose focus state remained unchanged are skipped
			continue;
		}
		if let Some(entity) = new {
			// Skip the directly-focused entity, we have already
			// handled it above
			if new != Some(event.entity) {
				commands.trigger(CylFocusChanged {
					entity,
					reason: event.reason,
					change_type: FocusChangeType::GainedTransitive,
				});
			}
		}
		if let Some(entity) = old {
			commands.trigger(CylFocusChanged {
				entity,
				reason: event.reason,
				change_type: FocusChangeType::Lost,
			});
		}
	}

	// Overwrite focus chain
	focus.focus_entities.0 = new_ancestry_chain;
	// We have somethng to focus, so we will end up with hard focus enabled.
	focus.hard_focus = true;
}

fn suspend_focus(
	_: On<CylSuspendFocus>,
	mut commands: Commands,
	mut focus: ResMut<CylCurrentFocusedWidget>,
) {
	// Do nothing if the focus is already suspended
	if !focus.hard_focus {
		return;
	}
	if let Some(entity) = focus.focus_entities.0.first().copied() {
		focus.hard_focus = false;
		commands.trigger(CylFocusChanged {
			entity,
			reason: SetFocusReason::Suspended,
			change_type: FocusChangeType::Suspended,
		});
	}
}

fn update_interaction(event: On<CylFocusChanged>, mut query: Query<&mut CylInteraction>) {
	if let Ok(mut interaction) = query.get_mut(event.entity) {
		let new_interaction = if event.change_type == FocusChangeType::Gained {
			CylInteraction::Focused
		} else {
			CylInteraction::None
		};
		interaction.set_if_neq(new_interaction);
	}
}

pub type CylNavigationSystem = SystemId<In<(Entity, NavigationDirection)>, NavigationResult>;

#[derive(Clone)]
pub enum CylUpNavigationTactic {
	/// This widget does not offer navigation capabilities among its subwidgets.
	/// However movements on the sub-axis may get captured and converted to other events.
	///
	/// Navigation events get passed to the parent if they're not a subnavigation or the subnavigation is propagated.
	SubnavigateCapture(SubnavigationMode),
	/// This widget navigates between its children like a list.
	///
	/// Navigation events get passed to the parent if the list goes outside its range
	/// or the direction is a subnavigation which didn't get captured.
	///
	/// This mutates the [`CylNavigator`] attached to the entity.
	List(SubnavigationMode),
	/// This widget navigates via a bespoke implementation.
	///
	/// See [`NavigationResult`] for what may happen with the navigation event.
	/// The navigation system may mutate the [`World`], for example trigger other input events, such as [`CylInteract`].
	Custom(CylNavigationSystem),
}

impl Default for CylUpNavigationTactic {
	fn default() -> Self {
		CylUpNavigationTactic::List(SubnavigationMode::default())
	}
}

pub enum NavigationResult {
	/// This entity could not handle the navigation and is giving it to the parent
	GoToParent,
	/// This entity could not handle the navigation and is giving it to the selected entity
	GoTo(Entity),
	/// This entity captures the event, upwards navigation stops here
	Stop,
}

/// Component describing the choices for navigation
#[derive(Component, Clone, Default)]
pub struct CylNavigator {
	/// Split between what this widget considers to be movements along the "main axis" and "sub axis"
	/// Sub axis movements may get consumed by [`CylUpNavigationTactic::SubnavigateCapture`] or [`CylUpNavigationTactic::List`].
	/// The
	pub axis_split: AxisSplit,
	/// Tacticc to execute when going upwards through this widget in the hiearchy
	pub up_navigation_tactic: CylUpNavigationTactic,
	/// Subwidget that should be chosen when going down through the hiearchy
	pub down_widget: Option<Entity>,
	/// If the widget selected is not applicable for whatever reason; whether one should prefer to have no subwidget than to choose a new `down_widget`.
	pub avoid_implicitly_changing_down_widget: bool,
}

/// The Cyl navigation algorithm works as follows:
///
/// First, a suitable start of navigation is sought, this is, in order of importance:
/// 1) The currently focused entity
/// 2) The last entity that was focused
/// 3) The parents of 2)
/// 4) A random root entity
///
/// If case 1) happens, meaning the selected widget is already focused, navigation happens,
/// triggering the [`CylNavigator`] behaviour of the widget.
/// This proceeds upwards until a widgets [`CylUpNavigationTactic`] specifies an ending condition (consuming the event), or the root is reached.
///
/// Second, the hiearchy is traversed downwards, following the `down_widget`s of the appropriate [`CylNavigator`]s until a dead end is found.
/// (Either a leaf or a node that doesn't wish to navigate to its children.)
///
/// Finally, [`CylSetFocus`] is emitted on the target entity.
fn navigation_system(navigation_direction: In<NavigationDirection>, world: &mut World) {
	let navigation_direction = *navigation_direction;
	let (navigation_start, is_properly_focused) = {
		fn navigation_start_search_system(
			widget_data: CylWidgetData,
			focus: Res<CylCurrentFocusedWidget>,
		) -> Option<(Entity, bool)> {
			focus.find_focusable(&widget_data).map(|focus_entity| {
				(
					focus_entity,
					focus.hard_focus && Some(focus_entity) == focus.hovered_widget(),
				)
			})
		}
		let Ok(Some((navigation_start, is_properly_focused))) =
			world.run_system_cached(navigation_start_search_system)
		else {
			return;
		};
		(navigation_start, is_properly_focused)
	};
	// The peak of upwards navigation, we go down from there.
	let navigation_top = if is_properly_focused {
		// If the start is properly focused, we will start navigating from it as intended.
		let mut current = navigation_start;
		for iteration in 0..256 {
			let Ok(current_entity) = world.get_entity(current) else {
				warn!("Cyl: Navigation upwards failed with a non-existent entity");
				return;
			};
			let (navigator, axis_split) =
				if let Some(navigator) = current_entity.get::<CylNavigator>() {
					(navigator.up_navigation_tactic.clone(), navigator.axis_split)
				} else {
					(
						CylUpNavigationTactic::SubnavigateCapture(
							SubnavigationMode::SubNavigatePropagate,
						),
						AxisSplit::default(),
					)
				};
			#[expect(dropping_copy_types)]
			drop(current_entity);
			let (main_navigation, is_positive) = axis_split.split(navigation_direction);
			if !main_navigation {
				world.trigger(CylSubnavigate {
					entity: current,
					is_positive,
				});
			}
			let next_step = match navigator {
				CylUpNavigationTactic::SubnavigateCapture(subnavigation_mode) => {
					if main_navigation {
						NavigationResult::GoToParent
					} else {
						match subnavigation_mode {
							SubnavigationMode::SubNavigateToInteract => {
								world.trigger(CylInteract(current));
								NavigationResult::Stop
							}
							SubnavigationMode::SubNavigateCapture => NavigationResult::Stop,
							SubnavigationMode::SubNavigatePropagate => NavigationResult::GoToParent,
						}
					}
				}
				CylUpNavigationTactic::List(subnavigation_mode) => {
					if !main_navigation {
						match subnavigation_mode {
							SubnavigationMode::SubNavigateToInteract => {
								world.trigger(CylInteract(current));
								NavigationResult::Stop
							}
							SubnavigationMode::SubNavigateCapture => NavigationResult::Stop,
							SubnavigationMode::SubNavigatePropagate => NavigationResult::GoToParent,
						}
					} else {
						fn list_navigation_system(
							list_entity_and_direction: In<(Entity, bool)>,
							mut list_query: Query<(&mut CylNavigator, Option<&CylSubWidget>)>,
						) -> NavigationResult {
							let (list_entity, forward) = *list_entity_and_direction;
							if let Ok((mut navigation, children)) = list_query.get_mut(list_entity)
							{
								let Some(children) = children else {
									return NavigationResult::GoToParent;
								};
								let children = children.as_slice();
								if children.is_empty() {
									navigation.down_widget = None;
									return NavigationResult::GoToParent;
								}
								let index = navigation.down_widget.and_then(|selected| {
									children.iter().position(|entity| *entity == selected)
								});
								let selected_entity = if let Some(i) = index {
									if forward {
										if i > 0 { children.get(i - 1) } else { None }
									} else {
										children.get(i + 1)
									}
								} else if forward {
									children.first()
								} else {
									children.last()
								}
								.copied();
								navigation.down_widget = selected_entity;
								if selected_entity.is_some() {
									NavigationResult::Stop
								} else {
									NavigationResult::GoToParent
								}
							} else {
								warn!("Cyl: List-like widget {list_entity} has no `CylNavigator`");
								NavigationResult::GoToParent
							}
						}
						world
							.run_system_cached_with(list_navigation_system, (current, is_positive))
							.unwrap_or(NavigationResult::GoToParent)
					}
				}
				CylUpNavigationTactic::Custom(system_id) => world
					.run_system_with(system_id, (current, navigation_direction))
					.unwrap_or(NavigationResult::GoToParent),
			};
			match next_step {
				NavigationResult::GoToParent => {
					let Some(CylParentWidget(parent)) = world.get::<CylParentWidget>(current)
					else {
						break;
					};
					current = *parent;
				}
				NavigationResult::GoTo(entity) => current = entity,
				NavigationResult::Stop => break,
			}
			if iteration == 255 {
				warn!("Cyl: Navigation reached maximum recursion limit (256) going up");
			}
		}
		current
	} else {
		// If the start is not properly focused (but remains as a ghost focus or similar),
		// We will not navigate away from it during this input, instead aiming to focus it proper so the next input can go through.
		navigation_start
	};
	// The target to truly focus
	let navigation_target = {
		// We descend downwards until a stopping condition is met.
		fn descend_system(
			input: In<(Entity, NavigationDirection)>,
			mut widget_q: Query<
				(Option<&CylSubWidget>, Option<&mut CylNavigator>),
				With<CylWidget>,
			>,
			widget_test: Query<(), With<CylWidget>>,
		) -> Option<Entity> {
			let (mut current, navigation_direction) = *input;
			for iteration in 0..256 {
				let Ok((subwidgets, navigator)) = widget_q.get_mut(current) else {
					warn!("Cyl: Navigation downwards failed with a non-existent entity");
					return None;
				};
				// First look for the navigator's preferred widget
				let mut suggested = if let Some(ref navigator) = navigator {
					match (
						navigator
							.down_widget
							.filter(|&entity| widget_test.contains(entity)),
						navigator.avoid_implicitly_changing_down_widget,
					) {
						(None, true) => {
							break;
						}
						(x, _) => x,
					}
				} else {
					None
				};

				// If that didn't work, look for a child widget, take the first or last depending on where we're coming from
				if suggested.is_none() {
					let Some(subwidgets) = subwidgets else {
						break;
					};
					suggested = if navigator
						.as_deref()
						.cloned()
						.map(|navigator| navigator.axis_split)
						.unwrap_or(AxisSplit::default())
						.split(navigation_direction)
						.1
					{
						subwidgets.first()
					} else {
						subwidgets.last()
					}
					.copied();
				}

				if let Some(mut navigator) = navigator {
					if !navigator.avoid_implicitly_changing_down_widget {
						navigator.down_widget = suggested;
					}
				}

				let Some(next_entity) = suggested else {
					break;
				};
				current = next_entity;

				if iteration == 255 {
					warn!("Cyl: Navigation reached maximum recursion limit (256) going down");
				}
			}
			Some(current)
		}
		let Ok(Some(bottom_entity)) =
			world.run_system_cached_with(descend_system, (navigation_top, navigation_direction))
		else {
			warn!("Cyl: Failed downwards navigation.");
			return;
		};
		bottom_entity
	};
	// Set focus
	world.trigger(CylSetFocus {
		entity: navigation_target,
		reason: SetFocusReason::Navigate(navigation_direction),
	});
}
