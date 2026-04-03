use bevy::prelude::*;
use crate::ui::cyl::prelude::*;

mod harness {
	use bevy::prelude::*;

	use crate::ui::cyl::{
		events::CylInteract,
		input::{InteractInput, NavigationInput},
		prelude::{CylSetFocus, NavigationDirection},
	};

	#[derive(Component)]
	pub struct TestId(pub usize);

	/// Log of all [`CylInteract`] events, sotring their [`TestId`]
	#[derive(Resource, Default)]
	pub struct InteractionLog(pub Vec<Option<usize>>);

	/// Log of all [`CylSetFocus`] events, sotring their [`TestId`]
	#[derive(Resource, Default)]
	pub struct SetFocusLog(pub Vec<Option<usize>>);

	pub struct Test {
		app: App,
	}

	impl Test {
		pub fn new() -> Self {
			let mut app = App::new();
			app.add_plugins(super::super::plugin);
			app.init_resource::<InteractionLog>();
			app.init_resource::<SetFocusLog>();
			app.add_observer(
				|trigger: On<CylInteract>,
				 mut log: ResMut<InteractionLog>,
				 id_q: Query<&TestId>| {
					log.0
						.push(id_q.get(trigger.0).ok().map(|test_id| test_id.0))
				},
			);
			app.add_observer(
				|trigger: On<CylSetFocus>, mut log: ResMut<SetFocusLog>, id_q: Query<&TestId>| {
					log.0
						.push(id_q.get(trigger.entity).ok().map(|test_id| test_id.0))
				},
			);
			Test { app }
		}

		pub fn spawn(&mut self, bundle: impl Bundle) -> Entity {
			self.app.world_mut().spawn(bundle).id()
		}

		pub fn enter(&mut self) {
			self.app.world_mut().trigger(InteractInput);
			self.tick();
		}

		pub fn navigate(&mut self, navigation_direction: NavigationDirection) {
			self.app
				.world_mut()
				.trigger(NavigationInput(navigation_direction));
			self.tick();
		}

		pub fn tick(&mut self) {
			self.app.update();
		}

		pub fn clear_logs(&mut self) {
			self.app.insert_resource(InteractionLog::default());
			self.app.insert_resource(SetFocusLog::default());
		}

		pub fn assert_interaction_log(&mut self, desired_interactions: &[usize]) {
			println!("Validating interaction");
			let records = &self.app.world().resource::<InteractionLog>().0;
			Self::compare_log_vectors(records, desired_interactions);
		}

		pub fn assert_focus_log(&mut self, desired_foci: &[usize]) {
			println!("Validating focus");
			let records = &self.app.world().resource::<SetFocusLog>().0;
			Self::compare_log_vectors(records, desired_foci);
		}

		fn compare_log_vectors(records: &[Option<usize>], desired: &[usize]) {
			let desired = desired.iter().map(|x| Some(*x)).collect::<Vec<_>>();
			assert_eq!(records, &desired);
		}
	}
}

use harness::*;

use crate::ui::cyl::widgets::CylButton;

#[test]
fn basic_interact() {
	let mut test = Test::new();
	test.spawn((TestId(0), CylButton));
	test.enter(); // This should focus the button
	test.enter(); // This should trigger the button
	test.assert_focus_log(&[0]);
	test.assert_interaction_log(&[0]);
}

#[test]
fn basic_nav_and_interact() {
	let mut test = Test::new();
	test.spawn((TestId(0), CylButton));
	test.navigate(NavigationDirection::Down); // This should focus the button
	test.enter(); // This should trigger the button
	test.assert_focus_log(&[0]);
	test.assert_interaction_log(&[0]);
}

#[test]
fn basic_nav_to_interact() {
	let mut test = Test::new();
	test.spawn((TestId(0), CylButton));
	test.navigate(NavigationDirection::Down); // This should focus the button
	test.navigate(NavigationDirection::Down); // This should still just focus the button
	test.assert_focus_log(&[0, 0]);
	test.navigate(NavigationDirection::Right); // This should activate the button
	test.assert_interaction_log(&[0]);
	test.assert_focus_log(&[0, 0]);
	test.navigate(NavigationDirection::Left); // This should also activate the button
	test.assert_interaction_log(&[0,0]);
}

#[test]
fn basic_container() {
	let mut test = Test::new();
	let container = test.spawn((TestId(1000), CylWidget, CylNavigator::default()));
	test.spawn((TestId(0), CylWidget, CylParentWidget(container), CylNavigator::default()));
	test.spawn((TestId(1), CylWidget, CylParentWidget(container), CylNavigator::default()));
	test.spawn((TestId(2), CylWidget, CylParentWidget(container), CylNavigator::default()));
	test.spawn((TestId(3), CylWidget, CylParentWidget(container), CylNavigator::default()));
	test.navigate(NavigationDirection::Down);
	test.enter();
	test.navigate(NavigationDirection::Down);
	test.navigate(NavigationDirection::Down);
	test.navigate(NavigationDirection::Down);
	test.enter();
	test.assert_focus_log(&[0, 1, 2, 3]);
	test.assert_interaction_log(&[0, 3]);
	test.clear_logs();
	test.navigate(NavigationDirection::Up);
	test.navigate(NavigationDirection::Up);
	test.navigate(NavigationDirection::Up);
	test.assert_focus_log(&[2,1,0]);
}

#[test]
fn basic_container_loop_down() {
	let mut test = Test::new();
	let container = test.spawn((TestId(1000), CylWidget, CylNavigator::default()));
	test.spawn((TestId(0), CylWidget, CylParentWidget(container), CylNavigator::default()));
	test.spawn((TestId(1), CylWidget, CylParentWidget(container), CylNavigator::default()));
	test.spawn((TestId(2), CylWidget, CylParentWidget(container), CylNavigator::default()));
	test.spawn((TestId(3), CylWidget, CylParentWidget(container), CylNavigator::default()));
	test.navigate(NavigationDirection::Down);
	test.enter();
	test.navigate(NavigationDirection::Down);
	test.enter();
	test.navigate(NavigationDirection::Down);
	test.enter();
	test.navigate(NavigationDirection::Down);
	test.enter();
	test.assert_focus_log(&[0, 1, 2, 3]);
	test.assert_interaction_log(&[0, 1, 2, 3]);
	test.clear_logs();
	test.navigate(NavigationDirection::Down);
	test.enter();
	test.navigate(NavigationDirection::Down);
	test.enter();
	test.navigate(NavigationDirection::Down);
	test.enter();
	test.navigate(NavigationDirection::Down);
	test.enter();
	test.assert_focus_log(&[0, 1, 2, 3]);
	test.assert_interaction_log(&[0, 1, 2, 3]);
}

#[test]
fn basic_container_loop_up() {
	let mut test = Test::new();
	let container = test.spawn((TestId(1000), CylWidget, CylNavigator::default()));
	test.spawn((TestId(0), CylWidget, CylParentWidget(container), CylNavigator::default()));
	test.spawn((TestId(1), CylWidget, CylParentWidget(container), CylNavigator::default()));
	test.spawn((TestId(2), CylWidget, CylParentWidget(container), CylNavigator::default()));
	test.spawn((TestId(3), CylWidget, CylParentWidget(container), CylNavigator::default()));
	test.navigate(NavigationDirection::Up);
	test.enter();
	test.navigate(NavigationDirection::Up);
	test.enter();
	test.navigate(NavigationDirection::Up);
	test.enter();
	test.navigate(NavigationDirection::Up);
	test.enter();
	test.assert_focus_log(&[3, 2, 1, 0]);
	test.assert_interaction_log(&[3, 2, 1, 0]);
	test.clear_logs();
	test.navigate(NavigationDirection::Up);
	test.enter();
	test.navigate(NavigationDirection::Up);
	test.enter();
	test.navigate(NavigationDirection::Up);
	test.enter();
	test.navigate(NavigationDirection::Up);
	test.enter();
	test.assert_focus_log(&[3, 2, 1, 0]);
	test.assert_interaction_log(&[3, 2, 1, 0]);
}
