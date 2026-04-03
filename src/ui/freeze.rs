//! Allows entities to freeze the UI

use super::*;
use bevy::ecs::system::SystemParam;

/// If an entity with this component exists, all UI inputs are discarded
#[derive(Component, Clone, Copy, Debug, Default)]
pub struct FreezeUi;

/// [`SystemParam`] that checks whether the UI is currently frozen
#[derive(SystemParam)]
pub struct IsUiFrozen<'w, 's> {
	freezer_query: Query<'w, 's, Ref<'static, FreezeUi>>,
	removed_freezers: RemovedComponents<'w, 's, FreezeUi>,
}

impl IsUiFrozen<'_, '_> {
	pub fn get(&self) -> bool {
		!self.freezer_query.is_empty()
	}

	pub fn changed(&self) -> bool {
		let just_froze = self.freezer_query.iter().all(|f| f.is_added());
		let just_unfroze = !self.removed_freezers.is_empty();
		just_froze != just_unfroze
	}
}

/// [`SystemCondition`] that succeeds when the UI is not frozen
/// by any [`FreezeUi`] entities.
pub fn ui_not_frozen(is_frozen: IsUiFrozen) -> bool {
	!is_frozen.get()
}
