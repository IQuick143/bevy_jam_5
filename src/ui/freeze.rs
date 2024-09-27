//! Allows entities to freeze the UI

use super::*;

/// If an entity with this component exists, all UI inputs are discarded
#[derive(Component, Clone, Copy, Debug, Default)]
pub struct FreezeUi;

/// [`Condition`] that succeeds when the UI is not frozen
/// by any [`FreezeUi`] entities.
pub fn ui_not_frozen(query: Query<(), With<FreezeUi>>) -> bool {
	query.is_empty()
}
