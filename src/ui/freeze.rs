//! Allows systems to freeze the UI

use super::*;

pub(super) fn plugin(app: &mut App) {
	app.init_resource::<FreezeUi>();
}

/// Indicates whether the UI should discard all inputs
#[derive(Resource, Debug, Default)]
pub struct FreezeUi(pub bool);

/// [`Condition`] that succeeds when the UI is not frozen
pub fn ui_not_frozen(freeze: Res<FreezeUi>) -> bool {
	!freeze.0
}
