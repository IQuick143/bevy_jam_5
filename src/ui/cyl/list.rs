//! Cyl list container widget

use super::CylWidget;
use bevy::prelude::*;

pub(super) fn plugin(_app: &mut App) {}

/// Marker component for a list container widget
#[derive(Component, Clone, Copy, Default)]
#[require(CylWidget)]
pub struct CylListWidget;
