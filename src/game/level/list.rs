//! The level list and construction thereof

use bevy::reflect::Reflect;

/// A list of levels split into sections
#[derive(Clone, Debug, Reflect)]
pub struct LevelList {
	/// Sections of the level list
	pub sections: Vec<LevelListSection>,
}

/// A user-facing categorization of levels
#[derive(Clone, Debug, Reflect)]
pub struct LevelListSection {
	/// Display name of the section, if any
	pub name: Option<String>,
	/// Indices of levels that belong to this section.
	/// Intentionally a range as level sections must be presented as such.
	pub level_indices: std::ops::Range<usize>,
}

/// Helper object for construction of a level list
pub struct LevelListBuilder {
	list: LevelList,
	level_count: usize,
}

#[derive(Debug)]
pub enum LevelListBuildError {
	/// [`declare_level`](LevelListBuilder::declare_level) was called
	/// before [`begin_section`](LevelListBuilder::begin_section)
	LevelDeclaredBeforeSection,
	/// [`set_current_section_name`](LevelListBuilder::declare_level) was called
	/// before [`begin_section`](LevelListBuilder::begin_section)
	NameDeclaredBeforeSection,
	/// [`set_current_section_name`](LevelListBuilder::declare_level) was called
	/// more than once on the same section
	SectionDisplayNameRedefined,
}

impl LevelListBuilder {
	pub fn new() -> Self {
		Self {
			list: LevelList {
				sections: Vec::new(),
			},
			level_count: 0,
		}
	}

	pub fn begin_section(&mut self, _section_id: Option<&str>) -> Result<(), LevelListBuildError> {
		self.list.sections.push(LevelListSection {
			name: None,
			level_indices: self.level_count..self.level_count,
		});
		Ok(())
	}

	pub fn declare_level(&mut self, _level_id: &str) -> Result<(), LevelListBuildError> {
		let Some(section) = self.list.sections.last_mut() else {
			return Err(LevelListBuildError::LevelDeclaredBeforeSection);
		};
		section.level_indices.end += 1;
		self.level_count += 1;
		Ok(())
	}

	pub fn set_current_section_name(
		&mut self,
		display_name: &str,
	) -> Result<(), LevelListBuildError> {
		let Some(section) = self.list.sections.last_mut() else {
			return Err(LevelListBuildError::NameDeclaredBeforeSection);
		};
		if section.name.is_some() {
			return Err(LevelListBuildError::SectionDisplayNameRedefined);
		}
		section.name = Some(display_name.to_owned());
		Ok(())
	}

	pub fn build(self) -> Result<LevelList, LevelListBuildError> {
		Ok(self.list)
	}
}

impl std::error::Error for LevelListBuildError {}

impl std::fmt::Display for LevelListBuildError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::LevelDeclaredBeforeSection => write!(
				f,
				"A level was declared before starting a level list section."
			),
			Self::NameDeclaredBeforeSection => write!(
				f,
				"A level list section name was assigned before starting a section."
			),
			Self::SectionDisplayNameRedefined => write!(
				f,
				"A level list section was assigned more than one display name."
			),
		}
	}
}
