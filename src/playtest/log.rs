//! Persistent storage of the tester's feedback and playthrough log

use crate::persistent::*;
use bevy::{asset::uuid::Uuid, platform::collections::HashMap, prelude::*};
use rand::RngCore as _;

pub(super) fn plugin(app: &mut App) {
	app.register_saveable_resource::<PlaytestLog>().add_systems(
		Startup,
		(|mut log: ResMut<PlaytestLog>| log.after_load()).after(LoadPersistentResourcesSystems),
	);
}

#[derive(Resource, Debug, Default)]
pub struct PlaytestLog {
	/// Randomly generated and hopefully unique identifier of a playtester
	tester_id: Uuid,
	/// How many sessions has the tester opened before this one
	session_index: u32,
	/// Information about playthrough and feedback on individual levels
	levels: HashMap<String, LevelPlaytestLog>,
}

/// Play log and feedback to a particular level
#[derive(Debug, Default)]
pub struct LevelPlaytestLog {
	/// Star rating given by the user
	pub stars: u8,
	/// Subjective difficulty given by the user
	pub difficulty: u8,
	/// Additional comments left by the user
	pub comment: String,
}

impl Saveable for PlaytestLog {
	const FILENAME: &'static str = "playtest_log";

	fn write_json(&self, store: &mut serde_json::Value) {
		use serde_json::Map;
		if !store.is_object() {
			*store = Map::new().into();
		}
		let m = store.as_object_mut().unwrap();

		m.write(Self::TESTER_ID, self.tester_id.to_string());
		m.write(Self::SESSION_INDEX, self.session_index);

		let levels = m.put_object_at(Self::LEVELS);
		for (key, level) in &self.levels {
			level.write_json(levels.put_object_at(key));
		}
	}

	fn read_json(&mut self, store: &serde_json::Value) {
		use serde_json::Value;
		let Value::Object(m) = store else {
			return;
		};
		if let Some(tester_id) = m
			.get(Self::TESTER_ID)
			.and_then(Value::as_str)
			.and_then(|s| Uuid::parse_str(s).ok())
		{
			self.tester_id = tester_id;
		}
		if let Some(session_index) = m
			.get(Self::SESSION_INDEX)
			.and_then(Value::as_u64)
			.and_then(|i| u32::try_from(i).ok())
		{
			self.session_index = session_index;
		}
		if let Some(levels) = m.get(Self::LEVELS).and_then(Value::as_object) {
			for (key, level) in levels {
				self.level_mut(key.clone()).read_json(level);
			}
		}
	}
}

impl PlaytestLog {
	const TESTER_ID: &str = "tester_id";
	const SESSION_INDEX: &str = "session";
	const LEVELS: &str = "levels";

	/// Initialization to be run immediately after
	/// the resource is loaded
	pub fn after_load(&mut self) {
		if self.tester_id.is_nil() {
			self.first_initialize();
		} else {
			self.after_reload();
		}
	}

	fn first_initialize(&mut self) {
		self.initialize_tester_id();
		self.session_index = 0;
	}

	fn after_reload(&mut self) {
		self.session_index += 1;
	}

	fn initialize_tester_id(&mut self) {
		let mut uuid_bytes = [0u8; 16];
		rand::rng().fill_bytes(&mut uuid_bytes);
		self.tester_id = Uuid::from_bytes(uuid_bytes);
	}

	pub fn get_level(&self, key: &str) -> Option<&LevelPlaytestLog> {
		self.levels.get(key)
	}

	pub fn level_mut(&mut self, key: String) -> &mut LevelPlaytestLog {
		self.levels.entry(key).or_default()
	}

	pub fn is_level_rated(&self, key: &str) -> bool {
		self.get_level(key).is_some_and(LevelPlaytestLog::is_rated)
	}
}

impl LevelPlaytestLog {
	const STAR_RATING: &str = "stars";
	const DIFFICULTY_RATING: &str = "difficulty";
	const EXTRA_FEEDBACK: &str = "comment";

	fn write_json(&self, m: &mut serde_json::Map<String, serde_json::Value>) {
		// Zero means the field is unfilled
		if self.stars != 0 {
			m.write(Self::STAR_RATING, self.stars);
		}
		if self.difficulty != 0 {
			m.write(Self::DIFFICULTY_RATING, self.difficulty);
		}
		if !self.comment.is_empty() {
			m.write(Self::EXTRA_FEEDBACK, self.comment.as_str());
		}
	}

	fn read_json(&mut self, j: &serde_json::Value) {
		use serde_json::Value;
		let Value::Object(m) = j else {
			return;
		};

		if let Some(stars) = m
			.get(Self::STAR_RATING)
			.and_then(Value::as_u64)
			.and_then(|i| u8::try_from(i).ok())
		{
			self.stars = stars;
		}

		if let Some(difficulty) = m
			.get(Self::DIFFICULTY_RATING)
			.and_then(Value::as_u64)
			.and_then(|i| u8::try_from(i).ok())
		{
			self.difficulty = difficulty;
		}

		if let Some(comment) = m.get(Self::EXTRA_FEEDBACK).and_then(Value::as_str) {
			self.comment = comment.to_owned();
		}
	}

	pub fn is_rated(&self) -> bool {
		self.stars > 0
	}
}
