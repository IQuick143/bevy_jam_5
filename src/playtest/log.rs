//! Persistent storage of the tester's feedback and playthrough log

use crate::{
	game::{
		logic_relay::{RotateCycleGroup, RotationCause},
		prelude::RotateCycle,
	},
	persistent::*,
};
use bevy::{asset::uuid::Uuid, platform::collections::HashMap, prelude::*};
use rand::RngCore as _;
use serde_json::{Map, Value};

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
	/// General feedback given by the tester,
	/// per question mapped by text key
	pub global_feedback: HashMap<String, String>,
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
	/// Move logs from all sessions of the level
	pub sessions: Vec<LevelSessionPlaytestLog>,
}

/// Play log from a session over a particular level
#[derive(Debug)]
pub struct LevelSessionPlaytestLog {
	/// Index of the session of the game
	pub game_session: u32,
	/// Timestamp of the tester entering the level
	pub time_entered: f32,
	/// Moves made by the tester in the level
	pub moves: Vec<PlaytestMoveLog>,
}

#[derive(Debug)]
pub struct PlaytestMoveLog {
	/// Timestamp of the move
	pub time: f32,
	/// Action that the player has attempted
	pub rotation: RotateCycleGroup,
	/// Whether the move actually went through
	pub succeeded: bool,
}

impl Saveable for PlaytestLog {
	const FILENAME: &'static str = "playtest_log";

	fn write_json(&self, store: &mut serde_json::Value) {
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

		let global = m.put_object_at(Self::GLOBAL_FEEDBACK);
		for (key, answer) in &self.global_feedback {
			if !answer.is_empty() {
				global.write(key, answer.clone());
			}
		}
	}

	fn read_json(&mut self, store: &serde_json::Value) {
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
		if let Some(global) = m.get(Self::GLOBAL_FEEDBACK).and_then(Value::as_object) {
			for (key, answer) in global {
				if let Some(answer) = answer.as_str() {
					self.global_feedback
						.insert(key.to_owned(), answer.to_owned());
				}
			}
		}
	}
}

impl PlaytestLog {
	const TESTER_ID: &str = "tester_id";
	const SESSION_INDEX: &str = "session";
	const LEVELS: &str = "levels";
	const GLOBAL_FEEDBACK: &str = "feedback";

	/// Initialization to be run immediately after
	/// the resource is loaded
	pub fn after_load(&mut self) {
		if self.tester_id.is_nil() {
			self.first_initialize();
		} else {
			self.after_reload();
		}
	}

	pub fn session_index(&self) -> u32 {
		self.session_index
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
	const PLAY_LOG: &str = "sessions";

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
		if !self.sessions.is_empty() {
			let list = self
				.sessions
				.iter()
				.map(LevelSessionPlaytestLog::to_json)
				.collect::<Vec<_>>();
			m.write(Self::PLAY_LOG, list);
		}
	}

	fn read_json(&mut self, j: &serde_json::Value) {
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

		if let Some(sessions) = m.get(Self::PLAY_LOG).and_then(Value::as_array) {
			self.sessions = sessions
				.iter()
				.map(LevelSessionPlaytestLog::from_json)
				.filter_map(Result::ok)
				.collect();
		}
	}

	pub fn is_rated(&self) -> bool {
		self.stars > 0
	}
}

impl LevelSessionPlaytestLog {
	const SESSION_INDEX: &str = "session";
	const ENTER_TIME: &str = "enter";
	const MOVE_LOG: &str = "moves";

	pub fn new(game_session: u32, time_entered: f32) -> Self {
		Self {
			game_session,
			time_entered,
			moves: Vec::new(),
		}
	}

	fn to_json(&self) -> Map<String, Value> {
		let mut m = Map::new();
		m.write(Self::SESSION_INDEX, self.game_session);
		m.write(Self::ENTER_TIME, self.time_entered);
		let list = self
			.moves
			.iter()
			.map(PlaytestMoveLog::to_json)
			.collect::<Vec<_>>();
		m.write(Self::MOVE_LOG, list);
		m
	}

	fn from_json(m: &Value) -> Result<Self, ()> {
		let Some(game_session) = m
			.get(Self::SESSION_INDEX)
			.and_then(Value::as_u64)
			.and_then(|i| u32::try_from(i).ok())
		else {
			return Err(());
		};

		let Some(time_entered) = m.get(Self::ENTER_TIME).and_then(Value::as_f64) else {
			return Err(());
		};

		let moves = m
			.get(Self::MOVE_LOG)
			.and_then(Value::as_array)
			.into_iter()
			.flatten()
			.map(PlaytestMoveLog::from_json)
			.filter_map(Result::ok)
			.collect();

		Ok(Self {
			game_session,
			time_entered: time_entered as f32,
			moves,
		})
	}
}

impl PlaytestMoveLog {
	const TIMESTAMP: &str = "time";
	const SUCCEEDED: &str = "succeeded";
	const ROTATION_CAUSE: &str = "cause";
	const TARGET_CYCLE: &str = "target";
	const ROTATE_BY: &str = "amount";

	fn to_json(&self) -> Map<String, Value> {
		let mut m = Map::new();
		m.write(Self::TIMESTAMP, self.time);
		if !self.succeeded {
			m.write(Self::SUCCEEDED, self.succeeded);
		}
		if self.rotation.cause != RotationCause::default() {
			m.write(Self::ROTATION_CAUSE, self.rotation.cause);
		}
		m.write(Self::TARGET_CYCLE, self.rotation.rotation.target_cycle);
		if self.rotation.rotation.amount != 1 {
			m.write(Self::ROTATE_BY, self.rotation.rotation.amount);
		}
		m
	}

	fn from_json(m: &Value) -> Result<Self, ()> {
		let timestamp = m.get(Self::TIMESTAMP).and_then(Value::as_f64).ok_or(())?;
		let succeeded = m
			.get(Self::SUCCEEDED)
			.and_then(Value::as_bool)
			.unwrap_or(true);
		let cause = m
			.get(Self::ROTATION_CAUSE)
			.ok_or(())
			.and_then(RotationCause::try_from)
			.unwrap_or_default();
		let target_cycle = m
			.get(Self::TARGET_CYCLE)
			.and_then(Value::as_u64)
			.and_then(|x| usize::try_from(x).ok())
			.ok_or(())?;
		let amount = m.get(Self::ROTATE_BY).and_then(Value::as_i64).unwrap_or(1);
		Ok(Self {
			time: timestamp as f32,
			succeeded,
			rotation: RotateCycleGroup {
				cause,
				rotation: RotateCycle {
					target_cycle,
					amount,
				},
			},
		})
	}
}

impl RotationCause {
	const KEY_MANUAL: &str = "manual";
	const KEY_UNDO: &str = "undo";
}

impl From<RotationCause> for Value {
	fn from(value: RotationCause) -> Self {
		match value {
			RotationCause::Manual => RotationCause::KEY_MANUAL.into(),
			RotationCause::Undo => RotationCause::KEY_UNDO.into(),
		}
	}
}

impl TryFrom<&Value> for RotationCause {
	type Error = ();
	fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
		if value == Self::KEY_MANUAL {
			Ok(Self::Manual)
		} else if value == Self::KEY_UNDO {
			Ok(Self::Undo)
		} else {
			Err(())
		}
	}
}
