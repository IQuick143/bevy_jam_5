//! Persistent storage of the tester's feedback and playthrough log

use crate::persistent::*;
use bevy::{platform::collections::HashMap, prelude::*};
use rand::Rng;
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
	tester_id: u64,
	/// How many sessions has the tester opened before this one
	session_index: u32,
	/// Information about playthrough and feedback on individual levels
	levels: HashMap<String, LevelPlaytestLog>,
	/// General feedback given by the tester,
	/// per question mapped by text key
	pub global_feedback: HashMap<String, String>,
	/// Whether the tester has clicked through the privacy statement page
	pub has_ackd_privacy_statement: bool,
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
	/// Timestamp of the tester entering the level, in 100ths of second
	pub time_entered: u32,
	/// Moves made by the tester in the level
	pub moves: Vec<PlaytestMoveLog>,
}

#[derive(Debug)]
pub struct PlaytestMoveLog {
	/// Timestamp of the move, in 100ths of second
	pub time: u32,
	/// What kind of input from the player translated into the move
	pub turn: PlaytestMove,
}

#[derive(Debug)]
pub enum PlaytestMove {
	Undo,
	Redo,
	Manual {
		/// Index of cycle that the player has attempted to move
		target_cycle: usize,
		/// How much the player tried to move the cycle
		amount: i32,
	},
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub enum LogSerializationScope {
	/// Serialize nothing except the tester ID.
	/// Intended to wipe data server-side if the tester decides so
	Clear,
	/// Serialize only the testers' feedback
	FeedbackOnly,
	/// Serialize all data except technical metadata needed
	/// for testers' experience
	Full,
	/// Serialize the data completely. Intended for persistent storage
	#[default]
	Persistent,
}

impl Saveable for PlaytestLog {
	const FILENAME: &'static str = "playtest_log";

	fn write_json(&self, store: &mut serde_json::Value) {
		self.write_json(store, LogSerializationScope::Persistent);
	}

	fn read_json(&mut self, store: &serde_json::Value) {
		let Value::Object(m) = store else {
			return;
		};
		if let Some(tester_id) = m.get(Self::TESTER_ID).and_then(Value::as_u64) {
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
		if let Some(clicked_through) = m.get(Self::TESTER_CLICKED_THROUGH).and_then(Value::as_bool)
		{
			self.has_ackd_privacy_statement = clicked_through;
		}
	}
}

impl PlaytestLog {
	const TESTER_ID: &str = "tester_id";
	const SESSION_INDEX: &str = "session";
	const LEVELS: &str = "levels";
	const GLOBAL_FEEDBACK: &str = "feedback";
	const TESTER_CLICKED_THROUGH: &str = "opened";
	const VERSION: &str = "version";

	/// Version of the serialization format
	///
	/// Will increase if a breaking change is made
	const PERSISTENCE_VERSION: u32 = 1;

	pub fn write_json(&self, store: &mut serde_json::Value, scope: LogSerializationScope) {
		if !store.is_object() {
			*store = Map::new().into();
		}
		let m = store.as_object_mut().unwrap();

		m.write(Self::VERSION, Self::PERSISTENCE_VERSION);
		m.write(Self::TESTER_ID, self.tester_id);

		// If the tester wants to delete their response, stop here
		if scope <= LogSerializationScope::Clear {
			return;
		}

		let include_logs = scope >= LogSerializationScope::Full;
		let levels = m.put_object_at(Self::LEVELS);
		for (key, level) in &self.levels {
			level.write_json(levels.put_object_at(key), include_logs);
		}

		let global = m.put_object_at(Self::GLOBAL_FEEDBACK);
		for (key, answer) in &self.global_feedback {
			if !answer.is_empty() {
				global.write(key, answer.clone());
			} else {
				global.remove(key);
			}
		}

		// Only save these when saving locally,
		// we do not care about it server-side
		if scope < LogSerializationScope::Persistent {
			return;
		}
		m.write(Self::SESSION_INDEX, self.session_index);
		m.write(
			Self::TESTER_CLICKED_THROUGH,
			self.has_ackd_privacy_statement,
		);
	}

	/// Initialization to be run immediately after
	/// the resource is loaded
	pub fn after_load(&mut self) {
		if self.tester_id == 0 {
			self.first_initialize();
		} else {
			self.after_reload();
		}
	}

	pub fn session_index(&self) -> u32 {
		self.session_index
	}

	pub fn tester_id(&self) -> u64 {
		self.tester_id
	}

	fn first_initialize(&mut self) {
		self.initialize_tester_id();
		self.session_index = 0;
	}

	fn after_reload(&mut self) {
		self.session_index += 1;
	}

	fn initialize_tester_id(&mut self) {
		// Leave the top bit zero
		self.tester_id = rand::rng().random::<u64>() & !(1 << 63);
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

	fn write_json(
		&self,
		m: &mut serde_json::Map<String, serde_json::Value>,
		include_game_log: bool,
	) {
		// Zero means the field is unfilled
		if self.stars != 0 {
			m.write(Self::STAR_RATING, self.stars);
		}
		if self.difficulty != 0 {
			m.write(Self::DIFFICULTY_RATING, self.difficulty);
		}
		if !self.comment.is_empty() {
			m.write(Self::EXTRA_FEEDBACK, self.comment.as_str());
		} else {
			m.remove(Self::EXTRA_FEEDBACK);
		}
		if include_game_log && !self.sessions.is_empty() {
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
	pub fn new(game_session: u32, time_entered: u32) -> Self {
		Self {
			game_session,
			time_entered,
			moves: Vec::new(),
		}
	}

	fn to_json(&self) -> Vec<Value> {
		let mut m = Vec::new();
		m.push(self.game_session.into());
		m.push(self.time_entered.into());
		if let Ok(move_log) = serialize_move_list(&self.moves)
			&& !move_log.is_empty()
		{
			m.push(move_log.into());
		}
		m
	}

	fn from_json(m: &Value) -> Result<Self, ()> {
		let Some(game_session) = m
			.get(0)
			.and_then(Value::as_u64)
			.and_then(|i| u32::try_from(i).ok())
		else {
			return Err(());
		};

		let Some(time_entered) = m
			.get(1)
			.and_then(Value::as_u64)
			.and_then(|i| u32::try_from(i).ok())
		else {
			return Err(());
		};

		let moves = m
			.get(2)
			.and_then(Value::as_str)
			.and_then(|s| deserialise_move_list(s).ok())
			.unwrap_or_default();

		Ok(Self {
			game_session,
			time_entered,
			moves,
		})
	}
}

fn base64_character_to_u8(character: char) -> Option<u8> {
	let num = character as u32;
	let uppercase_a = 'A' as u32;
	let lowercase_a = 'a' as u32;
	let zero = '0' as u32;
	match character {
		'A'..='Z' => Some((num - uppercase_a) as u8),
		'a'..='z' => Some((num - lowercase_a) as u8 + 26),
		'0'..='9' => Some((num - zero) as u8 + 52),
		'+' => Some(62),
		'-' => Some(63),
		_ => None,
	}
}

fn base64_to_character(value: u8) -> Option<char> {
	match value {
		0..=25 => Some((value + b'A') as char),
		26..=51 => Some(((value - 26) + b'a') as char),
		52..=61 => Some(((value - 52) + b'0') as char),
		62 => Some('+'),
		63 => Some('-'),
		_ => None,
	}
}

#[test]
fn test_base64() {
	for i in 0..=63 {
		assert_eq!(
			base64_to_character(i).map(base64_character_to_u8),
			Some(Some(i))
		);
	}
}

fn serialise_varlen_base32_number(
	mut number: u64,
	writer: &mut impl std::fmt::Write,
) -> std::fmt::Result {
	let mut digits = smallvec::SmallVec::<[u8; 13]>::new();
	if number == 0 {
		digits.push(0);
	}
	while number > 0 {
		let digit = (number % 32) as u8;
		number >>= 5;
		digits.push(digit);
	}
	for digit in digits.iter().skip(1).rev() {
		write!(writer, "{}", base64_to_character(digit + 32).unwrap())?;
	}
	// Last digit (which is first in the vector)
	write!(writer, "{}", base64_to_character(digits[0]).unwrap())?;
	Ok(())
}

fn deserialise_varlen_base32_number(characters: &mut impl Iterator<Item = char>) -> Option<u64> {
	let mut current_character = characters.next()?;
	let mut value = base64_character_to_u8(current_character)?;
	let mut result = (value & 0b00011111) as u64;
	// Continue until we reach a value without a top bit
	while value & 0b00100000 != 0 {
		if result >= (1 << (64 - 5)) {
			// Overflow
			return None;
		}
		result <<= 5;
		current_character = characters.next()?;
		value = base64_character_to_u8(current_character)?;
		result += (value & 0b00011111) as u64;
	}
	Some(result)
}

#[test]
fn test_base64_serde() {
	for i in 0..=2000 {
		let mut string = String::new();
		serialise_varlen_base32_number(i, &mut string).unwrap();
		let value = deserialise_varlen_base32_number(&mut string.chars()).unwrap();
		assert_eq!(value, i);
	}
	assert_eq!(
		deserialise_varlen_base32_number(
			&mut "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxA".chars()
		),
		None
	);
}

impl std::fmt::Display for PlaytestMoveLog {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut write_move = None;
		match self.turn {
			PlaytestMove::Manual {
				amount,
				target_cycle,
			} => {
				write!(f, "{}", if amount > 0 { '+' } else { '-' })?;
				write_move = Some(target_cycle);
			}
			PlaytestMove::Undo => write!(f, "u")?,
			PlaytestMove::Redo => write!(f, "r")?,
		}
		serialise_varlen_base32_number(self.time as u64, f)?;
		if let Some(cycle) = write_move {
			serialise_varlen_base32_number(cycle as u64, f)?;
		}
		Ok(())
	}
}

fn serialize_move_list(moves: &[PlaytestMoveLog]) -> Result<String, std::fmt::Error> {
	let mut move_log = String::new();
	for mv in moves {
		use std::fmt::Write;
		write!(move_log, "{mv}")?;
	}
	Ok(move_log)
}

fn deserialise_move_list(data: &str) -> Result<Vec<PlaytestMoveLog>, ()> {
	let mut iter = data.chars();
	let mut moves = Vec::new();
	while let Some(first_char) = iter.next() {
		match first_char {
			'+' | '-' => {
				let Some(time) = deserialise_varlen_base32_number(&mut iter) else {
					return Err(());
				};
				let Some(cycle) = deserialise_varlen_base32_number(&mut iter) else {
					return Err(());
				};
				let amount = if first_char == '+' { 1 } else { -1 };
				moves.push(PlaytestMoveLog {
					time: u32::try_from(time).map_err(|_| ())?,
					turn: PlaytestMove::Manual {
						target_cycle: usize::try_from(cycle).map_err(|_| ())?,
						amount,
					},
				})
			}
			'u' | 'r' => {
				let Some(time) = deserialise_varlen_base32_number(&mut iter) else {
					return Err(());
				};
				moves.push(PlaytestMoveLog {
					time: u32::try_from(time).map_err(|_| ())?,
					turn: if first_char == 'u' {
						PlaytestMove::Undo
					} else {
						PlaytestMove::Redo
					},
				})
			}
			_ => {
				return Err(());
			}
		}
	}
	Ok(moves)
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn success_serde() {
		let original = "-kvoFC+kvqdC-kvtDB-kvucB-kvwMB+kvwbB-kv0QC-kv3NB+kv5YB-kv8XB+kv+aB+kwgRB+kwkYB-kwmIB-kwnVB+kwpTB-kwvRG-kwyYB-kw2QF-kw6NE-kw8EE-kxhVD-kxiOD-kxlLC-kxmfC+kxqDH-kxtRH+kxwOH+kxyKB+kx1TB+kx6BG-kx9IB-kyoGB-kysKE+kyvHD-kyyQC+ky1FC+ky3DC-ky6WC-kzjEBukzsdukztMukztbukzubukzvXukzwQukzyE";
		let moves = deserialise_move_list(original).unwrap();
		let result = serialize_move_list(&moves).unwrap();
		assert_eq!(original, result);
	}
}
