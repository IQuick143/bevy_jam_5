//! Persistent storage of the tester's feedback and playthrough log

use crate::{
	game::{
		logic_relay::{RotateCycleGroup, RotationCause},
		prelude::RotateCycle,
	},
	persistent::*,
};
use bevy::{platform::collections::HashMap, prelude::*};
use logos::Logos;
use pomelo::pomelo;
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
	/// Action that the player has attempted
	pub rotation: RotateCycleGroup,
	/// Whether the move actually went through
	pub succeeded: bool,
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
	const SESSION_INDEX: &str = "session";
	const ENTER_TIME: &str = "enter";
	const MOVE_LOG: &str = "moves";

	pub fn new(game_session: u32, time_entered: u32) -> Self {
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
		if let Ok(move_log) = serialize_move_list(&self.moves) {
			if !move_log.is_empty() {
				m.write(Self::MOVE_LOG, move_log);
			}
		}
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

		let Some(time_entered) = m
			.get(Self::ENTER_TIME)
			.and_then(Value::as_u64)
			.and_then(|i| u32::try_from(i).ok())
		else {
			return Err(());
		};

		let moves = m
			.get(Self::MOVE_LOG)
			.and_then(Value::as_str)
			.and_then(|s| move_list_parser::parse(s).ok())
			.unwrap_or_default();

		Ok(Self {
			game_session,
			time_entered,
			moves,
		})
	}
}

impl std::fmt::Display for PlaytestMoveLog {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let amount = self.rotation.rotation.amount;
		let cause = self.rotation.cause;
		let cycle = self.rotation.rotation.target_cycle;

		write!(f, "{}", self.time)?;
		write!(f, "{}", if amount > 0 { '+' } else { '-' })?;
		if amount.abs() != 1 {
			write!(f, "{}:", amount.abs())?;
		}
		write!(f, "{cycle}")?;
		if !self.succeeded {
			write!(f, "F")?;
		}
		match cause {
			RotationCause::Manual => {}
			RotationCause::Undo => write!(f, "u")?,
		}
		Ok(())
	}
}

fn serialize_move_list(moves: &[PlaytestMoveLog]) -> Result<String, std::fmt::Error> {
	let mut move_log = String::new();
	for mv in moves {
		use std::fmt::Write;
		write!(move_log, "{mv};")?;
	}
	// Drop the trailing semicolon
	move_log.pop();
	Ok(move_log)
}

pomelo! {
	%module move_list_parser;
	%include {
		use super::*;

		pub fn parse(s: &str) -> Result<Vec<PlaytestMoveLog>, ()> {
			let mut lexer = Token::lexer(s);
			let mut parser = Parser::new();
			for token in lexer.flatten() {
				// Parser error is irrecoverable,
				// the parser will just keep failing
				parser.parse(token)?;
			}
			parser.end_of_input()
		}
	}

	%syntax_error { Ok(()) }

	%token
	#[derive(Logos)]
	enum Token {};

	%type
	#[regex(r"\d+", |lex| lex.slice().parse().map_err(|_| ()))]
	Int u32;

	%type
	#[token("+", |_| 1)]
	#[token("-", |_| -1)]
	Sign i32;

	%type
	#[token("u", |_| RotationCause::Undo)]
	Cause RotationCause;

	%type
	#[token("F")]
	Fail;

	%type
	#[token(";")]
	Semi;

	%type
	#[token(":")]
	Colon;

	// Underlying types of nonterminals
	%type output Vec<PlaytestMoveLog>;
	%type list   Vec<PlaytestMoveLog>;
	%type entry  PlaytestMoveLog;
	%type rot    RotateCycle;

	output ::= { Vec::new() }
	output ::= list;

	list ::= entry(m) { vec![m] }
	list ::= list error;
	list ::= list(mut l) Semi entry(m) { l.push(m); l }

	entry ::= Int(time) rot(rotation) Fail?(f) Cause?(cause) {
		PlaytestMoveLog {
			time,
			succeeded: f.is_none(),
			rotation: RotateCycleGroup {
				rotation,
				cause: cause.unwrap_or_default()
			}
		}
	}

	rot ::= Sign(s) Int(cycle) {
		RotateCycle {
			target_cycle: cycle as usize,
			amount: s as i64,
		}
	}

	rot ::= Sign(s) Int(abs) Colon Int(cycle) {
		RotateCycle {
			target_cycle: cycle as usize,
			amount: s as i64 * abs as i64,
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn success_serde() {
		let original = "1234+0;1352-2:1u;4353+2F;4531-2Fu";
		let moves = move_list_parser::parse(original).unwrap();
		let result = serialize_move_list(&moves).unwrap();
		assert_eq!(original, result);
	}
}
