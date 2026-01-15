//! Persistent storage of the tester's feedback and playthrough log

use crate::persistent::*;
use bevy::{asset::uuid::Uuid, prelude::*};
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
	}
}

impl PlaytestLog {
	const TESTER_ID: &str = "tester_id";
	const SESSION_INDEX: &str = "session";

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
}
