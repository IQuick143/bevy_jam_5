//! Handles sending playtest logs to the server

use super::log::{LogSerializationScope, PlaytestLog};
use bevy::{
	prelude::*,
	tasks::{block_on, poll_once, IoTaskPool, Task},
};
use std::{future::Future, time::Duration};

pub(super) fn plugin(app: &mut App) {
	app.add_systems(Update, submit_handling.in_set(SubmissionSystems));
}

/// [`SystemSet`] where async submission logic runs
#[derive(SystemSet, Clone, Copy, PartialEq, Eq, Debug, Default, Hash)]
pub struct SubmissionSystems;

/// Spawn an entity with this component to start
/// sending data to the server.
///
/// The entity is despawned once completed
#[derive(Component)]
pub struct SubmissionTask {
	status: SubmissionStatus,
}

impl SubmissionTask {
	pub fn new(scope: LogSerializationScope) -> Self {
		Self {
			status: SubmissionStatus::New(scope),
		}
	}
	pub fn get_result(&self) -> Option<&Result<(), ureq::Error>> {
		match &self.status {
			SubmissionStatus::Completed(result) => Some(result),
			_ => None,
		}
	}
}

enum SubmissionStatus {
	New(LogSerializationScope),
	Sent(Task<Result<(), ureq::Error>>),
	Completed(Result<(), ureq::Error>),
}

fn submit_handling(
	mut query: Query<(Entity, &mut SubmissionTask)>,
	mut commands: Commands,
	playtest: Res<PlaytestLog>,
) {
	for (id, mut task) in &mut query {
		match &mut task.status {
			SubmissionStatus::New(scope) => {
				let task_pool = IoTaskPool::get();
				let new_task = task_pool.spawn(submit_playtest_log(&playtest, *scope));
				task.status = SubmissionStatus::Sent(new_task);
			}
			SubmissionStatus::Sent(future) => {
				let poll_result = block_on(poll_once(future));
				if let Some(result) = poll_result {
					match &result {
						Ok(()) => info!("Playtest log submitted successfully"),
						Err(err) => error!("While submitting playtest log: {err}"),
					}
					// Indicate the result on the entity,
					// despawn it in the next frame
					task.status = SubmissionStatus::Completed(result);
				}
			}
			SubmissionStatus::Completed(_) => {
				commands.entity(id).despawn();
			}
		}
	}
}

fn send_request_with_playtest_log(tester_id: u64, body: &str) -> Result<(), ureq::Error> {
	let config = ureq::config::Config::builder()
		.timeout_global(Some(Duration::from_secs(5)))
		.build();
	let agent = ureq::Agent::new_with_config(config);

	let uri = format!("https://lidajatus.com/epicycles/playtest/{tester_id}");

	// Try updating an existing entry first
	let result = agent.patch(&uri).send(body).map(|_| ());

	// If the entry does not exist, try resubmitting with post
	// Otherwise the response is final
	if !matches!(result, Err(ureq::Error::StatusCode(404))) {
		return result;
	}

	agent.post(&uri).send(body).map(|_| ())
}

fn submit_playtest_log(
	playtest: &PlaytestLog,
	scope: LogSerializationScope,
) -> impl Future<Output = Result<(), ureq::Error>> + Send + Sync {
	let mut body = serde_json::Map::new().into();
	playtest.write_json(&mut body, scope);
	let tester_id = playtest.tester_id();
	async move { send_request_with_playtest_log(tester_id, &body.to_string()) }
}
