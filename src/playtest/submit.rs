//! Handles sending playtest logs to the server

use super::log::{LogSerializationScope, PlaytestLog};
use bevy::{prelude::*, tasks::IoTaskPool};
use futures::channel::oneshot::{channel, Receiver, Sender};
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
	Sent(Receiver<Result<(), ureq::Error>>),
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
				let (promise, future) = channel();
				let task_pool = IoTaskPool::get();
				task_pool
					.spawn(submit_playtest_log(&playtest, *scope, promise))
					.detach();
				task.status = SubmissionStatus::Sent(future);
			}
			SubmissionStatus::Sent(future) => {
				let poll_result = future.try_recv().unwrap_or_else(|_| {
					// Error here means the task was dropped before it could finish
					Some(Err(ureq::Error::Io(std::io::ErrorKind::Interrupted.into())))
				});
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
	promise: Sender<Result<(), ureq::Error>>,
) -> impl Future<Output = ()> + Send + Sync {
	let mut body = serde_json::Map::new().into();
	playtest.write_json(&mut body, scope);
	let tester_id = playtest.tester_id();
	async move {
		let result = send_request_with_playtest_log(tester_id, &body.to_string());
		// If this operation fails, somebody must have shut down the ECS,
		// so we can leave it without response
		let _ = promise.send(result);
	}
}
