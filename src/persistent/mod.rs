use std::{
	marker::PhantomData,
	path::{Path, PathBuf},
	time::{Duration, Instant},
};

use bevy::prelude::*;
use serde_json::Value as JsonValue;

mod io;
mod map_ext;

pub use map_ext::MapExt;

pub fn plugin(app: &mut App) {
	app.insert_resource(StoragePath::new());
}

/// System set where all persistent resources are fetched
#[derive(SystemSet, Clone, Copy, PartialEq, Eq, Debug, Default, Hash)]
pub struct LoadPersistentResourcesSystems;

/// Extension trait that allows registering of resources that are bound to persistent state
pub trait RegisterSaveableResource {
	/// Registers a resource that is bound to persistent state
	/// and a set of systems that synchronize the state of the resource
	fn register_saveable_resource<T: Saveable + Resource>(&mut self) -> &mut Self;
}

impl RegisterSaveableResource for App {
	fn register_saveable_resource<T: Saveable + Resource>(&mut self) -> &mut Self {
		self.init_resource::<T>()
			.init_resource::<JsonStore<T>>()
			.add_systems(
				Startup,
				load_system::<T>.in_set(LoadPersistentResourcesSystems),
			)
			.add_systems(Update, save_system::<T>.run_if(resource_changed::<T>))
			.add_systems(Update, write_system::<T>)
	}
}

fn load_system<T: Saveable + Resource>(
	mut store: ResMut<JsonStore<T>>,
	mut resource: ResMut<T>,
	storage_path: Res<StoragePath>,
) {
	let path = storage_path.0.join(format!("{}.json", T::FILENAME));
	match io::read(&path) {
		Ok(value) => {
			store.value = value;
			resource.bypass_change_detection().read_json(&store.value);
			log::info!("Loaded file {}", path.display());
		}
		Err(error) => {
			log::error!(
				"Failed to load {} with error {} trying to load newfile",
				path.display(),
				error
			);
			let path = storage_path.0.join(format!("{}.json.new", T::FILENAME));
			match io::read(&path) {
				Ok(value) => {
					store.value = value;
					resource.bypass_change_detection().read_json(&store.value);
					log::info!("Loaded file {}", path.display());
				}
				Err(error) => {
					log::error!("Failed to load {} with error {}", path.display(), error);
				}
			}
		}
	}
}

fn save_system<T: Saveable + Resource>(mut store: ResMut<JsonStore<T>>, resource: Res<T>) {
	resource.write_json(&mut store.value);
	store.dirty = true;
}

fn write_system<T: Saveable + Resource>(
	mut store: ResMut<JsonStore<T>>,
	storage_path: Res<StoragePath>,
) {
	if store.dirty
		&& store
			.last_saved
			.is_none_or(|last_saved| Instant::now() - last_saved > Duration::from_secs(10))
	{
		let data = store.value.to_string();
		io::write(&data, &storage_path.0, T::FILENAME);
		store.dirty = false;
		store.last_saved = Some(Instant::now());
	}
}

#[derive(Resource, Default)]
struct JsonStore<T> {
	/// The json data stored by this resource
	value: JsonValue,
	/// The last instant (if any) this resource was saved. Used for debouncing.
	last_saved: Option<Instant>,
	/// Whether [`Self::value`] has been changed since last save to disk.
	dirty: bool,
	_phantom: PhantomData<T>,
}

pub trait Saveable: Default {
	/// Name of the savefile for this object (without the .json extension)
	const FILENAME: &'static str;
	/// Write overrides this object needs to store to a Json representation
	/// Writes should ideally only be additive or replacement operations.
	fn write_json(&self, store: &mut JsonValue);
	/// Reads overrides from a Json representation and uses them to update itself
	fn read_json(&mut self, store: &JsonValue);
}

#[derive(Resource)]
struct StoragePath(pub PathBuf);

impl StoragePath {
	pub fn new() -> Self {
		StoragePath(
			directories::ProjectDirs::from("", "debug_corporation", "PtolemysEpicycles")
				.map(|project_dir| project_dir.data_dir().to_path_buf())
				.unwrap_or_else(|| {
					log::error!("COULD NOT RESOLVE DATA DIRECTORY, DEFAULTING TO LOCAL FOLDER");
					Path::new("./").to_path_buf()
				}),
		)
	}
}
