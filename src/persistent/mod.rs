use std::{
	marker::PhantomData,
	path::{Path, PathBuf},
};

use bevy::prelude::*;
use serde_json::Value as JsonValue;

mod io;

pub fn plugin(app: &mut App) {
	app.insert_resource(StoragePath::new());
}

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
			.add_systems(Startup, load_system::<T>)
			.add_systems(Update, save_system::<T>.run_if(resource_changed::<T>))
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

fn save_system<T: Saveable + Resource>(
	mut store: ResMut<JsonStore<T>>,
	resource: Res<T>,
	storage_path: Res<StoragePath>,
) {
	resource.write_json(&mut store.value);
	let data = store.value.to_string();
	io::write(&data, &storage_path.0, T::FILENAME);
}

#[derive(Resource, Default)]
struct JsonStore<T> {
	value: JsonValue,
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
