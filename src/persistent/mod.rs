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

pub fn register_saveable_resource<T: Saveable + Resource>(app: &mut App) {
	app.init_resource::<T>()
		.init_resource::<JsonStore<T>>()
		.add_systems(Startup, load_system::<T>)
		.add_systems(Update, save_system::<T>.run_if(resource_changed::<T>));
}

fn load_system<T: Saveable + Resource>(
	mut store: ResMut<JsonStore<T>>,
	mut resource: ResMut<T>,
	storage_path: Res<StoragePath>,
) {
	let path = T::get_path(storage_path.0.clone());
	match io::read(&path) {
		Ok(value) => {
			store.value = value;
			resource.read_json(&store.value);
		}
		Err(error) => {
			log::error!("Failed to load {} with error {}", path.display(), error);
		}
	}
}

fn save_system<T: Saveable + Resource>(
	mut store: ResMut<JsonStore<T>>,
	resource: Res<T>,
	storage_path: Res<StoragePath>,
) {
	let path = T::get_path(storage_path.0.clone());
	resource.write_json(&mut store.value);
	if let Err(error) = io::write(&store.value, &path) {
		log::error!("Failed to load {} with error {}", path.display(), error);
	}
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
	/// Gets the save file path from a base path
	fn get_path(mut path: PathBuf) -> PathBuf {
		path.set_file_name(Self::FILENAME);
		path.set_extension("json");
		path
	}
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
