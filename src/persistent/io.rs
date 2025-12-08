use std::fs::OpenOptions;
use std::io;
use std::path::Path;

use bevy::tasks::IoTaskPool;
use serde_json::Value as JsonValue;

/// Writes the json data to a path
pub fn write(data: &str, base_path: &Path, filename: &str) {
	// Code adapted from `bevy_prefs_lite` (Thanks Talin et al.!)
	IoTaskPool::get().scope(|scope| {
		scope.spawn(async {
			// Recursively create the directory if it doesn't exist.
			let mut dir_builder = std::fs::DirBuilder::new();
			dir_builder.recursive(true);
			if let Err(e) = dir_builder.create(base_path) {
				log::warn!("Could not create directory: {:?}", e);
				return;
			}
			// Save to temp file
			let temp_path = base_path.join(format!("{}.json.new", filename));
			if let Err(e) = std::fs::write(&temp_path, data) {
				log::error!("Error saving file: {}", e);
			}
			// Replace old prefs file with new one.
			let file_path = base_path.join(format!("{}.json", filename));
			if let Err(e) = std::fs::rename(&temp_path, &file_path) {
				log::warn!("Could not save file: {:?}", e);
			} else {
				log::info!("Saved file {}", file_path.display());
			}
		});
	});
}

/// Read a json file
pub fn read(path: &Path) -> Result<JsonValue, io::Error> {
	let file = OpenOptions::new()
		.read(true)
		.append(true)
		.create(true)
		.open(path)?;
	Ok(serde_json::from_reader(file)?)
}
