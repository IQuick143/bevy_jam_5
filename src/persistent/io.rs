use std::fs::OpenOptions;
use std::io;
use std::path::Path;

use serde_json::Value as JsonValue;

/// Writes the json data to a path
pub fn write(data: &JsonValue, path: &Path) -> Result<(), io::Error> {
	if let Some(parent) = path.parent() {
		std::fs::create_dir_all(parent)?
	}
	let file = OpenOptions::new()
		.write(true)
		.create(true)
		.truncate(true)
		.open(path)?;
	serde_json::to_writer(file, data)?;
	Ok(())
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
