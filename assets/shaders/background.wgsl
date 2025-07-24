#import bevy_render::globals::Globals;
#import bevy_sprite::mesh2d_vertex_output::VertexOutput;

@group(0) @binding(1) var<uniform> globals: Globals;
@group(2) @binding(0) var<uniform> material_scale: vec2f;
@group(2) @binding(1) var<uniform> material_speed: vec2f;
@group(2) @binding(2) var material_texture: texture_2d<f32>;
@group(2) @binding(3) var material_sampler: sampler;
@group(2) @binding(4) var<uniform> colors: array<vec4f, 4>;
@group(2) @binding(5) var<uniform> sweep_origin: vec2f;
@group(2) @binding(6) var<uniform> sweep_direction: vec2f;
@group(2) @binding(7) var<uniform> nominal_sweep_start: f32;
@group(2) @binding(8) var<uniform> nominal_sweep_width: f32;

// https://github.com/bevyengine/bevy/discussions/8937
// Bevy seems to convert our textures to lrgba without asking
// TODO: Fix this in a way that actually fixes it
fn from_linear(linear: vec4f) -> vec4f {
    let cutoff = step(linear, vec4f(0.0031308));
    let higher = vec4f(1.055) * pow(linear, vec4(1.0 / 2.4)) - vec4(0.055);
    let lower = linear * vec4f(12.92);
    return mix(higher, lower, cutoff);
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4f {
    // Texture coordinates, scaled for repeating texture
    let scaled_uv: vec2f = in.uv * material_scale;
    // Actual texture coordinates for sampling the material texture
	let uv: vec2f = fract(scaled_uv - globals.time * material_speed);
    // Near corner of the current tile (tile = region that gets painted with one instance
    // of the material texture) in scaled global texture coordinates
    let tile_near_point: vec2f = scaled_uv - uv + sweep_origin / material_scale;
    // Sample of the material texture that corresponds to the current fragment
	let material_sample: vec4f = textureSample(material_texture, material_sampler, uv);
    // Sample of the material texture in srgba space
    let corrected_material_sample: vec4f = from_linear(material_sample);
    // Red channel of material texture indicates which of two colors should be used
    let color_picker: f32 = corrected_material_sample.x;
    // Green channel of material texture indicates how far behind the sweep boundry
    // the current fragment's color transition should lag
    let sweep_lag: f32 = corrected_material_sample.y * sqrt(2.0);
    // Blue channel of the material texture indicates how long the color transition should take
    let sweep_duration: f32 = corrected_material_sample.z;
    let sweep_projection: f32 = dot(tile_near_point - sweep_origin, sweep_direction);
    let nominal_sweep_point: f32 = (nominal_sweep_start - sweep_projection) / nominal_sweep_width;
    let fragment_sweep_point: f32 = (nominal_sweep_point - sweep_lag) / sweep_duration;
    let palette_picker: f32 = clamp(fragment_sweep_point, 0.0, 1.0);
    let primary_color: vec4f = mix(colors[0], colors[1], color_picker);
    let secondary_color: vec4f = mix(colors[2], colors[3], color_picker);
    return mix(primary_color, secondary_color, palette_picker);
}
