#import bevy_render::globals::Globals;
#import bevy_sprite::mesh2d_vertex_output::VertexOutput;

struct ShaderParams {
    material_scale: vec2f,
    material_speed: vec2f,
    colors: array<vec4f, 4>,
    sweep_origin: vec2f,
    sweep_direction: vec2f,
    nominal_sweep_start: f32,
    nominal_sweep_width: f32,
}

@group(0) @binding(1) var<uniform> globals: Globals;
@group(2) @binding(0) var material_texture: texture_2d<f32>;
@group(2) @binding(1) var material_sampler: sampler;
@group(2) @binding(2) var<uniform> params: ShaderParams;

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4f {
    // Texture coordinates, scaled for repeating texture
    let scaled_uv: vec2f = in.uv * params.material_scale;
    // Actual texture coordinates for sampling the material texture
	let uv: vec2f = fract(scaled_uv - globals.time * params.material_speed);
    // Near corner of the current tile (tile = region that gets painted with one instance
    // of the material texture) in scaled global texture coordinates
    let tile_near_point: vec2f = scaled_uv - uv + params.sweep_origin / params.material_scale;
    // Sample of the material texture that corresponds to the current fragment
	let material_sample: vec4f = textureSample(material_texture, material_sampler, uv);
    // Red channel of material texture indicates which of two colors should be used
    let color_picker: f32 = material_sample.x;
    // Green channel of material texture indicates how far behind the sweep boundry
    // the current fragment's color transition should lag
    let sweep_lag: f32 = material_sample.y * sqrt(2.0);
    // Blue channel of the material texture indicates how long the color transition should take
    let sweep_duration: f32 = material_sample.z;
    let sweep_projection: f32 = dot(tile_near_point - params.sweep_origin, params.sweep_direction);
    let nominal_sweep_point: f32 = (params.nominal_sweep_start - sweep_projection) / params.nominal_sweep_width;
    let fragment_sweep_point: f32 = (nominal_sweep_point - sweep_lag) / sweep_duration;
    let palette_picker: f32 = clamp(fragment_sweep_point, 0.0, 1.0);
    let primary_color: vec4f = mix(params.colors[0], params.colors[1], color_picker);
    let secondary_color: vec4f = mix(params.colors[2], params.colors[3], color_picker);
    return mix(primary_color, secondary_color, palette_picker);
}
