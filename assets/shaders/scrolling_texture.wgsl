#import bevy_render::globals::Globals;
#import bevy_sprite::mesh2d_vertex_output::VertexOutput;

@group(0) @binding(1) var<uniform> globals: Globals;
@group(2) @binding(0) var<uniform> material_scale: f32;
@group(2) @binding(1) var<uniform> material_speed: vec2f;
@group(2) @binding(2) var material_texture: texture_2d<f32>;
@group(2) @binding(3) var material_sampler: sampler;

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4f {
	var offset: vec2f = (globals.time * material_speed) % 1;
	var uv: vec2f = (in.uv * material_scale - offset) % 1;
	return textureSample(material_texture, material_sampler, uv);
}
