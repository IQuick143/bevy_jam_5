#import bevy_render::globals::Globals;
#import bevy_sprite::mesh2d_vertex_output::VertexOutput;

@group(0) @binding(1) var<uniform> globals: Globals;
@group(2) @binding(0) var<uniform> material_scale: vec2f;
@group(2) @binding(1) var<uniform> material_speed: vec2f;
@group(2) @binding(2) var material_texture: texture_2d<f32>;
@group(2) @binding(3) var material_sampler: sampler;

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4f {
	var uv: vec2f = fract(in.uv * material_scale - globals.time * material_speed);
	return textureSample(material_texture, material_sampler, uv);
}
