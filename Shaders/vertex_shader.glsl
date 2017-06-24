attribute vec2 a_verts;

uniform mat3 u_transform;
uniform mat4 u_projection;
uniform vec3 u_color;
uniform int u_transparent;

varying vec4 f_color;

void main(void) {
  vec3 transformed_verts = u_transform * vec3(a_verts, 1.0);

  gl_Position = u_projection * vec4(transformed_verts, 1.0);

  if (u_transparent == 1) {
    f_color = vec4(u_color, 0.5);
  } else {
    f_color = vec4(u_color, 1);
  }
}