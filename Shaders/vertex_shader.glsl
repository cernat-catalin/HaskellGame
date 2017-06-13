attribute vec2 a_verts;

uniform mat3 u_transform;
uniform vec3 u_color;

varying vec3 f_color;

void main(void) {
  vec3 transformed_verts = u_transform * vec3(a_verts, 1.0);

  gl_Position = vec4(transformed_verts, 1.0);
  f_color = u_color;
}