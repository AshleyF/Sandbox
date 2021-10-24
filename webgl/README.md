# WebGL

Working through [this WebGL tutorial series](https://www.youtube.com/playlist?list=PLjcVFFANLS5zH_PeKC6I8p0Pt1hzph_rt).

## [Setup and Triangle](https://www.youtube.com/playlist?list=PLjcVFFANLS5zH_PeKC6I8p0Pt1hzph_rt)

Essentially, taking these 15 floats and writing a vertex and fragment shader to render a colored triangle.

```javascript
var triangleVertices = new Float32Array([
        0.0,  0.5,  1.0, 1.0, 0.0,
    -0.5, -0.5,  0.7, 0.0, 1.0,
        0.5, -0.5,  0.1, 1.0, 0.6,
]);
```

```
precision mediump float;
attribute vec2 vertPosition;
attribute vec3 vertColor;
varying vec3 fragColor;
void main() {
  gl_Position = vec4(vertPosition, 0.0, 1.0);
  fragColor = vertColor;
}
```

```
precision mediump float;
varying vec3 fragColor;
void main() {
  gl_FragColor = vec4(fragColor, 1.0);
}
```