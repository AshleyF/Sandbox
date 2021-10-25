function init() {
    console.log("Loaded");

    function initWebGl(canvasName) {
        var canvas = document.getElementById(canvasName);
        var gl = canvas.getContext("webgl");
        if (!gl) {
            gl = canvas.getContext("experimental-context");
            console.log("WebGL not supported - falling back to experimental.")
        }

        gl.clearColor(0.0, 0.0, 0.0, 1.0);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        gl.enable(gl.DEPTH_TEST);
        gl.enable(gl.CULL_FACE);
        gl.frontFace(gl.CCW);
        gl.cullFace(gl.BACK);
        return gl;
    }

    function initShaders(vertShader, fragShader, gl) {
        var vertexShader = gl.createShader(gl.VERTEX_SHADER);
        var fragmentShader = gl.createShader(gl.FRAGMENT_SHADER);

        gl.shaderSource(vertexShader, vertShader);
        gl.shaderSource(fragmentShader, fragShader);

        gl.compileShader(vertexShader);
        if (!gl.getShaderParameter(vertexShader, gl.COMPILE_STATUS)) {
            console.error("ERROR compiling vertex shader", gl.getShaderInfoLog(vertexShader));
            return;
        }

        gl.compileShader(fragmentShader);
        if (!gl.getShaderParameter(fragmentShader, gl.COMPILE_STATUS)) {
            console.error("ERROR compiling fragment shader", gl.getShaderInfoLog(fragmentShader));
            return;
        }

        var program = gl.createProgram();
        gl.attachShader(program, vertexShader);
        gl.attachShader(program, fragmentShader);
        gl.linkProgram(program);
        if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
            console.error("ERROR linking program", gl.getProgramInfoLog(program));
            return;
        }
        gl.useProgram(program);

        gl.validateProgram(program); // TODO: debug-only
        if (!gl.getProgramParameter(program, gl.VALIDATE_STATUS)) {
            console.error("ERROR validating program", gl.getProgramInfoLog(program));
            return;
        }

        return program;
    }

    function loadBuffer(data, target, gl) {
        var buffer = gl.createBuffer();
        gl.bindBuffer(target, buffer);
        gl.bufferData(target, data, gl.STATIC_DRAW);
    }

    function loadArrayBuffer(vertices, gl) {
        loadBuffer(vertices, gl.ARRAY_BUFFER, gl);
    }

    function loadElementBuffer(elements, gl) {
        loadBuffer(elements, gl.ELEMENT_ARRAY_BUFFER, gl);
    }

    function enableAttribute(name, size, offset, cols, gl, program) {
        var loc = gl.getAttribLocation(program, name);
        gl.vertexAttribPointer(loc, size, gl.FLOAT, gl.FALSE, cols * Float32Array.BYTES_PER_ELEMENT, offset * Float32Array.BYTES_PER_ELEMENT);
        gl.enableVertexAttribArray(loc);
        return offset + size;
    }

    function enableUniformMatrix(name, matrix) {
        var loc = gl.getUniformLocation(program, name);
        gl.uniformMatrix4fv(loc, gl.FALSE, matrix);
    }

    var vertexShaderSource =
        'precision mediump float;' +
        'attribute vec3 vertPosition;' +
        'attribute vec3 vertColor;' +
        'varying vec3 fragColor;' +
        'uniform mat4 matWorld;' +
        'uniform mat4 matView;' +
        'uniform mat4 matProj;' +
        'void main() {' +
        '  gl_Position = matProj * matView * matWorld * vec4(vertPosition, 1.0);' +
        '  fragColor = vertColor;' +
        '}';

    var fragmentShaderSource =
        'precision mediump float;' +
        'varying vec3 fragColor;' +
        'void main() {' +
        '  gl_FragColor = vec4(fragColor, 1.0);' +
        '}';

    var gl = initWebGl("canvas");
    var program = initShaders(vertexShaderSource, fragmentShaderSource, gl);

    var boxVertices = new Float32Array([
		-1.0, 1.0, -1.0,   0.5, 0.5, 0.5, // Top
		-1.0, 1.0, 1.0,    0.5, 0.5, 0.5,
		1.0, 1.0, 1.0,     0.5, 0.5, 0.5,
		1.0, 1.0, -1.0,    0.5, 0.5, 0.5,
		-1.0, 1.0, 1.0,    0.75, 0.25, 0.5, // Left
		-1.0, -1.0, 1.0,   0.75, 0.25, 0.5,
		-1.0, -1.0, -1.0,  0.75, 0.25, 0.5,
		-1.0, 1.0, -1.0,   0.75, 0.25, 0.5,
		1.0, 1.0, 1.0,    0.25, 0.25, 0.75, // Right
		1.0, -1.0, 1.0,   0.25, 0.25, 0.75,
		1.0, -1.0, -1.0,  0.25, 0.25, 0.75,
		1.0, 1.0, -1.0,   0.25, 0.25, 0.75,
		1.0, 1.0, 1.0,    1.0, 0.0, 0.15, // Front
		1.0, -1.0, 1.0,    1.0, 0.0, 0.15,
		-1.0, -1.0, 1.0,    1.0, 0.0, 0.15,
		-1.0, 1.0, 1.0,    1.0, 0.0, 0.15,
		1.0, 1.0, -1.0,    0.0, 1.0, 0.15, // Back
		1.0, -1.0, -1.0,    0.0, 1.0, 0.15,
		-1.0, -1.0, -1.0,    0.0, 1.0, 0.15,
		-1.0, 1.0, -1.0,    0.0, 1.0, 0.15,
		-1.0, -1.0, -1.0,   0.5, 0.5, 1.0, // Bottom
		-1.0, -1.0, 1.0,    0.5, 0.5, 1.0,
		1.0, -1.0, 1.0,     0.5, 0.5, 1.0,
		1.0, -1.0, -1.0,    0.5, 0.5, 1.0,
	]);

	var boxIndices = new Uint16Array([
		0, 1, 2, // Top
		0, 2, 3,
		5, 4, 6, // Left
		6, 4, 7,
		8, 9, 10, // Right
		8, 10, 11,
		13, 12, 14, // Front
		15, 14, 12,
		16, 17, 18, // Back
		16, 18, 19,
		21, 20, 22, // Bottom
		22, 20, 23
	]);

    loadArrayBuffer(boxVertices, gl);
    loadElementBuffer(boxIndices, gl);

    const cols = 6;
    var offset = 0;
    offset = enableAttribute("vertPosition", 3, offset, cols, gl, program);
    offset = enableAttribute("vertColor", 3, offset, cols, gl, program);

	var worldMatrix = new Float32Array(16);
	var viewMatrix = new Float32Array(16);
    var projMatrix = new Float32Array(16);

    glMatrix.mat4.identity(worldMatrix);
    glMatrix.mat4.lookAt(viewMatrix, [0, 0, -5], [0, 0, 0], [0, 1, 0]);
	glMatrix.mat4.perspective(projMatrix, Math.PI / 4, 800 / 600, 0.1, 1000.0);

    enableUniformMatrix("matWorld", worldMatrix);
    enableUniformMatrix("matView", viewMatrix);
    enableUniformMatrix("matProj", projMatrix);

    var xRotationMatrix = new Float32Array(16);
    var yRotationMatrix = new Float32Array(16);

    var identityMatrix = new Float32Array(16);
    glMatrix.mat4.identity(identityMatrix);
    var loop = function() {
        var angle = performance.now() / 1000 / 6 * 2 * Math.PI;
        glMatrix.mat4.rotate(xRotationMatrix, identityMatrix, angle, [0, 1, 0]);
        glMatrix.mat4.rotate(yRotationMatrix, identityMatrix, angle / 4, [1, 0, 0]);
        glMatrix.mat4.mul(worldMatrix, yRotationMatrix, xRotationMatrix);
        enableUniformMatrix("matWorld", worldMatrix);

        gl.clearColor(0.0, 0.0, 0.0, 1.0);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        // gl.drawArrays(gl.TRIANGLES, 0, triangleVertices.length / cols);
        gl.drawElements(gl.TRIANGLES, boxIndices.length, gl.UNSIGNED_SHORT, 0);

        requestAnimationFrame(loop);
    }
    requestAnimationFrame(loop);
}