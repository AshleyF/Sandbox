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

    function loadBuffer(vertices, gl) {
        var buffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
        gl.bufferData(gl.ARRAY_BUFFER, vertices, gl.STATIC_DRAW);
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

    var triangleVertices = new Float32Array([
         0.0,  0.5, 0.0,  1.0, 1.0, 0.0,
        -0.5, -0.5, 0.0,  0.7, 0.0, 1.0,
         0.5, -0.5, 0.0,  0.1, 1.0, 0.6,
    ]);

    loadBuffer(triangleVertices, gl);

    const cols = 6;
    var offset = 0;
    offset = enableAttribute("vertPosition", 3, offset, cols, gl, program);
    offset = enableAttribute("vertColor", 3, offset, cols, gl, program);

	var worldMatrix = new Float32Array(16);
	var viewMatrix = new Float32Array(16);
    var projMatrix = new Float32Array(16);

    glMatrix.mat4.identity(worldMatrix);
    glMatrix.mat4.lookAt(viewMatrix, [0, 0, -2], [0, 0, 0], [0, 1, 0]);
	glMatrix.mat4.perspective(projMatrix, Math.PI / 4, 800 / 600, 0.1, 1000.0);

    enableUniformMatrix("matWorld", worldMatrix);
    enableUniformMatrix("matView", viewMatrix);
    enableUniformMatrix("matProj", projMatrix);

    var identityMatrix = new Float32Array(16);
    glMatrix.mat4.identity(identityMatrix);

    var loop = function() {
        var angle = performance.now() / 1000 / 6 * 2 * Math.PI;
        glMatrix.mat4.rotate(worldMatrix, identityMatrix, angle, [0, 1, 0]);
        enableUniformMatrix("matWorld", worldMatrix);

        gl.clearColor(0.0, 0.0, 0.0, 1.0);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        gl.drawArrays(gl.TRIANGLES, 0, triangleVertices.length / cols);

        requestAnimationFrame(loop);
    }
    requestAnimationFrame(loop);
}