document.addEventListener('DOMContentLoaded', async () => {
    const canvas = document.getElementById('glCanvas');
    const audio = document.getElementById('myAudio');
    const gl = canvas.getContext('webgl');
    const queryString = window.location.search;
    const urlParams = new URLSearchParams(queryString);
    const scale = urlParams.get('scale');
    const timestamp = urlParams.get('time');

    let startTime = Date.now();
    let demoStarted = false;
    let engineLoaded = false;
    const demoDOM =  document.getElementsByTagName('demo')[0];
    if (!gl) {
        alert('Unable to initialize WebGL. Your browser may not support it.');
        return;
    }

    async function fetchShaderSource(path) {
        const response = await fetch(path);
        if (!response.ok) {
            throw new Error(`Could not load shader at ${path}: ${response.status}`);
        }
        return await response.text();
    }

    const vertexShaderSource = await fetchShaderSource('vertex_shader.glsl');
    const fragmentShaderSource = await fetchShaderSource('fragment_shader.glsl');
    const shaderProgram = initShaderProgram(gl, vertexShaderSource, fragmentShaderSource);
    const programInfo = {
        program: shaderProgram,
        attribLocations: {
            vertexPosition: gl.getAttribLocation(shaderProgram, 'aVertexPosition'),
        },
    };

    const iResolutionLocation = gl.getUniformLocation(shaderProgram, 'iResolution');
    const buffers = initBuffers(gl);
    function initBuffers(gl) {
        const positionBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
        const positions = [
             1.0,  1.0,
            -1.0,  1.0,
             1.0, -1.0,
            -1.0, -1.0,
        ];
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);
        return {
            position: positionBuffer,
        };
    }

    function drawScene(gl, programInfo, buffers) {
        gl.clearColor(0.0, 0.0, 0.0, 1.0);
        gl.clearDepth(1.0);
        gl.enable(gl.DEPTH_TEST);
        gl.depthFunc(gl.LEQUAL);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        {
            const numComponents = 2;
            const type = gl.FLOAT;
            const normalize = false;
            const stride = 0;
            const offset = 0;
            gl.bindBuffer(gl.ARRAY_BUFFER, buffers.position);
            gl.vertexAttribPointer(
                programInfo.attribLocations.vertexPosition,
                numComponents,
                type,
                normalize,
                stride,
                offset);
            gl.enableVertexAttribArray(
                programInfo.attribLocations.vertexPosition);
        }
        gl.useProgram(programInfo.program);
        {
            const offset = 0;
            const vertexCount = 4;
            gl.drawArrays(gl.TRIANGLE_STRIP, offset, vertexCount);
        }
    }

    function initShaderProgram(gl, vsSource, fsSource) {
        const vertexShader = loadShader(gl, gl.VERTEX_SHADER, vsSource);
        const fragmentShader = loadShader(gl, gl.FRAGMENT_SHADER, fsSource);
        const shaderProgram = gl.createProgram();
        gl.attachShader(shaderProgram, vertexShader);
        gl.attachShader(shaderProgram, fragmentShader);
        gl.linkProgram(shaderProgram);

        if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS)) {
            alert('Unable to initialize the shader program: ' + gl.getProgramInfoLog(shaderProgram));
            return null;
        }
        return shaderProgram;
    }

    function loadShader(gl, type, source) {
        const shader = gl.createShader(type);
        gl.shaderSource(shader, source);
        gl.compileShader(shader);
        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
            alert('An error occurred compiling the shaders: ' + gl.getShaderInfoLog(shader));
            gl.deleteShader(shader);
            return null;
        }
        return shader;
    }

    function resizeCanvasToDisplaySize(canvas, multiplier = 1) {
        if (scale !== null && !isNaN(scale)) { multiplier = scale; }
        const width  = window.innerWidth * multiplier;
        const height = window.innerHeight * multiplier;


        if (canvas.width !== width || canvas.height !== height) {
            canvas.width  = width;
            canvas.height = height;
            const gl = canvas.getContext('webgl') || canvas.getContext('experimental-webgl');
            gl.viewport(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight);
            gl.useProgram(shaderProgram);
            gl.uniform2f(iResolutionLocation, gl.drawingBufferWidth, gl.drawingBufferHeight);
            return true;
        }

        return false;
    }

    window.addEventListener('resize', () => resizeCanvasToDisplaySize(document.getElementById('glCanvas')));
    resizeCanvasToDisplaySize(document.getElementById('glCanvas'));

    canvas.addEventListener('click', function() {
        if(engineLoaded && !demoStarted){
            demoDOM.classList.remove('wait');
            startTime = Date.now();
            demoStarted = true;
            audio.play().catch(e => console.error("Failed to play audio:", e));
        }
    });

    function render() {
        let currentTime,elapsedTime, iTimeLocation;
        if (demoStarted){
            currentTime = Date.now();
            elapsedTime = (currentTime - startTime) / 1000.0;
            iTimeLocation = gl.getUniformLocation(shaderProgram, "iTime");
            gl.uniform1f(iTimeLocation, elapsedTime + Number(timestamp));
        }else {
            engineLoaded = true;
            demoDOM.style.setProperty('--loading-text', '"🖱️ ▶ 🖥️"');
        }
        drawScene(gl, programInfo, buffers);
        requestAnimationFrame(render);
    }

    requestAnimationFrame(render);
});


