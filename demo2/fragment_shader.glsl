precision mediump float;
uniform vec2 iResolution;
uniform float iTime;

const int NUMBER_OF_STEPS = 300;
const float MINIMUM_HIT_DISTANCE = 0.0001;
const float MAXIMUM_TRACE_DISTANCE = 100.0;

#define PI 3.141592

float sdSphere( vec3 p, float s )
{
    return length(p)-s;
}

float sdBox( vec3 p, vec3 b )
{
    vec3 q = abs(p) - b;
    return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}

float sdRoundBox( vec3 p, vec3 b, float r )
{
    vec3 q = abs(p) - b;
    return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0) - r;
}


float sdCylinder(vec3 p, float r, float height) {
    float d = length(p.xz) - r;
    d = max(d, abs(p.y) - height);
    return d;
}

// Hexagonal prism, circumcircle variant
float sdHexagonCircumcircle(vec3 p, vec2 h) {
    vec3 q = abs(p);
    return max(q.y - h.y, max(q.x*sqrt(3.)*0.5 + q.z*0.5, q.z) - h.x);
}

float sdCapsule(vec3 p, float r, float c) {
    return mix(length(p.xz) - r, length(vec3(p.x, abs(p.y) - c, p.z)) - r, step(c, abs(p.y)));
}

float opUnion( float d1, float d2 ) { return min(d1,d2); }

float opSubtraction( float d1, float d2 ) { return max(-d1,d2); }

float opIntersection( float d1, float d2 ) { return max(d1,d2); }

float opSmoothUnion( float d1, float d2, float k ) {
    float h = clamp( 0.5 + 0.5*(d2-d1)/k, 0.0, 1.0 );
    return mix( d2, d1, h ) - k*h*(1.0-h); }

float opSmoothSubtraction( float d1, float d2, float k ) {
    float h = clamp( 0.5 - 0.5*(d2+d1)/k, 0.0, 1.0 );
    return mix( d2, -d1, h ) + k*h*(1.0-h); }

float opSmoothIntersection( float d1, float d2, float k ) {
    float h = clamp( 0.5 - 0.5*(d2-d1)/k, 0.0, 1.0 );
    return mix( d2, d1, h ) + k*h*(1.0-h); }


vec2 hash( vec2 p ) // replace this by something better
{
    p = vec2( dot(p,vec2(127.1,311.7)), dot(p,vec2(269.5,183.3)) );
    return -1.0 + 2.0*fract(sin(p)*43758.5453123);
}

vec3 hash3( vec2 p )
{
    vec3 q = vec3( dot(p,vec2(127.1,311.7)),
                   dot(p,vec2(269.5,183.3)),
                   dot(p,vec2(419.2,371.9)) );
    return fract(sin(q)*43758.5453);
}

float voronoise( in vec2 p, float u, float v )
{
    float k = 1.0+63.0*pow(1.0-v,6.0);

    vec2 i = floor(p);
    vec2 f = fract(p);

    vec2 a = vec2(0.0,0.0);
    for( int y=-2; y<=2; y++ )
        for( int x=-2; x<=2; x++ )
        {
            vec2  g = vec2( x, y );
            vec3  o = hash3( i + g )*vec3(u,u,1.0);
            vec2  d = g - f + o.xy;
            float w = pow( 1.0-smoothstep(0.0,1.414,length(d)), k );
            a += vec2(o.z*w,w);
        }

        return a.x/a.y;
}

float noise( in vec2 p )
{
    const float K1 = 0.366025404; // (sqrt(3)-1)/2;
    const float K2 = 0.211324865; // (3-sqrt(3))/6;

    vec2  i = floor( p + (p.x+p.y)*K1 );
    vec2  a = p - i + (i.x+i.y)*K2;
    float m = step(a.y,a.x);
    vec2  o = vec2(m,1.0-m);
    vec2  b = a - o + K2;
    vec2  c = a - 1.0 + 2.0*K2;
    vec3  h = max( 0.5-vec3(dot(a,a), dot(b,b), dot(c,c) ), 0.0 );
    vec3  n = h*h*h*h*vec3( dot(a,hash(i+0.0)), dot(b,hash(i+o)), dot(c,hash(i+1.0)));
    return dot( n, vec3(70.0) );
}

void opRot(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float opMod1(inout float p, float size) {
    float halfsize = size*0.5;
    float c = floor((p + halfsize)/size);
    p = mod(p + halfsize, size) - halfsize;
    return c;
}

vec2 opMod2(inout vec2 p, vec2 size) {
    vec2 c = floor((p + size*0.5)/size);
    p = mod(p + size*0.5,size) - size*0.5;
    return c;
}

vec3 opMod3(inout vec3 p, vec3 size) {
    vec3 c = floor((p + size*0.5)/size);
    p = mod(p + size*0.5, size) - size*0.5;
    return c;
}

float opModInterval1(inout float p, float size, float start, float stop) {
    float halfsize = size*0.5;
    float c = floor((p + halfsize)/size);
    p = mod(p+halfsize, size) - halfsize;
    if (c > stop) { //yes, this might not be the best thing numerically.
        p += size*(c - stop);
        c = stop;
    }
    if (c <start) {
        p += size*(c - start);
        c = start;
    }
    return c;
}

float sgn(float x) {
     if(x<0.){ return -1.;} else {return 1.;};
}

float opMirror (inout float p, float dist) {
    float s = sgn(p);
    p = abs(p)-dist;
    return s;
}


/*
 * SDF WORLD
 *
 */


vec2 sdWorld(in vec3 p)
{
    float c = 0.0;
    float waves = 0.5 + 0.5*sin((noise(p.xz*0.005)*16.66));
    float sand = p.y + 0.05*noise(p.xz*0.7)+2.0*noise(p.xz*0.06*waves);

    // WORM
    vec3 worm_pos = p-vec3(0.0,2.8,-1.5);
    float worm = opSmoothSubtraction(
                sdSphere(worm_pos-vec3(0.0,0.0,1.4),2.5),
                sdSphere(worm_pos,2.8),0.25);
    vec3 worm_chunk_pos = vec3(0.);
    for (float i=0.0; i<1.2; i+=0.1){
        worm_chunk_pos = worm_pos+vec3(0., -0.2 + cos(2.+i*5.)*1.5+i*2.0, 2.8 + i*10.);
        worm = opSmoothUnion(worm,
            sdCapsule(worm_chunk_pos, 2.6, 0.1),
            0.05);
        worm = opSmoothUnion(worm,
                sdSphere(worm_chunk_pos-vec3(2.1,1.75,0.),  0.05),
                0.25);
        worm = opSmoothUnion(worm,
                             sdSphere(worm_chunk_pos-vec3(-2.1,1.75,0.),  0.05),
                             0.25);
    }

    worm = opSmoothSubtraction(
        sdSphere(worm_pos+vec3(0.,0.0,1.0),1.6),
        worm,0.5);

    //worm = worm + (sin(noise(p.xz*16.)*4.) + sin(noise(p.xy*16.)*4.))*0.001;
    float skin_dist = 20.0;
    float skin_dist2 = 4.0;
    float skin = (noise(p.xy*skin_dist) + noise(p.xz*skin_dist));
    float skin2 = (noise(sin(p.xz)*skin_dist2) + noise(cos(p.yz)*skin_dist2));
    worm = worm + skin*skin*0.002+skin2*0.01;

    float rad = 2.5;
    for (float i=0.0; i<19.0; i+=1.0){
        worm = opSmoothSubtraction(
            sdSphere(worm_pos+vec3(cos(i)*rad,sin(i)*rad,-0.8),0.05),
                    worm, 0.5);
    }

    if (worm<=MINIMUM_HIT_DISTANCE){
        c=1.0;
    }

    // WORM 'EYE'

    rad = 1.0;
    vec3 pp = worm_pos-vec3(.0,.0,-0.8);
    float eye=1.0;
    for (float i=0.0; i<16.0; i+=1.0){
        opRot(pp.xy,0.2);
        eye = opSmoothUnion(
            sdCapsule(pp, 0.04, 1.8),
            eye, 0.1);
    }
    vec3 er = pp;
    opRot(er.yz,PI*.5);
    eye = opSmoothSubtraction(
        sdCylinder(er,0.1,0.2),
        eye, 0.7);


    worm = opUnion(worm,eye);

    if (eye<=MINIMUM_HIT_DISTANCE){
        c=4.0;
    }

    // HERO
    vec3 hero_pos = p+vec3(0.0,-0.18,-2.0);
    float hero = opSmoothUnion(sdSphere(hero_pos,0.02),
                               sdSphere(hero_pos+vec3(0.0,-0.065,0.0),0.01),
                                0.085);

    if (hero<=MINIMUM_HIT_DISTANCE){
        c=2.0;
    }


    // WALL


    vec3 base_wall_pos = p-vec3(-16.,-0.4,1.0);
    opMod1(base_wall_pos.z,5.);
    opMirror(base_wall_pos.x,4.5);

    vec3 prot = base_wall_pos;
    opRot(prot.xy,.5);

    float wall_base = opSubtraction(
        sdBox(prot-vec3(1.5,4.,0.0),vec3(2.0,3.,2.6)),
        sdBox(base_wall_pos-vec3(-2.5,-0.5,0.0),vec3(2.0,5.,2.)));
    wall_base = opSubtraction(sdBox(prot-vec3(0,3.,0.0),vec3(1.0,1.,1.0)),wall_base);
    wall_base = opSubtraction(sdBox(base_wall_pos-vec3(-2.5,4.,0.0),vec3(0.5,0.05,1.8)),wall_base);
    float joinWall = sdBox(base_wall_pos-vec3(-3.0,-0.5,0.),vec3(1.0,4.,2.45));
    float wall_outer = sdRoundBox(base_wall_pos-vec3(.0,-0.5,0.),vec3(0.1,0.8,2.3),0.05);


    //  BASE

    vec3 base_org = p-vec3(-40.,0.0,0.0);
    vec3 base_pos = base_org;

    float building = 1.0;

    opMirror(base_pos.z,8.0);
    opMirror(base_pos.x,8.0);

    building = opUnion(sdBox(base_pos,vec3(6.0,8.,6.)),building);
    building = opUnion(sdBox(base_pos,vec3(8.0,1.,8.)), building);

    prot = base_pos;
    opRot(prot.xy,.5);

    building = opSubtraction( sdBox(prot-vec3(8.,2.5,0.),vec3(2.0,4.,6.2)), building);



    vec3 mul = prot;
    vec3 mul2 = base_pos;
    opModInterval1(mul.z,0.4,-14.0,14.0);
    opModInterval1(mul2.z,0.6,-4.0,4.0);
    building = opSubtraction(sdBox(mul-vec3(6.,2.0,0.),vec3(0.8,3.5,0.08)),building);

    float pillars = sdRoundBox(base_pos-vec3(2.,3.0,0.),vec3(1.,8.,3.),.05);
    pillars = opSubtraction(sdBox(mul2-vec3(3.5,8.0,0.),vec3(0.8,2.0,0.24)),pillars);
    pillars = opSubtraction(sdBox(base_pos-vec3(3.5,10.5,0.),vec3(0.8,0.05,2.8)),pillars);
    building = opUnion(pillars,building);

    building = opUnion(sdBox(base_pos-vec3(1.0,8.,0.),vec3(1.,0.5,4.0)),building);

    vec3 prot2 = base_pos;
    opRot(prot2.xy,-.4);
    building = opSubtraction(sdBox(prot2-vec3(-4.,10.,0.),vec3(1.5,1.5,4.0)),building);

    opRot(prot.xy,.5);
    building = opSubtraction(sdBox(prot-vec3(4.,8.0,2.),vec3(6.0,2.,6.0)), building);

    building = opSubtraction(sdBox(base_org-vec3(0.,10.,0.),vec3(8.,6.,8.)),building);
    building = opUnion(sdBox(base_org-vec3(0.,4.,0.),vec3(6.,4.,6.)),building);
    building = opSubtraction(sdBox(base_org-vec3(0.,8.,0.),vec3(5.5,0.5,5.5)),building);


    // ENTRANCE

    vec3 entr_pos = base_org -vec3(-12.0,0.0,.0);
    building = opUnion(sdBox(entr_pos-vec3(0.,2.,0.),vec3(3.,1.,4.)),building);
    building = opUnion(sdBox(entr_pos-vec3(0.,3.,0.),vec3(1.,1.,1.8)),building);

    mul = entr_pos-vec3(0.,5.,0.);
    opModInterval1(mul.z,0.25,-6.0,6.0);
    building = opUnion(sdBox(mul,vec3(0.25,1.5,0.05)),building);


    building = opUnion(sdBox(entr_pos-vec3(0.,6.,0.),vec3(1.,0.5,1.8)),building);

    building = opSubtraction(sdBox(entr_pos-vec3(0.,2.2,0.),vec3(8.,0.4,1.4)),building);
    building = opUnion(sdBox(entr_pos-vec3(-10.,0.,0.),vec3(12.,2.,1.)),building);
    building = opSubtraction(sdBox(entr_pos-vec3(-10.,2.0,0.),vec3(12.,0.15,0.9)),building);


    float base = opUnion(opUnion(wall_base, joinWall), wall_outer);

    // smaller buildings
    vec3 b_pos = entr_pos-vec3(-6.0,2.0, 0.);

    opModInterval1(b_pos.x,5.0,-3.0,0.0);
    opMirror(b_pos.z,5.0);
    opMirror(b_pos.z,1.5);

    float body = sdBox(b_pos, vec3(1.0, 2.0, 1.0));
    body = opSubtraction(sdBox(b_pos - vec3(0.0, -1.5, 1.01), vec3(0.2, 0.5, 0.1)),body);
    body = opUnion(sdBox(b_pos - vec3(0.0, 2.+noise(b_pos.xz*0.1+iTime), 0.), vec3(1.2, 0.15, 1.2)),body); // ufo
    body = opSubtraction(sdBox(b_pos - vec3(0.0, 2.2, 0.), vec3(0.94, 0.5, 0.94)),body);

    vec3 mul_pos = b_pos - vec3(0.0, -0.86, 1.01);
    opModInterval1(mul_pos.y,0.25,0.0,8.0);
    body = opSubtraction(sdBox(mul_pos, vec3(0.2, 0.03, 0.1)),body);

    float window1 = sdBox(b_pos - vec3(-0.5, 0.0, 1.01), vec3(0.1, 1.2, 0.1));
    float window2 = sdBox(b_pos - vec3(0.5, 0.0, 1.01), vec3(0.1, 1.2, 0.1));
    body = opSubtraction(window1, body );
    body = opSubtraction(window2, body );


    base = opUnion(base,body);
    base = opUnion(base,building);
    base += sin((noise(p.xy*15.) + noise(p.xz*15.)))*0.0015;

    float base_dist = 12.0-noise((p.xy+p.xz)*0.01)*8.;
    float base_dist2 = 0.25;
    float n = (noise(p.xy*base_dist) + noise(p.xz*base_dist));
    float n2 = (noise(p.xy*base_dist2) + noise(p.xz*base_dist2));
    base = base + (n2*n2*0.08*n*n)*.14;

    sand = opUnion(sand,base);

    if (base<=MINIMUM_HIT_DISTANCE){
        c=3.0;
    }



    // BLOB

    float blob = opSmoothIntersection(
        noise(p.xy*0.42+iTime)+noise(p.xz*0.42+iTime),
        sdSphere(p-vec3(-40.,12.0,0.),2.0),
        0.4);
    sand = opUnion(sand,blob);

    if (blob<=MINIMUM_HIT_DISTANCE){
        c=5.0;
    }

    // vehicle/ornicopter

    // RETURN
    return vec2(opSmoothUnion(opUnion(sand,hero),worm,0.1),c);
}


/*
 * END of SDF WORLD
 *
 */


vec3 calcNormal(in vec3 p){
    vec2 e = vec2(0.0001,0.0);
    return normalize(
        vec3(sdWorld(p+e.xyy).x-sdWorld(p-e.xyy).x,
            sdWorld(p+e.yxy).x-sdWorld(p-e.yxy).x,
            sdWorld(p+e.yyx).x-sdWorld(p-e.yyx).x));
}

vec2 castRay(in vec3 ro, vec3 rd){
    float t = 0.0;
    float c = 0.0;
    for (int i=0; i<NUMBER_OF_STEPS; i++){
        vec3 pos = ro + t*rd;
        vec2 world = sdWorld(pos);
        float dis = world.x;
        c = world.y;
        if (dis<MINIMUM_HIT_DISTANCE) break;
        t+=dis;
        if (t>MAXIMUM_TRACE_DISTANCE) break;
    }
    if (t>MAXIMUM_TRACE_DISTANCE) t = -1.0;
    return vec2(t,c);
}

vec3 render(in vec2 p, in vec3 ro, in vec3 ta){
    vec3 ww = normalize (ta-ro);
    vec3 uu = normalize( cross(ww, vec3(0,1,0)));
    vec3 vv = normalize (cross(uu,ww));

    vec3 rd = normalize(p.x*uu+p.y*vv+1.5*ww);

    vec3 col = vec3(0.4,0.75,1.0) - 0.5*rd.y;
    col =  mix(col, vec3(1.9,1.9,1.9), exp(-2.0*rd.y));

    vec3 mate = vec3(0.2);
    vec2 ray = castRay(ro,rd);
    float t = ray.x;

    if (t>0.0){
        vec3 pos = ro+t*rd;
        vec3 nor = calcNormal(pos);
        vec3 sun_dir = normalize(vec3(0.6,1.3,0.6));
        float sun_shadow = step(castRay( pos+nor*0.001, sun_dir).x, 0.0);
        float sun_dif = clamp(dot(nor,sun_dir),0.0,1.0);
        float sky_dif = clamp(0.5 + 0.5*dot(nor,vec3(0.0,1.0,0.0)),0.0,1.0);
        float bou_dif = clamp(0.5 + 0.5*dot(nor,vec3(0.0,-1.0,0.0)),0.0,1.0);
        float worm_tex_s = 0.25;
        float base_tex_s = 0.15;

        // MATERIALS
        if(ray.y>=5.0){
            col  = mate*vec3(8.,1.0+1.0*nor.z,1.0+1.0*nor.y);
        }else if(ray.y>=4.0){
            col  = mate*vec3(0.,0.,0.);
        }else if(ray.y>=3.0){
            col  = mate*vec3(0.5)*(noise(pos.xy*base_tex_s) + noise(pos.yx*base_tex_s)+ noise(pos.xz*base_tex_s));
        }else if(ray.y>=2.0){
            col  = mate*vec3(0.,1.,3.0);
        }else if(ray.y>=1.0){
            col  = mate*vec3(1.2,0.5,0.15)+(noise(pos.xy*worm_tex_s) + noise(pos.xx*worm_tex_s)+ noise(pos.xz*worm_tex_s))*.15;
        }else{
            col  = mate*vec3(1.2,0.5,0.15)*4.;
        }


        // ENVIRONMENT LIGHTING
        col += mate*vec3(5.0,3.0,2.0)*sun_dif*sun_shadow;
        col += mate*vec3(0.5,0.8,0.9)*sky_dif;
        col += mate*vec3(0.7,0.3,0.2)*bou_dif;
        col = mix( col, vec3(1.9,1.9,1.9), 1.0-exp( -0.00002*t*t*t ) );
    }
    return col;
}



vec3 ray_march(in vec3 ro, in vec3 rd)
{
    float distance = 0.0;

    for (int i = 0; i < NUMBER_OF_STEPS; ++i)
    {
        vec3 current_position = ro + distance * rd;
        float closest = sdWorld(current_position).x;
        if (closest < MINIMUM_HIT_DISTANCE)
        {
            vec3 normal = calcNormal(current_position);
            vec3 light_position = vec3(sin(iTime*4.0)*4.0, -5.0*cos(iTime*4.0)*4.0, 3.0);
            vec3 direction_to_light = normalize(current_position - light_position);
            float diffuse_intensity = max(0.1, dot(normal, direction_to_light));

            return vec3(0.5+0.5*sin(iTime*0.6)*normal.z,
                        0.5+0.5*sin(iTime*0.7)*normal.x,
                        0.5+0.5*sin(iTime)*normal.y) * diffuse_intensity;
        }

        if (distance > MAXIMUM_TRACE_DISTANCE) break;
        distance += closest;
    }

    return vec3(0.0);
}

void main()
{
    vec2 uv = (2.0 * gl_FragCoord.xy-iResolution.xy)/ iResolution.y;
    vec3 ro = vec3(0.);
    vec3 ta = vec3(0.);
    float dir_time = iTime;
    float time = dir_time;

    // DIRECTION
    if(dir_time>60.){
        time -= 60.;
        time *= 0.25;
        ro = vec3(-90.0 - time,2.5,0.);
        ta = vec3(0.0,2.5,0.);
    }else
    if(dir_time>38.){
        time -= 38.;
        time *= 0.25;
        time += 25.;
        ro = vec3(-4.0 + 8.0*sin(5.+time*1.2), 7.0 + 5.0*sin(time*1.7), 7.5 + 5.0*cos(time*1.6));
        ta = vec3(.0);
    }else
    if(dir_time>34.){
        time -= 34.;
        time *= 0.25;
        time += 55.;
        ro = vec3(-4.0 + 8.0*sin(5.+time*1.2), 7.0 + 5.0*sin(time*1.7), 7.5 + 5.0*cos(time*1.6));
        ta = vec3(.0);
    }else
    if(dir_time>30.){
        time -= 30.;
        time *= 0.25;
        ro = vec3(-4.0 + 8.0*sin(5.+time*1.2), 7.0 + 5.0*sin(time*1.7), 7.5 + 5.0*cos(time*1.6));
        ta = vec3(.0);
    }else
    if(dir_time>25.){
        time -= 25.;
        time *= 0.25;
        ro = vec3(0.,1.,5.-time);
        ta = vec3(0.,1.,4.-time);
    }else
    if(dir_time>20.){
        time -= 20.;
        time *= 0.25;
        ro = vec3(0.,3.,15.-time);
        ta = vec3(0.,3.,14.-time);
    }else
    if(dir_time>16.){
        time -= 16.;
        time *= 0.25;
        ro = vec3(-4.0,5.,6.0 - time);
        ta = vec3(-6.0,5.1,5.0 - time);
    }else
    if(dir_time>8.){
        time -= 8.;
        time *= 0.25;
        ro = vec3(-44.0,12.,-3.0 + time);
        ta = vec3(-15.0,8.,-3.0 + time);
    }else
    if(dir_time>4.){
        time -= 4.;
        time *= 0.25;
        ro = vec3(-70. + time,5.,0.0);
        ta = vec3(0.0,2.1,0.0);
    }else
    {
        time *= 0.25;
        ro = vec3(-100.0 + time,1.,0.);
        ta = vec3(0.0,1.,0.);
    }

    vec3 col = render(uv, ro, ta);

    col = 1.15*col/(1.0+col); // compress
    col = pow( col, vec3(0.4545) ); // gamma

    col *= 1.0 - 0.18 * dot(uv, uv);

    gl_FragColor = vec4(col,1.0);
}
