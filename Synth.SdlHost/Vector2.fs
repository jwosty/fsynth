namespace Synth.SdlHost
    
type Vector2<'t> =
    { x: 't; y: 't }
    static member ( + ) (v1, v2) = { x = v1.x + v2.x; y = v1.y + v2.y }
    static member ( - ) (v1, v2) = { x = v1.x - v2.x; y = v1.y - v2.y }
    static member ( ~-) v = { x = -v.x; y = -v.y }
    static member ( * ) (v1, v2) = { x = v1.x * v2.x; y = v1.y * v2.y }
    static member ( / ) (v1, v2) = { x = v1.x / v2.x; y = v1.y / v2.y }