module Synth.SdlHost.HelperFunctions
open Microsoft.FSharp.NativeInterop
open OpenGL
open SDL2
open Synth.SdlHost

// pointer stuff
#nowarn "51"
#nowarn "9"

let inline vec3 (x, y, z) = new OpenGL.Vector3(float32 x, float32 y, float32 z)
let inline (@@) x y = new OpenGL.Vector2(float32 x, float32 y)

/// Gets the last SDL error message and raises it in an exception
let sdlErr () = failwith (SDL.SDL_GetError ())

// default enum function is garbage...
let inline enum (value: 'T) : 'Enum = LanguagePrimitives.EnumOfValue value

let rec pollEvents () =
    let mutable event = ref Unchecked.defaultof<_>
    if SDL.SDL_PollEvent event = 0 then [] else !event :: pollEvents ()

let rectContainsPoint (topLeft: Vector2, bottomRight: Vector2) (point: Vector2) =
    point.x > topLeft.x && point.x < bottomRight.x
    && point.y > topLeft.y && point.y < bottomRight.y

type VAO = { id: uint32; count: int; vbos: uint32 list }