module Synth.SdlHost.HelperFunctions
open Microsoft.FSharp.NativeInterop
open OpenGL
open SDL2

// pointer stuff
#nowarn "51"
#nowarn "9"

//[<AutoOpen>]
let (@@) x y = { x = x; y = y }

/// Gets the last SDL error message and raises it in an exception
let sdlErr () = failwith (SDL.SDL_GetError ())

// default enum function is garbage...
let inline enum (value: 'T) : 'Enum = LanguagePrimitives.EnumOfValue value

let rec pollEvents () =
    let mutable event = ref Unchecked.defaultof<_>
    if SDL.SDL_PollEvent event = 0 then [] else !event :: pollEvents ()

let drawLines renderer lines =
    let lines = lines |> Array.map (fun v -> new SDL.SDL_Point(x = v.x, y = v.y))
    if SDL.SDL_RenderDrawLines (renderer, lines, lines.Length) <> 0 then sdlErr ()

let fillRect renderer rect =
    let mutable rect = rect
    if SDL.SDL_RenderFillRect (renderer, NativePtr.toNativeInt &&rect) <> 0 then sdlErr ()

let rectContainsPoint (topLeft, bottomRight) point =
    point.x > topLeft.x && point.x < bottomRight.x
    && point.y > topLeft.y && point.y < bottomRight.y

type VAO = { id: uint32; count: int; vbos: uint32 list }