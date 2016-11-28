module Synth.SdlHost.HelperFunctions
open Microsoft.FSharp.NativeInterop
open OpenGL
open SDL2
open Synth.SdlHost
open System
open System.Runtime.InteropServices

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
    let mutable event = Unchecked.defaultof<_>
    if SDL.SDL_PollEvent (&event) = 0 then [] else event :: pollEvents ()

let rectContainsPoint (topLeft: Vector2, bottomRight: Vector2) (point: Vector2) =
    point.x > topLeft.x && point.x < bottomRight.x
    && point.y > topLeft.y && point.y < bottomRight.y
let inline dispose (x: IDisposable) = x.Dispose ()

let flattenVec2s vector2List =
    [|for (vec: Vector2) in vector2List do
        yield vec.x
        yield vec.y|]

let rec getGlErrors () =
    let err = Gl.GetError ()
    if err = ErrorCode.NoError
    then []
    else err :: getGlErrors ()

let checkGl () =
    for error in getGlErrors () do
        failwith ("GL error: " + string error)

/// Uploads a Vector2 sequence to the currently bound buffer
let submitVec2Data vbo offset vec2s =
    let glData =
        [|for (v: Vector2) in vec2s do
            yield v.x
            yield v.y|]
    // Get a pointer to glData without having to copy it just to give it to OpenGL
    let pinnedGlData = GCHandle.Alloc (glData, GCHandleType.Pinned)
    Gl.BindBuffer (BufferTarget.ArrayBuffer, vbo)
    checkGl ()
    Gl.BufferSubData (BufferTarget.ArrayBuffer,
                      nativeint (offset * 2 * sizeof<float32>),
                      nativeint (glData.Length * sizeof<float32>),
                      pinnedGlData.AddrOfPinnedObject ())
    checkGl ()
    pinnedGlData.Free ()

/// Upload a Vector3 sequence to a VBO
let submitVec3Data vbo offset vec2s =
    let glData =
        [|for (v: Vector3) in vec2s do
            yield v.x
            yield v.y
            yield v.z|]
    Gl.BindBuffer (BufferTarget.ArrayBuffer, vbo)
    // Get a pointer to glData without having to copy it just to give it to OpenGL
    let pinnedGlData = GCHandle.Alloc (glData, GCHandleType.Pinned)
    Gl.BufferSubData (BufferTarget.ArrayBuffer,
                      nativeint (offset * 3 * sizeof<float32>),
                      nativeint (glData.Length * sizeof<float32>),
                      pinnedGlData.AddrOfPinnedObject ())
    pinnedGlData.Free ()

/// Creates a new VBO with a given size and copies the contents from a given VBO
let copyAndResizeVbo vbo newSize =
    checkGl ()
    let newSize = newSize * sizeof<float32>
    Gl.BindBuffer (BufferTarget.ArrayBuffer, vbo)
    checkGl ()
    
    let mutable param = [|0|]
    Gl.GetBufferParameteriv (BufferTarget.ArrayBuffer, BufferParameterName.BufferSize, param)
    checkGl ()
    let oldSize = param.[0]
    printfn "newSize: %i" newSize
    printfn "oldSize: %i" oldSize
    
    let newVbo = Gl.GenBuffer ()
    printfn "newVbo: %i" newVbo
    Gl.BindBuffer (BufferTarget.CopyWriteBuffer, newVbo)
    checkGl ()
    Gl.BufferData (BufferTarget.CopyWriteBuffer, nativeint newSize, 0n, BufferUsageHint.DynamicDraw)
    checkGl ()
    Gl.BindBuffer (BufferTarget.CopyReadBuffer, vbo)
    checkGl ()
    Gl.CopyBufferSubData (BufferTarget.CopyReadBuffer, BufferTarget.CopyWriteBuffer, 0n, 0n, 12n)//nativeint oldSize)
    checkGl ()
    newVbo