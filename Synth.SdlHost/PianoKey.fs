﻿namespace Synth.SdlHost
open Microsoft.FSharp.NativeInterop
open OpenGL
open SDL2
open Synth
open Synth.SdlHost.HelperFunctions
open System.Runtime.InteropServices

// pointer stuff
#nowarn "9"

type PianoKey = {
    noteAndOctave: Note * int
    position: Vector2<int>
    natural: bool
    pressed: bool
    /// The SDL_SCANCODE of key on the computer keyboard that controls this piano key
    charKeyMapping: SDL.SDL_Scancode option
    cutoutWidth1: int; cutoutWidth2: int }

type MidiEvent = | NoteOn of Note * int | NoteOff of Note * int

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PianoKey =
    let whiteKeySize = 24 @@ 100
    let blackKeySize = 14 @@ 56
    
    /// Returns the two bounding boxes that make up a piano key
    let bounds pianoKey =
        let size = if pianoKey.natural then whiteKeySize else blackKeySize
        let rect1Start = pianoKey.position.x + pianoKey.cutoutWidth1 @@ pianoKey.position.y
        let rect1End = pianoKey.position.x + size.x - pianoKey.cutoutWidth2 @@ pianoKey.position.y + blackKeySize.y
        let rect2Start = pianoKey.position.x @@ pianoKey.position.y + blackKeySize.y
        let rect2End = pianoKey.position.x + size.x@@ pianoKey.position.y + size.y
        (rect1Start, rect1End), (rect2Start, rect2End)
    
    /// Updates the piano key, returning the new key, the midi events generated, and whether or not it should be redrawn
    let update (mousePosition, leftMouseDown) (keyboard: nativeptr<uint8>) pianoKey =
        let (rect1, rect2) = bounds pianoKey
        let mouseTriggering = leftMouseDown && (rectContainsPoint rect1 mousePosition || rectContainsPoint rect2 mousePosition)
        let keyboardTriggering =
            match pianoKey.charKeyMapping with
            | Some(k) -> NativePtr.get keyboard (int k) = 1uy
            | None -> false
        let pressed' = mouseTriggering || keyboardTriggering
        let events, needsRedraw =
            if pressed' = pianoKey.pressed
            then [], false
            else [(if pressed' then NoteOn else NoteOff) pianoKey.noteAndOctave], true
        { pianoKey with pressed = pressed' }, events, needsRedraw
    
    /// Returns the fill color to use for drawing the piano key
    let fillColor pianoKey =
        let white =
            match pianoKey.natural, pianoKey.pressed with
            | true, false -> 1.f
            | true, true -> 0.8f
            | false, false -> 0.2f
            | false, true -> 0.1f
        white, white, white
    
    /// A flat list of triangle vertices that makes up the inside of the piano key
    let fillVertices pianoKey =
        let (rect1Start, rect1End), (rect2Start, rect2End) = bounds pianoKey
        [|rect1Start; rect1End.x @@ rect1Start.y; rect1End
          rect1Start; rect1End; rect1Start.x @@ rect1End.y
          rect2Start; rect2End.x @@ rect2Start.y; rect2End
          rect2Start; rect2End; rect2Start.x @@ rect2End.y|]
    
    /// A list of vertices along the outer edge of the piano key
    let outlineVertices pianoKey =
        let (rect1Start, rect1End), (rect2Start, rect2End) = bounds pianoKey
        [|rect1Start; rect1End.x @@ rect1Start.y; rect1End;
          rect2End.x @@ rect2Start.y; rect2End; rect2Start.x @@ rect2End.y;
          rect2Start; rect1Start.x @@ rect2Start.y|]
    
    /// Re-sends geometry information to the GPU via OpenGL that will be used next time it is rendered
    let submitGlData fillColorsVbo pianoKey =
        // TODO: this and one other number have to be changed if the amount of vertices are changed... fix that!!
        let fillColors = [|for i in 1..12 do
                             let r, g, b = fillColor pianoKey
                             yield r; yield g; yield b|]
        let pinnedFillColors = GCHandle.Alloc(fillColors, GCHandleType.Pinned)
        Gl.BindBuffer (BufferTarget.ArrayBuffer, fillColorsVbo)
        Gl.BufferSubData (BufferTarget.ArrayBuffer,
                          nativeint ((Note.noteToKeyIndex pianoKey.noteAndOctave - 4) * 144),
                          nativeint (fillColors.Length * sizeof<float32>),
                          pinnedFillColors.AddrOfPinnedObject ())
        pinnedFillColors.Free ()
    
module PianoKeyboard =
    /// Initializes a new piano keyboard (just a list of keys)
    let create () =
        // TODO: fix the 1-pixel gap mouse input gap between white and black keys
        let pianoKeyboardPosition = 0 @@ 0
        [for octave in 1..5 do
            // an octave is 168 pixels wide
            let keyboardOctaveStart = (octave - 1) * 168
            let naturals = [C, 0, (SDL.SDL_Scancode.SDL_SCANCODE_A, Some(SDL.SDL_Scancode.SDL_SCANCODE_K))
                            D, 24, (SDL.SDL_Scancode.SDL_SCANCODE_S, Some(SDL.SDL_Scancode.SDL_SCANCODE_L))
                            E, 48, (SDL.SDL_Scancode.SDL_SCANCODE_D, Some(SDL.SDL_Scancode.SDL_SCANCODE_SEMICOLON))
                            F, 72, (SDL.SDL_Scancode.SDL_SCANCODE_F, Some(SDL.SDL_Scancode.SDL_SCANCODE_APOSTROPHE))
                            G, 96, (SDL.SDL_Scancode.SDL_SCANCODE_G, None)
                            A, 120, (SDL.SDL_Scancode.SDL_SCANCODE_H, None)
                            B, 144, (SDL.SDL_Scancode.SDL_SCANCODE_J, None)]
            
            let sharps = [CS, 16, (SDL.SDL_Scancode.SDL_SCANCODE_W, Some(SDL.SDL_Scancode.SDL_SCANCODE_O))
                          DS, 44, (SDL.SDL_Scancode.SDL_SCANCODE_E, Some(SDL.SDL_Scancode.SDL_SCANCODE_P))
                          FS, 86, (SDL.SDL_Scancode.SDL_SCANCODE_T, Some(SDL.SDL_Scancode.SDL_SCANCODE_RIGHTBRACKET))
                          GS, 114, (SDL.SDL_Scancode.SDL_SCANCODE_Y, None)
                          AS, 142, (SDL.SDL_Scancode.SDL_SCANCODE_U, None)]
            
            // white keys
            for (note, x, charKey) in naturals do
                // Look for a key that overlaps on the left and determine how much it overlaps
                let leftOverlap =
                    sharps |> List.tryFind (fun (note, kx, _) -> kx < x && kx + PianoKey.blackKeySize.x > x)
                    |> Option.map (fun (note, kx, _) -> kx + PianoKey.blackKeySize.x - x)
                let leftOverlap = match leftOverlap with | Some(x) -> x | None -> 0
                // Look for a key that overlaps on the right and determinate how much it overlaps
                let rightOverlap =
                    sharps |> List.tryFind (fun (k, kx, _) -> kx > x && kx < x + PianoKey.whiteKeySize.x)
                    |> Option.map (fun (k, kx, _) -> x + PianoKey.whiteKeySize.x - kx)
                let rightOverlap = match rightOverlap with | Some(x) -> x | None -> 0
                
                yield { noteAndOctave = note, octave
                        position = (keyboardOctaveStart + x @@ 0) + pianoKeyboardPosition
                        natural = true
                        pressed = false
                        charKeyMapping = if octave = 4 then Some(fst charKey) elif octave = 5 then snd charKey else None
                        cutoutWidth1 = leftOverlap; cutoutWidth2 = rightOverlap }
            
            // black keys
            for (note, x, charKey) in sharps do
                yield { noteAndOctave = note, octave
                        position = (keyboardOctaveStart + x @@ 0) + pianoKeyboardPosition
                        natural = false
                        pressed = false
                        charKeyMapping = if octave = 4 then Some(fst charKey) elif octave = 5 then snd charKey else None
                        cutoutWidth1 = 0; cutoutWidth2 = 0 }]
        |> List.sortBy (fun k -> let note, octave = k.noteAndOctave in octave, note)
    
    /// Creates a ready-to-use VBO of the fills of the piano keys
    let createFillVAO pianoKeyboard =
        let vao = Gl.GenVertexArray ()
        Gl.BindVertexArray vao
        
        let vertices =
            [|for pianoKey in pianoKeyboard do
                // Flatten a single key's vertices into an array of floats that OpenGL can interpret
                for vertex in PianoKey.fillVertices pianoKey do
                    yield float32 vertex.x; yield float32 vertex.y|]
        let vertexBuffer = Gl.GenBuffer ()
        Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
        Gl.BufferData (BufferTarget.ArrayBuffer, vertices.Length * sizeof<float32>, vertices, BufferUsageHint.StaticDraw)
        Gl.EnableVertexAttribArray 0
        // vertex is parameter index 0 in shader
        Gl.VertexAttribPointer (0, 2, VertexAttribPointerType.Float, false, 0, 0n)
        
        // this won't update since its only called once; figure that out next
        let fillColors =
            [|for pianoKey in pianoKeyboard do
                let r, g, b = PianoKey.fillColor pianoKey
                for i in 1..12 do
                    yield r; yield g; yield b|]
        let fillColorBuffer = Gl.GenBuffer ()
        Gl.BindBuffer (BufferTarget.ArrayBuffer, fillColorBuffer)
        Gl.BufferData (BufferTarget.ArrayBuffer, fillColors.Length * sizeof<float32>, fillColors, BufferUsageHint.StaticDraw)
        Gl.EnableVertexAttribArray 1
        // inColor is parameter index 1 in shader
        Gl.VertexAttribPointer (1, 3, VertexAttribPointerType.Float, false, 0, 0n)
        
        { id = vao; count = vertices.Length / 2; vbos = [vertexBuffer; fillColorBuffer] }
    
    /// Creates a ready-to-use VBO of the outlines of the piano keys
    let createOutlineVAO pianoKeyboard =
        let vao = Gl.GenVertexArray ()
        Gl.BindVertexArray vao
        
        let vertices =
            [|for pianoKey in pianoKeyboard do
                  let vs = PianoKey.outlineVertices pianoKey
                  let vs = Array.append vs [|vs.[0]|] |> Array.pairwise
                  for (v1, v2) in vs do
                      yield float32 v1.x; yield float32 v1.y
                      yield float32 v2.x; yield float32 v2.y|]
        let vertexBuffer = Gl.GenBuffer ()
        Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
        Gl.BufferData (BufferTarget.ArrayBuffer, vertices.Length * sizeof<float32>, vertices, BufferUsageHint.StaticDraw)
        Gl.EnableVertexAttribArray 0
        // vertex is parameter index 0 in shader
        Gl.VertexAttribPointer (0, 2, VertexAttribPointerType.Float, false, 0, 0n)
        
        let outlineColors =
            [|for pianoKey in pianoKeyboard do
                  for i in 0..15 do
                      yield 0.f; yield 0.f; yield 0.f|]
        let outlineColorBuffer = Gl.GenBuffer ()
        Gl.BindBuffer (BufferTarget.ArrayBuffer, outlineColorBuffer)
        Gl.BufferData (BufferTarget.ArrayBuffer, outlineColors.Length * sizeof<float32>, outlineColors, BufferUsageHint.StaticDraw)
        Gl.EnableVertexAttribArray 1
        // inColor is parameter index 1 in shader
        Gl.VertexAttribPointer (1, 3, VertexAttribPointerType.Float, false, 0, 0n)
        
        { id = vao; count = vertices.Length / 2; vbos = [vertexBuffer; outlineColorBuffer] }