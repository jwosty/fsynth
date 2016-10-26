namespace Synth.SdlHost
open Microsoft.FSharp.NativeInterop
open OpenGL
open SDL2
open Synth
open Synth.SdlHost.HelperFunctions
open System
open System.Runtime.InteropServices

// pointer stuff
#nowarn "9"

type PianoKey = {
    noteAndOctave: Note * int
    position: Vector2
    natural: bool
    pressed: bool
    /// The SDL_SCANCODE of key on the computer keyboard that controls this piano key
    charKeyMapping: SDL.SDL_Scancode option
    cutoutWidth1: float32; cutoutWidth2: float32 }

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
    let update (mousePosition, leftMouseDown) keyboard pianoKey =
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
            else
                // for now just use a negative index as the note and octave. Don't use 0 because the midi sequencer uses that ID (at t = 0)
                // this is a bad system because collisions between different midi event sources are really easy
                let id = -(Note.noteToKeyIndex pianoKey.noteAndOctave) - 1
                [(if pressed' then NoteOn(pianoKey.noteAndOctave, id) else NoteOff(id))], true
        { pianoKey with pressed = pressed' }, events, needsRedraw
    
    /// Returns the fill color to use for drawing the piano key
    let fillColor pianoKey =
        let white =
            match pianoKey.natural, pianoKey.pressed with
            | true, false -> 1.f
            | true, true -> 0.8f
            | false, false -> 0.2f
            | false, true -> 0.1f
        vec3 (white, white, white)
    
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

type PianoKeyboard = { position: Vector2; keys: PianoKey list }

type PianoKeyboardView(modelMatrix: Matrix4, meshes: Mesh list) =
    member val ModelMatrix = modelMatrix with get, set
    member val Meshes = meshes
    interface IDisposable with
        override this.Dispose () =
            this.Meshes |> List.iter dispose

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PianoKeyboard =
    /// Initializes a collection of piano keys in their initial states
    let createKeys () =
        // TODO: fix the 1-pixel gap mouse input gap between white and black keys
        let pianoKeyboardPosition = 0 @@ 0
        [for octave in 1..5 do
            // an octave is 168 pixels wide
            let keyboardOctaveStart = float32 (octave - 1) * 168.f
            let naturals = [C, 0.f, (SDL.SDL_Scancode.SDL_SCANCODE_A, Some(SDL.SDL_Scancode.SDL_SCANCODE_K))
                            D, 24.f, (SDL.SDL_Scancode.SDL_SCANCODE_S, Some(SDL.SDL_Scancode.SDL_SCANCODE_L))
                            E, 48.f, (SDL.SDL_Scancode.SDL_SCANCODE_D, Some(SDL.SDL_Scancode.SDL_SCANCODE_SEMICOLON))
                            F, 72.f, (SDL.SDL_Scancode.SDL_SCANCODE_F, Some(SDL.SDL_Scancode.SDL_SCANCODE_APOSTROPHE))
                            G, 96.f, (SDL.SDL_Scancode.SDL_SCANCODE_G, None)
                            A, 120.f, (SDL.SDL_Scancode.SDL_SCANCODE_H, None)
                            B, 144.f, (SDL.SDL_Scancode.SDL_SCANCODE_J, None)]
            
            let sharps = [CS, 16.f, (SDL.SDL_Scancode.SDL_SCANCODE_W, Some(SDL.SDL_Scancode.SDL_SCANCODE_O))
                          DS, 44.f, (SDL.SDL_Scancode.SDL_SCANCODE_E, Some(SDL.SDL_Scancode.SDL_SCANCODE_P))
                          FS, 86.f, (SDL.SDL_Scancode.SDL_SCANCODE_T, Some(SDL.SDL_Scancode.SDL_SCANCODE_RIGHTBRACKET))
                          GS, 114.f, (SDL.SDL_Scancode.SDL_SCANCODE_Y, None)
                          AS, 142.f, (SDL.SDL_Scancode.SDL_SCANCODE_U, None)]
            
            // white keys
            for (note, x, charKey) in naturals do
                // Look for a key that overlaps on the left and determine how much it overlaps
                let leftOverlap =
                    sharps |> List.tryFind (fun (note, kx, _) -> kx < x && kx + PianoKey.blackKeySize.x > x)
                    |> Option.map (fun (note, kx, _) -> kx + PianoKey.blackKeySize.x - x)
                let leftOverlap = match leftOverlap with | Some(x) -> x | None -> 0.f
                // Look for a key that overlaps on the right and determinate how much it overlaps
                let rightOverlap =
                    sharps |> List.tryFind (fun (k, kx, _) -> kx > x && kx < x + PianoKey.whiteKeySize.x)
                    |> Option.map (fun (k, kx, _) -> x + PianoKey.whiteKeySize.x - kx)
                let rightOverlap = match rightOverlap with | Some(x) -> x | None -> 0.f
                
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
                        cutoutWidth1 = 0.f; cutoutWidth2 = 0.f }]
        |> List.sortBy (fun k -> let note, octave = k.noteAndOctave in octave, note)
    
    let create () = { position = 0 @@ 0; keys = createKeys () }
    
    let update leftMouseDown (mousePosition: Vector2) keyboard pianoKeyboard =
        let pianoKeys, (midiEvents, redraws) =
            pianoKeyboard.keys |> List.mapFold (fun (midiEventsAcc, redrawsAcc) pianoKey ->
                let pianoKey', midiEvents, needsRedraw = PianoKey.update (mousePosition.x @@ mousePosition.y, leftMouseDown) keyboard pianoKey
                pianoKey', (midiEvents @ midiEventsAcc, if needsRedraw then pianoKey' :: redrawsAcc else redrawsAcc)) ([], [])
        { pianoKeyboard with keys = pianoKeys }, midiEvents, redraws
    
    /// Creates a ready-to-use VBO of the fills of the piano keys
    let createFillVAO pianoKeyboard =
        let vertices, colors =
            [for pianoKey in pianoKeyboard.keys do
                let fillColor = PianoKey.fillColor pianoKey
                for vertex in PianoKey.fillVertices pianoKey do
                    // color is on a per-vertex basis, not per object
                    yield vertex, fillColor]
            |> List.unzip
        Mesh.create BufferUsageHint.DynamicDraw BufferUsageHint.StaticDraw BeginMode.Triangles vertices colors
    
    /// Creates a ready-to-use VBO of the outlines of the piano keys
    let createOutlineVAO pianoKeyboard =
        let vertices =
            [for pianoKey in pianoKeyboard.keys do
                let vs = PianoKey.outlineVertices pianoKey
                let vs = Array.append vs [|vs.[0]|] |> Array.pairwise
                for (v1, v2) in vs do
                    yield v1
                    yield v2]
        let colors = List.init vertices.Length (fun _ -> vec3(0, 0, 0))
        Mesh.create BufferUsageHint.StaticDraw BufferUsageHint.StaticDraw BeginMode.Lines vertices colors
    
    /// Update the PianoKeyboard VAOs with a given PianoKey state to be used next time it is rendered
    let updateVAOs (pianoKeyboardView: PianoKeyboardView) pianoKey =
        [for i in 1..12 -> PianoKey.fillColor pianoKey]
        |> submitVec3Data pianoKeyboardView.Meshes.[0].ColorVBO ((Note.noteToKeyIndex pianoKey.noteAndOctave - 4) * 12)