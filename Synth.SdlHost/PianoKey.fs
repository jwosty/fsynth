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
    let update (mousePosition, leftMouseDown) keyboardTriggering pianoKey =
        let (rect1, rect2) = bounds pianoKey
        let mouseTriggering = leftMouseDown && (rectContainsPoint rect1 mousePosition || rectContainsPoint rect2 mousePosition)
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

type PianoKeyboard = { position: Vector2; keys: PianoKey list; keyBindings: (SDL.SDL_Scancode * (Note * int)) List }

type PianoKeyboardView(modelMatrix: Matrix4, fillMesh: Mesh, outlineMesh: Mesh) =
    member val ModelMatrix = modelMatrix with get, set
    member val FillMesh = fillMesh
    member val OutlineMesh = outlineMesh
    interface IDisposable with
        override this.Dispose () =
            dispose this.FillMesh
            dispose this.OutlineMesh

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PianoKeyboard =
    /// Initializes a collection of piano keys in their initial states
    let createKeys () =
        // TODO: fix the 1-pixel gap mouse input gap between white and black keys
        let pianoKeyboardPosition = 0 @@ 0
        [for octave in 1..5 do
            // an octave is 168 pixels wide
            let keyboardOctaveStart = float32 (octave - 1) * 168.f
            let sharps =      [CS, 16.f;  DS, 44.f;             FS, 86.f;  GS, 114.f;  AS, 142.f]
            let naturals = [C, 0.f;    D, 24.f;   E, 48.f;   F, 72.f;   G, 96.f;    A, 120.f;    B, 144.f]
            
            // white keys
            for (note, x) in naturals do
                // Look for a key that overlaps on the left and determine how much it overlaps
                let leftOverlap =
                    sharps |> List.tryFind (fun (note, kx) -> kx < x && kx + PianoKey.blackKeySize.x > x)
                    |> Option.map (fun (note, kx) -> kx + PianoKey.blackKeySize.x - x)
                let leftOverlap = match leftOverlap with | Some(x) -> x | None -> 0.f
                // Look for a key that overlaps on the right and determinate how much it overlaps
                let rightOverlap =
                    sharps |> List.tryFind (fun (k, kx) -> kx > x && kx < x + PianoKey.whiteKeySize.x)
                    |> Option.map (fun (k, kx) -> x + PianoKey.whiteKeySize.x - kx)
                let rightOverlap = match rightOverlap with | Some(x) -> x | None -> 0.f
                
                yield { noteAndOctave = note, octave
                        position = (keyboardOctaveStart + x @@ 0) + pianoKeyboardPosition
                        natural = true
                        pressed = false
                        cutoutWidth1 = leftOverlap; cutoutWidth2 = rightOverlap }
            
            // black keys
            for (note, x) in sharps do
                yield { noteAndOctave = note, octave
                        position = (keyboardOctaveStart + x @@ 0) + pianoKeyboardPosition
                        natural = false
                        pressed = false
                        cutoutWidth1 = 0.f; cutoutWidth2 = 0.f }]
        |> List.sortBy (fun k -> let note, octave = k.noteAndOctave in octave, note)
    
    let create () = { position = 0 @@ 0; keys = createKeys ()
                      keyBindings = [ SDL.SDL_Scancode.SDL_SCANCODE_A, (C, 4);  SDL.SDL_Scancode.SDL_SCANCODE_W, (CS, 4)
                                      SDL.SDL_Scancode.SDL_SCANCODE_S, (D, 4);  SDL.SDL_Scancode.SDL_SCANCODE_E, (DS, 4)
                                      SDL.SDL_Scancode.SDL_SCANCODE_D, (E, 4);
                                      SDL.SDL_Scancode.SDL_SCANCODE_F, (F, 4);  SDL.SDL_Scancode.SDL_SCANCODE_T, (FS, 4)
                                      SDL.SDL_Scancode.SDL_SCANCODE_G, (G, 4);  SDL.SDL_Scancode.SDL_SCANCODE_Y, (GS, 4)
                                      SDL.SDL_Scancode.SDL_SCANCODE_H, (A, 4);  SDL.SDL_Scancode.SDL_SCANCODE_U, (AS, 4)
                                      SDL.SDL_Scancode.SDL_SCANCODE_J, (B, 4);
                                      SDL.SDL_Scancode.SDL_SCANCODE_K, (C, 5);  SDL.SDL_Scancode.SDL_SCANCODE_O, (CS, 5);
                                      SDL.SDL_Scancode.SDL_SCANCODE_L, (D, 5);  SDL.SDL_Scancode.SDL_SCANCODE_P, (DS, 5)] }
    
    let update leftMouseDown (mousePosition: Vector2) sdlEvents keyboard pianoKeyboard =
        let keyboardTriggeredNotes =
            [for (k, (note, octave)) in pianoKeyboard.keyBindings do
                if NativePtr.get keyboard (int k) = 1uy
                then yield note, octave ]
        let pianoKeys, (midiEvents, redraws) =
            pianoKeyboard.keys |> List.mapFold (fun (midiEventsAcc, redrawsAcc) pianoKey ->
                let keyboardTriggering = List.exists (fun (note, octave) -> (note, octave) = pianoKey.noteAndOctave) keyboardTriggeredNotes
                let pianoKey', midiEvents, needsRedraw = PianoKey.update (mousePosition.x @@ mousePosition.y, leftMouseDown) keyboardTriggering pianoKey
                pianoKey', (midiEvents @ midiEventsAcc, if needsRedraw then pianoKey' :: redrawsAcc else redrawsAcc)) ([], [])
        // Z/X to raise/lower the active octave
        let sdlKeyDowns = sdlEvents |> List.choose (fun (sdlEvent: SDL.SDL_Event) -> if sdlEvent.``type`` = SDL.SDL_EventType.SDL_KEYDOWN then Some(sdlEvent.key.keysym.scancode) else None)
        let deltaOctave =
            (if sdlKeyDowns |> List.exists (fun keyDown -> keyDown = SDL.SDL_Scancode.SDL_SCANCODE_Z) then -1 else 0)
            + (if sdlKeyDowns |> List.exists (fun keyDown -> keyDown = SDL.SDL_Scancode.SDL_SCANCODE_X) then 1 else 0)
        let keyBindings =
            if deltaOctave = 0 then pianoKeyboard.keyBindings
            else pianoKeyboard.keyBindings |> List.map (fun (k, (note, octave)) -> k, (note, octave + deltaOctave))
        { pianoKeyboard with keys = pianoKeys; keyBindings = keyBindings }, midiEvents, redraws
    
    /// Creates a ready-to-use VBO of the fills of the piano keys
    let createFillVAO pianoKeyboard =
        [for pianoKey in pianoKeyboard.keys do
            let fillColor = PianoKey.fillColor pianoKey
            for vertex in PianoKey.fillVertices pianoKey do
                // color is on a per-vertex basis, not per object
                yield vertex, fillColor]
        |> Mesh.create BeginMode.Triangles
    
    /// Creates a ready-to-use VBO of the outlines of the piano keys
    let createOutlineVAO pianoKeyboard =
        [for pianoKey in pianoKeyboard.keys do
            let vs = PianoKey.outlineVertices pianoKey
            let vs = Array.append vs [|vs.[0]|] |> Array.pairwise
            for (v1, v2) in vs do
                yield v1
                yield v2]
        |> List.map (fun vertex -> vertex, vec3(0, 0, 0))
        |> Mesh.create BeginMode.Lines
    
    /// Update the PianoKeyboard VAOs with a given PianoKey state to be used next time it is rendered
    let updateVAOs (pianoKeyboardView: PianoKeyboardView) pianoKey =
        [for i in 1..12 -> PianoKey.fillColor pianoKey]
        |> submitVec3Data pianoKeyboardView.FillMesh.ColorVBO ((Note.noteToKeyIndex pianoKey.noteAndOctave - 4) * 12)