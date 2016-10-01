namespace Synth.SdlHost
open Microsoft.FSharp.NativeInterop
open OpenGL
open SDL2
open Synth
open Synth.SdlHost.HelperFunctions

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
    let whiteKeySize = 25 @@ 100
    let blackKeySize = 14 @@ 56
    
    /// Returns the two bounding boxes that make up a piano key
    let bounds pianoKey =
        let size = if pianoKey.natural then whiteKeySize else blackKeySize
        let rect1Start = pianoKey.position.x + pianoKey.cutoutWidth1 @@ pianoKey.position.y
        let rect1End = pianoKey.position.x + size.x - pianoKey.cutoutWidth2 @@ pianoKey.position.y + blackKeySize.y
        let rect2Start = pianoKey.position.x @@ pianoKey.position.y + blackKeySize.y
        let rect2End = pianoKey.position.x + size.x@@ pianoKey.position.y + size.y
        (rect1Start, rect1End), (rect2Start, rect2End)
    
    let update (mousePosition, leftMouseDown) (keyboard: nativeptr<uint8>) pianoKey =
        let (rect1, rect2) = bounds pianoKey
        let mouseTriggering = leftMouseDown && (rectContainsPoint rect1 mousePosition || rectContainsPoint rect2 mousePosition)
        let keyboardTriggering =
            match pianoKey.charKeyMapping with
            | Some(k) -> NativePtr.get keyboard (int k) = 1uy
            | None -> false
        let pressed' = mouseTriggering || keyboardTriggering
        let events =
            if pressed' = pianoKey.pressed
            then []
            else [(if pressed' then NoteOn else NoteOff) pianoKey.noteAndOctave]
        { pianoKey with pressed = pressed' }, events
    
    /// Returns the fill color to use for drawing the piano key
    let fillColor pianoKey =
        let white =
            match pianoKey.natural, pianoKey.pressed with
            | true, false -> 1.f
            | true, true -> 0.8f
            | false, false -> 0.2f
            | false, true -> 0.1f
        white, white, white
    
    /// Returns the graphical vertex data in screen coordinates of a piano key that can be used for rendering
    let createMesh pianoKey =
        // rect1 and rect2 are the top (variably sized) and lower (uniform sized) half of the piano key, respectively
        let (rect1Start, rect1End), (rect2Start, rect2End) = bounds pianoKey    
        [|rect1Start; rect1End.x @@ rect1Start.y; rect1End
          rect1Start; rect1End; rect1Start.x @@ rect1End.y
          rect2Start; rect2End.x @@ rect2Start.y; rect2End
          rect2Start; rect2End; rect2Start.x @@ rect2End.y|]
    
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
    
    /// Creates a ready-to-use VBO containing all the data needed to render the given keyboard, returning the VBO and the element count of its buffers
    let createVAOMesh keyboardShader pianoKeyboard =
        let vertexBuffer = Gl.GenBuffer ()
        let vertices =
            [|for pianoKey in pianoKeyboard do
                // Flatten a single key's vertices into an array of floats that OpenGL can interpret
                for vertex in PianoKey.createMesh pianoKey do
                    yield float32 vertex.x
                    yield float32 vertex.y|]
        Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
        Gl.BufferData (BufferTarget.ArrayBuffer, vertices.Length * sizeof<float32>, vertices, BufferUsageHint.StaticDraw)
        
        let fillColorBuffer = Gl.GenBuffer ()
        // this won't update since its only called once; figure that out next
        //let fillColors = Array.create (vertices.Length / 2 * 3) (PianoKey.fillColor pianoKeyboard.[0])
        let fillColors =
            [|for pianoKey in pianoKeyboard do
                let r, g, b = PianoKey.fillColor pianoKey
                for i in 1..12 do
                    yield r; yield g; yield b|]
        Gl.BindBuffer (BufferTarget.ArrayBuffer, fillColorBuffer)
        Gl.BufferData (BufferTarget.ArrayBuffer, fillColors.Length * sizeof<float32>, fillColors, BufferUsageHint.StaticDraw)
        
        let keyboardVAO = Gl.GenVertexArray ()
        Gl.BindVertexArray keyboardVAO
        
        Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
        Gl.EnableVertexAttribArray 0
        // vertex is parameter index 0 in shader
        Gl.VertexAttribPointer (0, 2, VertexAttribPointerType.Float, false, 0, 0n)
        
        Gl.BindBuffer (BufferTarget.ArrayBuffer, fillColorBuffer)
        Gl.EnableVertexAttribArray 1
        // inColor is parameter index 1 in shader
        Gl.VertexAttribPointer (1, 3, VertexAttribPointerType.Float, false, 0, 0n)
        
        keyboardVAO, vertices.Length