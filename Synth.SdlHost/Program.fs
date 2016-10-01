namespace Synth.SdlHost
open OpenGL
open Microsoft.FSharp.NativeInterop
open SDL2
open Synth
open Synth.SdlHost.HelperFunctions
open System
open System.Threading

// pointer stuff
#nowarn "9"

type Gui =
    { window: nativeint
      glContext: nativeint
      keyboardVAOId: uint32
      /// Element count of the buffers referenced by the VAO (e.g. vertex buffer & color buffer)
      keyboardVAOCount: int
      keyboardShader: uint32
      pianoKeyboard: PianoKey list }
    
    interface IDisposable with
        member this.Dispose () =
            SDL.SDL_GL_DeleteContext this.glContext
            SDL.SDL_DestroyWindow this.window

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Gui =
    let windowSize gui =
        let w, h = ref 0, ref 0
        SDL.SDL_GetWindowSize (gui.window, w, h)
        !w, !h
    
    let createPianoKeyboard () =
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
    
    let compileShader shaderSource shaderType =
        let s = Gl.CreateShader shaderType
        Gl.ShaderSource (s, shaderSource)
        Gl.CompileShader s
        
        let compileStatus = [|0|]
        Gl.GetShaderiv (s, ShaderParameter.CompileStatus, compileStatus)
        if compileStatus.[0] <> 1 then
            let logLength = [|0|]
            Gl.GetShaderiv (s, ShaderParameter.InfoLogLength, logLength)
            let log = if logLength.[0] > 1 then Gl.GetShaderInfoLog s else ""
            failwith (sprintf "Failed to compile %A: %s" shaderType log)
        
        s
    
    /// Compiles and links vertex and fragment shader source into a complete shader program
    let compileShaderProgram vertexShaderSource fragmentShaderSource =
        let shaderProgram = Gl.CreateProgram ()
        let vertexShader = compileShader vertexShaderSource ShaderType.VertexShader
        let fragmentShader = compileShader fragmentShaderSource ShaderType.FragmentShader
        Gl.AttachShader (shaderProgram, vertexShader)
        Gl.AttachShader (shaderProgram, fragmentShader)
        Gl.LinkProgram shaderProgram
        
        let linkStatus = [|0|]
        Gl.GetProgramiv (shaderProgram, ProgramParameter.LinkStatus, linkStatus)
        if linkStatus.[0] <> 1 then
            let logLength = [|0|]
            Gl.GetProgramiv (shaderProgram, ProgramParameter.InfoLogLength, logLength)
            let log = if logLength.[0] > 1 then Gl.GetProgramInfoLog shaderProgram else ""
            failwith (sprintf "Failed to link shader program: %s" log)
        
        // Linked shader program no longer depends on individual shaders still existing
        Gl.DetachShader (shaderProgram, vertexShader)
        Gl.DeleteShader vertexShader
        Gl.DetachShader (shaderProgram, fragmentShader)
        Gl.DeleteShader fragmentShader
        
        shaderProgram
    
    /// Set OpenGL parameters for a given screen size. Should be called whenever the window size changes.
    let setScreenSize gui (width, height) =
        Gl.Viewport (0, 0, width, height)
        Gl.UseProgram gui.keyboardShader
        // Find the location of the shader's screenSize parameter
        let screenSizeLoc = Gl.GetUniformLocation (gui.keyboardShader, "screenSize")
        if screenSizeLoc = -1 then
            printfn "Warning: could not find screenSize shader parameter"
        else
            Gl.Uniform2fv (screenSizeLoc, 1, [|float32 width; float32 height|])
    
    let createGui () =
        // Initialize SDL (OpenGL 4.1, double buffered)
        if SDL.SDL_Init SDL.SDL_INIT_VIDEO <> 0 then sdlErr ()
        if (SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_PROFILE_MASK, int SDL.SDL_GLprofile.SDL_GL_CONTEXT_PROFILE_CORE)) <> 0 then sdlErr ()
        if (SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DOUBLEBUFFER, 1)) <> 0 then sdlErr ()
        if (SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MAJOR_VERSION, 4)) <> 0 then sdlErr ()
        if (SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MINOR_VERSION, 1)) <> 0 then sdlErr ()
        
        // Create one window with an OpenGL rendering context
        let width, height = 840, 480
        let window = SDL.SDL_CreateWindow ("Synth", SDL.SDL_WINDOWPOS_UNDEFINED, SDL.SDL_WINDOWPOS_UNDEFINED, width, height, SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE ||| SDL.SDL_WindowFlags.SDL_WINDOW_OPENGL)
        if window = 0n then sdlErr ()
        let glContext = SDL.SDL_GL_CreateContext window
        if glContext = 0n then sdlErr ()
        
        let pianoKeyboard = createPianoKeyboard ()
        
        Gl.Enable EnableCap.DepthTest
        Gl.DepthFunc DepthFunction.Less
        
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
        Gl.VertexAttribPointer (0, 2, VertexAttribPointerType.Float, false, 0, 0n)    // corresponds to: layout(location = 0) in vec3 vp
        
        Gl.BindBuffer (BufferTarget.ArrayBuffer, fillColorBuffer)
        Gl.EnableVertexAttribArray 1
        Gl.VertexAttribPointer (1, 3, VertexAttribPointerType.Float, false, 0, 0n)    // corresponds to: layout(location = 1) in vec3 vertexColor
        
        let vertexShader = """
#version 400
uniform vec2 screenSize;

layout(location = 0) in vec2 vertex;
layout(location = 1) in vec3 inputColor;

out vec3 vertexColor;

void main () {
    gl_Position = vec4(((vertex.x + 1.0) / screenSize.x * 2.0) - 1.0, 1.0 - ((vertex.y + 1) / screenSize.y * 2.0), 0.0, 1.0);
    vertexColor = inputColor;
}"""
        let fragmentShader = """
#version 400
in vec3 vertexColor;
out vec3 fragmentColor;
void main () {
    fragmentColor = vertexColor;
}"""
        
        let gui =
            { window = window; glContext = glContext
              keyboardShader = compileShaderProgram vertexShader fragmentShader
              keyboardVAOId = keyboardVAO
              keyboardVAOCount = vertices.Length
              pianoKeyboard = pianoKeyboard }
        
        setScreenSize gui (width, height)
        
        gui
    
    let drawGui gui =
        Gl.ClearColor (0.8f, 0.8f, 0.85f, 1.f)
        Gl.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
        
        Gl.UseProgram gui.keyboardShader
        let width, height = windowSize gui
        Gl.BindVertexArray gui.keyboardVAOId
        Gl.DrawArrays (BeginMode.Triangles, 0, gui.keyboardVAOCount)
        Gl.UseProgram 0u
        
        SDL.SDL_GL_SwapWindow gui.window
    
    let updateGui gui =
        let x, y = ref 0, ref 0
        let leftMouseDown = SDL.SDL_GetMouseState (x, y) &&& SDL.SDL_BUTTON(SDL.SDL_BUTTON_LEFT) <> 0u
        let mousePosition = !x @@ !y
        let numKeys = ref 0
        let keyboard = SDL.SDL_GetKeyboardState numKeys |> NativePtr.ofNativeInt
        let pianoKeyboard, midiEvents =
            gui.pianoKeyboard |> List.mapFold (fun midiEventsAcc pianoKey ->
                let pianoKey', midiEvents = PianoKey.update (!x @@ !y, leftMouseDown) keyboard pianoKey
                pianoKey', midiEvents @ midiEventsAcc) []
        { gui with pianoKeyboard = pianoKeyboard }, midiEvents
    
    let processMidiEvent (audioController: AudioController) activeNotes midiEvent =
        match midiEvent with
        | NoteOn(note, octave) ->
            let id = audioController.NoteOn (note, octave)
            Map.add (note, octave) id activeNotes
        | NoteOff(note, octave) ->
            match Map.tryFind (note, octave) activeNotes with
            | Some(id) ->
                audioController.NoteOff id
                Map.remove (note, octave) activeNotes
            | None ->
                printfn "NoteOff failed: Note %s%i not active. This is probably a (non-fatal) bug." (string note) octave
                activeNotes
    
    let rec runGuiLoop (gui: Gui) (audioController: AudioController) activeNotes =
        let events = pollEvents ()
        if events |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_QUIT)
        then ()
        else
            let gui, midiEvents = updateGui gui
            
            let activeNotes = List.fold (fun activeNotes midiEvent -> processMidiEvent audioController activeNotes midiEvent) activeNotes midiEvents
            
            let width, height = windowSize gui
            
            if events |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_WINDOWEVENT && event.window.windowEvent = SDL.SDL_WindowEventID.SDL_WINDOWEVENT_SIZE_CHANGED) then
                setScreenSize gui (width, height)
            
            drawGui gui
            
            // delay (so we don't hog the CPU) and repeat gui loop
            Thread.Sleep 20
            runGuiLoop gui audioController activeNotes
    
    [<EntryPoint>]
    let main argv =
        use gui = createGui ()
        
        let sdlVersion = ref Unchecked.defaultof<_>
        SDL.SDL_GetVersion sdlVersion
        let sdlVersion = !sdlVersion
        printfn "Using SDL %i.%i.%i" sdlVersion.major sdlVersion.minor sdlVersion.patch
        printfn "Using OpenGL %s" (Gl.GetString StringName.Version)
        
        let oscillator =
            [1, ADSREnvelopeNode(0.01, 0., 1., 0.2, 0.)
             2, GeneratorNode({ genFunc = Waveform.triangle; phase = 0. }, MidiInput, Constant 1., Constant 0.)
             3, GeneratorNode({ genFunc = Waveform.sin; phase = 0. }, MidiInput, Constant 1., Constant 0.)
             4, MixerNode(Input 1, [Input 2, Constant 0.5; Input 3, Constant 0.5])]
            |> Map.ofList
        use audioController = new AudioController(44100, oscillator, 4)
        audioController.Start ()
        runGuiLoop gui audioController Map.empty
        audioController.Stop ()
        0