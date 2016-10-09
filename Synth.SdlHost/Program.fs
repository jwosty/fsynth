namespace Synth.SdlHost
open OpenGL
open Microsoft.FSharp.NativeInterop
open SDL2
open Synth
open Synth.SdlHost.HelperFunctions
open System
open System.Threading
open System.Diagnostics

// pointer stuff
#nowarn "9"

// TODO: separate the views (VAOs, window handle, gl context, etc) from the models (pure PianoKeyboard + Sequencer)
type Gui =
    { window: nativeint
      glContext: nativeint
      pianoKeyboard: PianoKeyboard
      sequencer: Sequencer
      shader: uint32
      keyboardFillVAO: VAO
      keyboardOutlineVAO: VAO
      sequencerNotesOutlineVAO: VAO }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Gui =
    let windowSize gui =
        let w, h = ref 0, ref 0
        SDL.SDL_GetWindowSize (gui.window, w, h)
        !w, !h
    
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
            failwith (sprintf "GLSL compile error in %A: %s" shaderType log)
        
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
        Gl.UseProgram gui.shader
        let viewMatrix =
            Matrix4.CreateTranslation (vec3 (1.f, 1.f, 0.f))
          * Matrix4.CreateScaling (vec3 (1.f / float32 width * 2.f, 1.f / float32 height * -2.f, 0.f))
          * Matrix4.CreateTranslation (vec3 (-1.f, 1.f, 0.f))
        let viewLoc = Gl.GetUniformLocation (gui.shader, "view")
        if viewLoc >= 0 then Gl.UniformMatrix4fv (viewLoc, viewMatrix)
    
    let create sequencer =
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
        
        Gl.Disable EnableCap.DepthTest
        Gl.DepthFunc DepthFunction.Never
        
        let pianoKeyboard = { position = 0 @@ 0; keys = PianoKeyboard.create () }
        
        let vertexShader = """
#version 400
uniform vec2 screenSize;
uniform mat4 view;

layout(location = 0) in vec2 vertex;
layout(location = 1) in vec3 inputColor;

out vec3 vertexColor;

void main () {
    gl_Position = view * vec4(vertex.x, vertex.y, 1.0, 1.0);
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
              pianoKeyboard = pianoKeyboard
              sequencer = sequencer
              shader = compileShaderProgram vertexShader fragmentShader
              keyboardFillVAO = PianoKeyboard.createFillVAO pianoKeyboard
              keyboardOutlineVAO = PianoKeyboard.createOutlineVAO pianoKeyboard
              sequencerNotesOutlineVAO = Sequencer.createOutlineVAO sequencer }
        
        setScreenSize gui (width, height)
        
        gui
    
    let renderGl gui =
        Gl.ClearColor (0.8f, 0.8f, 0.85f, 1.f)
        Gl.Clear ClearBufferMask.ColorBufferBit
        
        Gl.UseProgram gui.shader
        // TODO: Only render the parts of the VBO that actually need it
        Gl.BindVertexArray gui.keyboardFillVAO.id
        Gl.DrawArrays (BeginMode.Triangles, 0, gui.keyboardFillVAO.count)
        Gl.BindVertexArray gui.keyboardOutlineVAO.id
        Gl.DrawArrays (BeginMode.LineStrip, 0, gui.keyboardOutlineVAO.count)
        
        SDL.SDL_GL_SwapWindow gui.window
    
    let draw gui redraws =
        match redraws with
        | [] -> ()
        | _ ->
            for pianoKey in redraws do
                PianoKey.submitGlData gui.keyboardFillVAO.vbos.[1] pianoKey
            renderGl gui
    
    let update gui audioController lastTime time =
        let mx, my = ref 0, ref 0
        let leftMouseDown = SDL.SDL_GetMouseState (mx, my) &&& SDL.SDL_BUTTON(SDL.SDL_BUTTON_LEFT) <> 0u
        let mousePosition = !mx @@ !my
        let numKeys = ref 0
        let keyboard = SDL.SDL_GetKeyboardState numKeys |> NativePtr.ofNativeInt
        
        // assume 4/4 time (quarter note gets the beat)
        let beat = gui.sequencer.bpm / 60. * time
        let lastBeat = gui.sequencer.bpm / 60. * lastTime
        
        let pianoKeyboard, midiEvents, redraws = PianoKeyboard.update leftMouseDown mousePosition keyboard gui.pianoKeyboard
        let sequencer = Sequencer.update lastBeat beat audioController gui.sequencer
        
        { gui with pianoKeyboard = pianoKeyboard; sequencer = sequencer }, midiEvents, redraws
    
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
    
    let rec runLoop (gui: Gui) (sequencerStopwatch: Stopwatch) lastTime (audioController: AudioController) activeNotes =
        let events = pollEvents ()
        if events |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_QUIT)
        then
            // Do this instead of implementing Dispose() because Gui is immutable and can be copied all the time
            Gl.DeleteProgram gui.shader
            for vbo in gui.keyboardFillVAO.vbos @ gui.keyboardOutlineVAO.vbos do
                Gl.DeleteBuffer vbo
            Gl.DeleteVertexArrays (1, [|gui.keyboardFillVAO.id|])
            Gl.DeleteVertexArrays (1, [|gui.keyboardOutlineVAO.id|])
            SDL.SDL_GL_DeleteContext gui.glContext
            SDL.SDL_DestroyWindow gui.window
        else
            // in seconds
            let deltaTime = float sequencerStopwatch.ElapsedTicks / float Stopwatch.Frequency
            sequencerStopwatch.Restart ()
            // also in seconds :)
            let time = lastTime + deltaTime
            
            let gui, midiEvents, redraws = update gui audioController lastTime time
            
            let activeNotes = List.fold (fun activeNotes midiEvent -> processMidiEvent audioController activeNotes midiEvent) activeNotes midiEvents
            
            for event in events do
                if event.``type`` = SDL.SDL_EventType.SDL_WINDOWEVENT && event.window.windowEvent = SDL.SDL_WindowEventID.SDL_WINDOWEVENT_SIZE_CHANGED then
                    // data1 and data2 here correspond to the new size of the window
                    setScreenSize gui (event.window.data1, event.window.data2)
                    renderGl gui
            
            draw gui redraws
            
            // delay (so we don't hog the CPU) and repeat gui loop
            Thread.Sleep 10
            
            runLoop gui sequencerStopwatch time audioController activeNotes
    
    let start gui audioController =
        renderGl gui
        let sequencerStopwatch = new Stopwatch()
        sequencerStopwatch.Start ()
        runLoop gui sequencerStopwatch 0. audioController Map.empty
    
module Main =
    [<EntryPoint>]
    let main argv =
        let t1 =
            [(E, 5), 1., 1.;                      (B, 4), 2., 0.5;   (C, 5), 2.5, 0.5;   (D, 5), 3., 1.;                       (C, 5), 4., 0.5;   (B, 4), 4.5, 0.5
             (A, 4), 5., 1.;                      (A, 4), 6., 0.5;   (C, 5), 6.5, 0.5;   (E, 5), 7., 1.;                       (D, 5), 8., 0.5;   (C, 5), 8.5, 0.5
             (B, 4), 9., 1.;                      (C, 5), 10.5, 0.5; (D, 5), 11., 1.;    (E, 5), 12., 1.;                                                         
             (C, 5), 13., 1.;                     (A, 4), 14., 1.;                       (A, 4), 15., 1.
             (D, 5), 17.5, 1.;                    (F, 5), 18.5, 0.5; (A, 5), 19., 1.;    (G, 5), 20., 0.5;  (F, 5), 20.5, 0.5
             (E, 5), 21., 1.;                                        (C, 5), 22.5, 0.5;  (E, 5), 23., 1.;                      (D, 5), 24., 0.5;  (C, 5), 24.5, 0.5
             (B, 4), 25., 1.;                                        (C, 5), 26.5, 0.5;  (D, 5), 27., 1.;                      (E, 5), 28., 1.;   
             (C, 5), 29., 1.;                     (A, 4), 30., 1.;                       (A, 4), 31., 1. ]
            |> List.map (fun (note, start, duration) -> note, start, if duration = 0.5 then 0.25 else duration - 0.5)
        let b =
            [(E, 2), 1., 0.5;  (E, 3), 1.5, 0.5;  (E, 2), 2., 0.5;   (E, 3), 2.5, 0.5;   (E, 2), 3., 0.5;   (E, 3), 3.5, 0.5;  (E, 2), 4., 0.5;   (E, 3), 4.5, 0.5
             (A, 2), 5., 0.5;  (A, 3), 5.5, 0.5;  (A, 2), 6., 0.5;   (A, 3), 6.5, 0.5;   (A, 2), 7., 0.5;   (A, 3), 7.5, 0.5;  (A, 2), 8., 0.5;   (A, 3), 8.5, 0.5
             (GS,2), 9., 0.5;  (GS,3), 9.5, 0.5;  (GS,2), 10., 0.5;  (GS,3), 10.5, 0.5;  (E, 2), 11., 0.5;  (E, 3), 11.5, 0.5; (E, 2), 12., 0.5;  (E, 3), 12.5, 0.5
             (A, 2), 13., 0.5; (A, 3), 13.5, 0.5; (A, 2), 14., 0.5;  (A, 3), 14.5, 0.5;  (A, 2), 15., 0.5;  (A, 3), 15.5, 0.5; (B, 2), 16., 0.5;  (C, 3), 16.5, 0.5
             (D, 3), 17., 0.5; (D, 2), 17.5, 0.5;                    (D, 2), 18.5, 0.5;                     (D, 2), 19.5, 0.5; (A, 2), 20., 0.5;  (F, 2), 20.5, 0.5
             (C, 2), 21., 0.5; (C, 3), 21.5, 0.5;                    (C, 3), 22.5, 0.5;  (C, 2), 23., 0.5;  (G, 2), 23.5, 0.5; (G, 2), 24., 0.5
             (B, 2), 25., 0.5; (B, 3), 25.5, 0.5;                    (B, 3), 26.5, 0.5;                     (E, 2), 27.5, 0.5;                    (GS,2), 28.5, 0.5
             (A, 2), 29., 0.5; (A, 3), 29.5, 0.5; (A, 2), 30., 0.5;  (A, 3), 30.5, 0.5;  (A, 2), 31., 1. ]
        
        let gui = Gui.create { notes = t1 @ b |> List.map (fun (noteAndOctave, start, duration) -> { noteAndOctave = noteAndOctave; start = start; duration = duration; id = None })
                               bpm = 150. }
        
        let oscillator =
            [1, ADSREnvelopeNode(0.001, 0.01, 0.7, 0.05, 0.)
             2, GeneratorNode({ genFunc = Waveform.triangle; phase = 0. }, MidiInput, Constant 1., Constant 0.)
             3, GeneratorNode({ genFunc = Waveform.square; phase = 0. }, MidiInput, Constant 1., Constant 0.)
             4, MixerNode(Input 1, [Input 2, Constant 0.5; Input 3, Constant 0.2])]
            |> Map.ofList
        
        let mutable sdlVersion = Unchecked.defaultof<_>
        SDL.SDL_GetVersion (&sdlVersion)
        printfn "Using SDL %i.%i.%i" sdlVersion.major sdlVersion.minor sdlVersion.patch
        printfn "Using OpenGL %s" (Gl.GetString StringName.Version)
        
        use audioController = new AudioController(44100, oscillator, 4)
        audioController.Start ()
        Gui.start gui audioController
        audioController.Stop ()
        0