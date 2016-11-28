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

type Gui = { pianoKeyboard: PianoKeyboard; sequencer: Sequencer }
type GuiView(window: nativeint, glContext: nativeint, shader: uint32,
             pianoKeyboardView: PianoKeyboardView,
             sequencerView: SequencerView)=
    member val Window = window
    member val GlContext = glContext
    member val Shader: uint32 = shader
    member val ViewMatrix = Matrix4.Identity with get, set
    member val FramerateStopwatch = new Stopwatch()
    
    member val PianoKeyboardView = pianoKeyboardView
    
    member val SequencerView = sequencerView
    
    interface IDisposable with
        override this.Dispose () =
            dispose this.PianoKeyboardView
            dispose this.SequencerView
            Gl.DeleteProgram this.Shader
            SDL.SDL_GL_DeleteContext this.GlContext
            SDL.SDL_DestroyWindow this.Window

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Gui =
    let create sequencer = { pianoKeyboard = PianoKeyboard.create (); sequencer = sequencer }
    
    let update audioController (inverseSequencerModelMatrix: Matrix4) sequencerTime sdlEvents gui =
        let mx, my = ref 0, ref 0
        let leftMouseDown = SDL.SDL_GetMouseState (mx, my) &&& SDL.SDL_BUTTON(SDL.SDL_BUTTON_LEFT) <> 0u
        let mousePosition = !mx @@ !my
        let numKeys = ref 0
        let keyboard = SDL.SDL_GetKeyboardState numKeys |> NativePtr.ofNativeInt
        
        // assume 4/4 time (quarter note gets the beat)
        let beat = gui.sequencer.bpm / 60. * sequencerTime
        
        let pianoKeyboard, pianoKeyboardMidiEvents, pianoKeyRedraws = PianoKeyboard.update leftMouseDown mousePosition sdlEvents keyboard gui.pianoKeyboard
        let sequencer, sequencerMidiEvents, playheadAction, sequencerNoteRedraws =
            let modelMousePosition = new Vector4(mousePosition.x, mousePosition.y, 1.f, 1.f) * inverseSequencerModelMatrix
            Sequencer.update audioController (modelMousePosition.x @@ modelMousePosition.y) beat keyboard sdlEvents gui.sequencer
        
        { pianoKeyboard = pianoKeyboard; sequencer = sequencer }, pianoKeyboardMidiEvents @ sequencerMidiEvents, playheadAction, pianoKeyRedraws, sequencerNoteRedraws

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GuiView =
    let vertexShaderSource = """
#version 400
uniform vec2 screenSize;
uniform mat4 modelViewMatrix;

layout(location = 0) in vec2 vertex;
layout(location = 1) in vec3 inputColor;

out vec3 vertexColor;

void main () {
gl_Position = modelViewMatrix * vec4(vertex.x, vertex.y, 1.0, 1.0);
vertexColor = inputColor;
}"""
    let fragmentShaderSource = """
#version 400
in vec3 vertexColor;

out vec3 fragmentColor;

void main () {
fragmentColor = vertexColor;
}"""
    
    let windowSize (guiView: GuiView) =
        let w, h = ref 0, ref 0
        SDL.SDL_GetWindowSize (guiView.Window, w, h)
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
    
    /// Sets a uniform by name in the current GLSL shader to the given value, throwing an error if the parameter doesn't exist
    let setUniform f paramName value shader =
        let paramLocation = Gl.GetUniformLocation (shader, paramName)
        if paramLocation < 0 then failwith (sprintf "Shader uniform '%s' not found" paramName)
        else f (paramLocation, value)
    
    /// Change the viewport and view matrix to match a screen size
    let setScreenSize (guiView: GuiView) (width, height) =
        Gl.Viewport (0, 0, width, height)
        Gl.UseProgram guiView.Shader
        guiView.ViewMatrix <-
            Matrix4.CreateTranslation (vec3 (0.5f, 0.5f, 0.f))
          * Matrix4.CreateScaling (vec3 (1.f / float32 width * 2.f, 1.f / float32 height * -2.f, 0.f))
          * Matrix4.CreateTranslation (vec3 (-1.f, 1.f, 0.f))
    
    /// A reference to the last Gui and GuiView that was rendered
    let mutable cachedGui = None
    
    /// Draw the VBOs and present the result to the window
    let renderGl gui (guiView: GuiView) =
        cachedGui <- Some(gui, guiView)
        Gl.ClearColor (0.8f, 0.8f, 0.85f, 1.f)
        Gl.Clear ClearBufferMask.ColorBufferBit
        
        Gl.UseProgram guiView.Shader
        
        setUniform Gl.UniformMatrix4fv "modelViewMatrix" (guiView.PianoKeyboardView.ModelMatrix * guiView.ViewMatrix) guiView.Shader
        Mesh.draw guiView.PianoKeyboardView.FillMesh
        Mesh.draw guiView.PianoKeyboardView.OutlineMesh
        
        setUniform Gl.UniformMatrix4fv "modelViewMatrix" (guiView.SequencerView.NotesModelMatrix * guiView.SequencerView.ModelMatrix * guiView.ViewMatrix) guiView.Shader
        let noteIdsToDraw = (gui.sequencer.notes |> List.map (fun note -> note.id))
        Mesh.drawObjects 2 noteIdsToDraw guiView.SequencerView.NotesFillMesh
        Mesh.drawObjects 4 noteIdsToDraw guiView.SequencerView.NotesOutlineMesh
        
        setUniform Gl.UniformMatrix4fv "modelViewMatrix" (guiView.SequencerView.PlayheadModelMatrix * guiView.SequencerView.ModelMatrix * guiView.ViewMatrix) guiView.Shader
        Mesh.draw guiView.SequencerView.PlayheadMesh
        
        SDL.SDL_GL_SwapWindow guiView.Window
    
    /// Resubmit vertex buffer data based on the widgets that need to be redrawn, then present it to the window
    let draw gui (guiView: GuiView) pianoKeyRedraws sequencerNoteRedraws =
        for pianoKey in pianoKeyRedraws do
            PianoKeyboard.updateVAOs guiView.PianoKeyboardView pianoKey
        for sequencerNote in sequencerNoteRedraws do
            Sequencer.updateVAOs guiView.SequencerView sequencerNote
        renderGl gui guiView
    
    /// A delegate that, when hooked into the SDL event queue using SDL_SetEventFilter, notices the intermediate window
    /// resize events and appropriately issues re-render calls. This is necessary to get around SDL blocking the main thread
    /// during window resizes and moves (causing unpleasent screen blanking)
    let dynamicResizeEventFilterHook = SDL.SDL_EventFilter(fun _ sdlEvent ->
        let sdlEvent: SDL.SDL_Event = NativePtr.ofNativeInt sdlEvent |> NativePtr.read
        if sdlEvent.``type`` = SDL.SDL_EventType.SDL_WINDOWEVENT then
            // TODO: figure out if the exposed event is ALWAYS sent after a window resize on every platform, and not just my OS X box
            if sdlEvent.window.windowEvent = SDL.SDL_WindowEventID.SDL_WINDOWEVENT_EXPOSED then
                match cachedGui with
                | Some(gui, guiView) ->
                    setScreenSize guiView (windowSize guiView)
                    renderGl gui guiView
                | none -> ()
        1)
    
    let rec runLoop gui (guiView: GuiView) lastTime (audioController: AudioController) =
        let sdlEvents = pollEvents ()
        if sdlEvents |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_QUIT) |> not
        then
            // all of this is in seconds
            // TODO: Use the timing stuff to regulate framerate
            let deltaTime = float guiView.FramerateStopwatch.ElapsedTicks / float Stopwatch.Frequency
            guiView.FramerateStopwatch.Restart ()
            let time = lastTime + deltaTime
            let sequencerTime = float guiView.SequencerView.BeatStopwatch.ElapsedTicks / float Stopwatch.Frequency
            
            let gui, midiEvents, playheadAction, pianoKeyRedraws, sequencerNoteRedraws =
                Gui.update audioController (guiView.SequencerView.ModelMatrix.Inverse () * guiView.SequencerView.NotesModelMatrix.Inverse ()) sequencerTime sdlEvents gui
            
            match playheadAction with
            | Some(Play) -> guiView.SequencerView.BeatStopwatch.Start ()
            | Some(Pause) -> guiView.SequencerView.BeatStopwatch.Stop ()
            | Some(JumpToStart) ->
                if gui.sequencer.paused then guiView.SequencerView.BeatStopwatch.Reset ()
                else guiView.SequencerView.BeatStopwatch.Restart ()
            | None -> ()
            
            midiEvents |> List.iter audioController.RecieveEvent
            
            guiView.SequencerView.PlayheadModelMatrix <- Matrix4.CreateTranslation (vec3(gui.sequencer.beat * 30., 0, 0))
            draw gui guiView pianoKeyRedraws sequencerNoteRedraws
            
            // delay (so we don't hog the CPU) and repeat gui loop
            Thread.Sleep 1
            
            runLoop gui guiView time audioController
    
    let start gui guiView audioController =
        renderGl gui guiView
        if not gui.sequencer.paused then guiView.SequencerView.BeatStopwatch.Start ()
        runLoop gui guiView 0. audioController
    
    let create gui =
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
        
        SDL.SDL_SetEventFilter (dynamicResizeEventFilterHook, 0n)
        
        Gl.Disable EnableCap.DepthTest
        Gl.DepthFunc DepthFunction.Never
        
        let guiView = new GuiView(window, glContext,
                                  compileShaderProgram vertexShaderSource fragmentShaderSource,
                                  new PianoKeyboardView(Matrix4.Identity, PianoKeyboard.createFillVAO gui.pianoKeyboard, PianoKeyboard.createOutlineVAO gui.pianoKeyboard),
                                  new SequencerView(Sequencer.createFillVAO gui.sequencer, Sequencer.createOutlineVAO gui.sequencer, Sequencer.createPlayheadVAO 630))
        
        setScreenSize guiView (width, height)
        
        guiView
    
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
        
        let oscillator =
            [1, ADSREnvelopeNode(0.001, 0.01, 0.7, 0.05, 0.)
             2, GeneratorNode({ genFunc = Waveform.triangle; phase = 0. }, MidiInput, Constant 1., Constant 0.)
             3, GeneratorNode({ genFunc = Waveform.square; phase = 0. }, MidiInput, Constant 1., Constant 0.)
             4, MixerNode(Input 1, [Input 2, Constant 0.5; Input 3, Constant 0.1])]
            |> Map.ofList
        
        //                                                      just use the note index as the unique ID
        let gui = Gui.create { notes = t1 @ b |> List.mapi (fun i (noteAndOctave, start, duration) -> { noteAndOctave = noteAndOctave; start = start - 1.; duration = duration; id = i })
                               bpm = 150.
                               beat = 0.
                               paused = true
                               draggedNoteAndOffset = None }
        use guiView = GuiView.create gui
        
        let mutable sdlVersion = Unchecked.defaultof<_>
        SDL.SDL_GetVersion (&sdlVersion)
        printfn "Using SDL %i.%i.%i" sdlVersion.major sdlVersion.minor sdlVersion.patch
        printfn "Using OpenGL %s" (Gl.GetString StringName.Version)
        
        use audioController = new AudioController(44100, oscillator, 4)
        audioController.Start ()
        GuiView.start gui guiView audioController
        audioController.Stop ()
        0