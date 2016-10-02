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
      keyboardFillVAO: VAO
      keyboardOutlineVAO: VAO
      keyboardShader: uint32
      pianoKeyboard: PianoKey list }

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
        if screenSizeLoc = -1 then failwith "Could not find screenSize shader parameter"
        Gl.Uniform2fv (screenSizeLoc, 1, [|float32 width; float32 height|])
    
    let create () =
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
        
        let pianoKeyboard = PianoKeyboard.create ()
        
        Gl.Disable EnableCap.DepthTest
        Gl.DepthFunc DepthFunction.Never
        
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
              keyboardFillVAO = PianoKeyboard.createFillVAO pianoKeyboard
              keyboardOutlineVAO = PianoKeyboard.createOutlineVAO pianoKeyboard
              pianoKeyboard = pianoKeyboard }
        
        setScreenSize gui (width, height)
        
        gui
    
    let renderGl gui =
        Gl.ClearColor (0.8f, 0.8f, 0.85f, 1.f)
        Gl.Clear ClearBufferMask.ColorBufferBit
        
        Gl.UseProgram gui.keyboardShader
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
    
    let update gui =
        let x, y = ref 0, ref 0
        let leftMouseDown = SDL.SDL_GetMouseState (x, y) &&& SDL.SDL_BUTTON(SDL.SDL_BUTTON_LEFT) <> 0u
        let mousePosition = !x @@ !y
        let numKeys = ref 0
        let keyboard = SDL.SDL_GetKeyboardState numKeys |> NativePtr.ofNativeInt
        let pianoKeyboard, (midiEvents, redraws) =
            gui.pianoKeyboard |> List.mapFold (fun (midiEventsAcc, redrawsAcc) pianoKey ->
                let pianoKey', midiEvents, needsRedraw = PianoKey.update (!x @@ !y, leftMouseDown) keyboard pianoKey
                pianoKey', (midiEvents @ midiEventsAcc, if needsRedraw then pianoKey' :: redrawsAcc else redrawsAcc)) ([], [])
        { gui with pianoKeyboard = pianoKeyboard }, midiEvents, redraws
    
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
    
    let rec runLoop (gui: Gui) (audioController: AudioController) activeNotes =
        let events = pollEvents ()
        if events |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_QUIT)
        then
            // Do this instead of implementing Dispose() because Gui is immutable and can be copied all the time
            for vbo in gui.keyboardFillVAO.vbos @ gui.keyboardOutlineVAO.vbos do
                Gl.DeleteBuffer vbo
            Gl.DeleteVertexArrays (1, [|gui.keyboardFillVAO.id|])
            Gl.DeleteVertexArrays (1, [|gui.keyboardOutlineVAO.id|])
            SDL.SDL_GL_DeleteContext gui.glContext
            SDL.SDL_DestroyWindow gui.window
        else
            let gui, midiEvents, redraws = update gui
            
            let activeNotes = List.fold (fun activeNotes midiEvent -> processMidiEvent audioController activeNotes midiEvent) activeNotes midiEvents
            
            for event in events do
                if event.``type`` = SDL.SDL_EventType.SDL_WINDOWEVENT && event.window.windowEvent = SDL.SDL_WindowEventID.SDL_WINDOWEVENT_SIZE_CHANGED then
                    // data1 and data2 here correspond to the new size of the window
                    setScreenSize gui (event.window.data1, event.window.data2)
                    renderGl gui
            
            draw gui redraws
            
            // delay (so we don't hog the CPU) and repeat gui loop
            Thread.Sleep 20
            runLoop gui audioController activeNotes
    
    let start gui audioController =
        renderGl gui
        runLoop gui audioController Map.empty
    
module Main =
    [<EntryPoint>]
    let main argv =
        let gui = Gui.create ()
        
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
        Gui.start gui audioController
        audioController.Stop ()
        0