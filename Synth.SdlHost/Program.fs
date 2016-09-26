namespace Synth.SdlHost
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
      renderer: nativeint
      pianoKeyboard: PianoKey list }
    
    interface IDisposable with
        member this.Dispose () =
            SDL.SDL_DestroyWindow this.window
            SDL.SDL_DestroyRenderer this.renderer

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Gui =
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
    
    let createGui () =
        if SDL.SDL_Init SDL.SDL_INIT_VIDEO <> 0 then sdlErr ()
        let window = SDL.SDL_CreateWindow ("Synth", SDL.SDL_WINDOWPOS_UNDEFINED, SDL.SDL_WINDOWPOS_UNDEFINED, 840, 480, SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE)
        if window = 0n then sdlErr ()
        let renderer = SDL.SDL_CreateRenderer (window, -1, SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
        if renderer = 0n then sdlErr ()
        { window = window; renderer = renderer; pianoKeyboard = createPianoKeyboard () }
    
    let drawGui gui =
        List.iter (PianoKey.draw gui.renderer) gui.pianoKeyboard
    
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
            
            if SDL.SDL_SetRenderDrawColor (gui.renderer, 220uy, 220uy, 230uy, 0uy) <> 0 then sdlErr ()
            if SDL.SDL_RenderClear gui.renderer <> 0 then sdlErr ()
            drawGui gui
            SDL.SDL_RenderPresent gui.renderer
            
            // delay (so we don't hog the CPU) and repeat gui loop
            Thread.Sleep 20
            runGuiLoop gui audioController activeNotes
    
    [<EntryPoint>]
    let main argv =
        use gui = createGui ()
        let oscillator =
            [1, ADSREnvelopeNode(0.1, 0., 1., 1., 0.)
             2, GeneratorNode({ genFunc = Waveform.triangle; phase = 0. }, MidiInput, Constant 1., Constant 0.)
             3, GeneratorNode({ genFunc = Waveform.sin; phase = 0. }, MidiInput, Constant 1., Constant 0.)
             4, MixerNode(Input 1, [Input 2, Constant 0.5; Input 3, Constant 0.5])]
            |> Map.ofList
        use audioController = new AudioController(44100, oscillator, 4)
        audioController.Start ()
        runGuiLoop gui audioController Map.empty
        audioController.Stop ()
        0