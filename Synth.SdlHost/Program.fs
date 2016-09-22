module Synth.SdlHost.Main
open Microsoft.FSharp.NativeInterop
open SDL2
open Synth
open Synth.SdlHost.HelperFunctions
open System
open System.Threading

// pointer stuff
#nowarn "9"

let initGui () =
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

let drawGui window renderer pianoKeys =
    List.iter (PianoKey.draw renderer) pianoKeys

let updateGui pianoKeys =
    let x, y = ref 0, ref 0
    let leftMouseDown = SDL.SDL_GetMouseState (x, y) &&& SDL.SDL_BUTTON(SDL.SDL_BUTTON_LEFT) <> 0u
    let mousePosition = !x @@ !y
    let numKeys = ref 0
    let keyboard = SDL.SDL_GetKeyboardState numKeys |> NativePtr.ofNativeInt
    // Map each piano key by the singular update function and keep track of the events they generate
    pianoKeys |> List.mapFold (fun midiEventsAcc pianoKey ->
        let pianoKey', midiEvents = PianoKey.update (!x @@ !y, leftMouseDown) keyboard pianoKey
        pianoKey', midiEvents @ midiEventsAcc) []

let rec runGuiLoop window renderer (audioController: AudioController) gui =
    let events = pollEvents ()
    if events |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_QUIT)
    then ()
    else
        let gui, midiEvents = updateGui gui
        
        for midiEvent in midiEvents do
            match midiEvent with
            | NoteOn(note, octave) -> audioController.NoteOn (note, octave)
            | NoteOff(note, octave) -> audioController.NoteOff (note, octave)
        
        if SDL.SDL_SetRenderDrawColor (renderer, 220uy, 220uy, 230uy, 0uy) <> 0 then sdlErr ()
        if SDL.SDL_RenderClear renderer <> 0 then sdlErr ()
        drawGui window renderer gui
        SDL.SDL_RenderPresent renderer
        
        // delay (so we don't hog the CPU) and repeat gui loop
        Thread.Sleep 20
        runGuiLoop window renderer audioController gui

[<EntryPoint>]
let main argv =
    try
        if SDL.SDL_Init SDL.SDL_INIT_VIDEO <> 0 then sdlErr ()
        let window = SDL.SDL_CreateWindow ("Synth", SDL.SDL_WINDOWPOS_UNDEFINED, SDL.SDL_WINDOWPOS_UNDEFINED, 840, 480, SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE)
        try
            let renderer = SDL.SDL_CreateRenderer (window, -1, SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
            try
                let oscillator =
                    [1, ADSREnvelopeNode((0.01, 1.), (0.0, 1.), 0.01)
                     2, GeneratorNode({ genFunc = Waveform.sin; phase = 0. }, MidiInput, Input 1, Constant 0.)]
                    |> Map.ofList
                use audioController = new AudioController(44100, oscillator, 2)
                if renderer = IntPtr.Zero then sdlErr ()
                let gui = initGui ()
                audioController.Start ()
                runGuiLoop window renderer audioController gui
                audioController.Stop ()
            finally
                SDL.SDL_DestroyRenderer renderer
        finally
            SDL.SDL_DestroyWindow window
    finally
        SDL.SDL_Quit ()
    0