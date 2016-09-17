module Synth.SdlHost.Main
open Microsoft.FSharp.NativeInterop
open SDL2
open Synth
open Synth.SdlHost.HelperFunctions
open System
open System.Threading

let initGui () =
    // TODO: fix the 1-pixel gap mouse input gap between white and black keys
    let pianoKeyboardPosition = 0 @@ 0
    [for octave in 1..5 do
        // an octave is 168 pixels wide
        let keyboardOctaveStart = (octave - 1) * 168
        let sharps = [CS, 16; DS, 44; FS, 86; GS, 114; AS, 142]

        // white keys
        for (note, x) in [C, 0; D, 24; E, 48; F, 72; G, 96; A, 120; B, 144] do
            // Look for a key that overlaps on the left and determine how much it overlaps
            let leftOverlap =
                sharps |> List.tryFind (fun (note, kx) -> kx < x && kx + PianoKey.blackKeySize.x > x)
                |> Option.map (fun (note, kx) -> kx + PianoKey.blackKeySize.x - x)
            let leftOverlap = match leftOverlap with | Some(x) -> x | None -> 0
            // Look for a key that overlaps on the right and determinate how much it overlaps
            let rightOverlap =
                sharps |> List.tryFind (fun (k, kx) -> kx > x && kx < x + PianoKey.whiteKeySize.x)
                |> Option.map (fun (k, kx) -> x + PianoKey.whiteKeySize.x - kx)
            let rightOverlap = match rightOverlap with | Some(x) -> x | None -> 0

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
                    cutoutWidth1 = 0; cutoutWidth2 = 0 }]

let drawGui window renderer pianoKeys =
    List.iter (PianoKey.draw renderer) pianoKeys

let updateGui pianoKeys =
    let x, y = ref 0, ref 0
    let leftMouseDown = SDL.SDL_GetMouseState (x, y) &&& SDL.SDL_BUTTON(SDL.SDL_BUTTON_LEFT) <> 0u
    let mousePosition = !x @@ !y
    // Map each piano key by the singular update function and keep track of the events they generate
    pianoKeys |> List.mapFold (fun midiEventsAcc pianoKey ->
        let pianoKey', midiEvents = PianoKey.update (!x @@ !y, leftMouseDown) pianoKey
        pianoKey', midiEvents @ midiEventsAcc) []

let rec runGuiLoop window renderer (audioController: AudioController) gui =
    let events = pollEvents ()
    if events |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_QUIT)
    then ()
    else
        let gui, midiEvents = updateGui gui
        
        for midiEvent in midiEvents do
            match midiEvent with
            | NoteOn(note, octave) -> audioController.StartNote (note, octave)
            | NoteOff(note, octave) -> audioController.StopNote (note, octave)
        
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
                    [1, GeneratorNode({ genFunc = Waveform.sin; phase = 0. }, MidiInput, Constant 0.75, Constant 0.)]
                    |> Map.ofList
                use audioController = new AudioController(44100, oscillator, 1)
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