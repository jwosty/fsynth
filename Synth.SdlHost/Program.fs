open Microsoft.FSharp.NativeInterop
open SDL2
open Synth
open System
open System.Threading

// suppress warnings from use of pointers generating potentially unverifiable code
#nowarn "51"
#nowarn "9"

type Vector2<'t> =
    { x: 't; y: 't }
    static member ( + ) (v1, v2) = { x = v1.x + v2.x; y = v1.y + v2.y }
    static member ( - ) (v1, v2) = { x = v1.x - v2.x; y = v1.y - v2.y }
    static member ( ~-) v = { x = -v.x; y = -v.y }

type PianoKey = {
    noteAndOctave: Note * int
    position: Vector2<int>
    natural: bool
    pressed: bool
    cutoutWidth1: int; cutoutWidth2: int }

let (@@) x y = { x = x; y = y }

let sdlErr () = failwith (SDL.SDL_GetError ())

// default enum function is garbage...
let inline enum (value: 'T) : 'Enum = LanguagePrimitives.EnumOfValue value

let rec pollEvents () =
    let mutable event = ref Unchecked.defaultof<_>
    if SDL.SDL_PollEvent event = 0 then [] else !event :: pollEvents ()

let whiteKeySize = 25 @@ 100
let blackKeySize = 14 @@ 56

let drawLines renderer lines =
    let lines = lines |> Array.map (fun v -> new SDL.SDL_Point(x = v.x, y = v.y))
    if SDL.SDL_RenderDrawLines (renderer, lines, lines.Length) <> 0 then sdlErr ()

let fillRect renderer rect =
    let mutable rect = rect
    if SDL.SDL_RenderFillRect (renderer, NativePtr.toNativeInt &&rect) <> 0 then sdlErr ()

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
                sharps |> List.tryFind (fun (note, kx) -> kx < x && kx + blackKeySize.x > x)
                |> Option.map (fun (note, kx) -> kx + blackKeySize.x - x)
            let leftOverlap = match leftOverlap with | Some(x) -> x | None -> 0
            // Look for a key that overlaps on the right and determinate how much it overlaps
            let rightOverlap =
                sharps |> List.tryFind (fun (k, kx) -> kx > x && kx < x + whiteKeySize.x)
                |> Option.map (fun (k, kx) -> x + whiteKeySize.x - kx)
            let rightOverlap = match rightOverlap with | Some(x) -> x | None -> 0

            yield { noteAndOctave = note, octave
                    position = (keyboardOctaveStart + x @@ 0) + pianoKeyboardPosition
                    natural = true
                    pressed = note = A
                    cutoutWidth1 = leftOverlap; cutoutWidth2 = rightOverlap }
            
        // black keys
        for (note, x) in sharps do
            yield { noteAndOctave = note, octave
                    position = (keyboardOctaveStart + x @@ 0) + pianoKeyboardPosition
                    natural = false
                    pressed = note = DS
                    cutoutWidth1 = 0; cutoutWidth2 = 0 }]

/// Calculates two bounding boxes that encompass a piano key
let pianoKeyBounds pianoKey =
    let size = if pianoKey.natural then whiteKeySize else blackKeySize
    let rect1Start = pianoKey.position.x + pianoKey.cutoutWidth1 @@ pianoKey.position.y
    let rect1End = pianoKey.position.x + size.x - pianoKey.cutoutWidth2 @@ pianoKey.position.y + blackKeySize.y
    let rect2Start = pianoKey.position.x @@ pianoKey.position.y + blackKeySize.y
    let rect2End = pianoKey.position.x + size.x@@ pianoKey.position.y + size.y
    (rect1Start, rect1End), (rect2Start, rect2End)

let drawPianoKey renderer pianoKey =
    // rect1 and rect2 are the top (variably sized) and lower (uniform sized) half of the piano key, respectively
    let (rect1Start, rect1End), (rect2Start, rect2End) = pianoKeyBounds pianoKey    
    let fill =
        match pianoKey.natural, pianoKey.pressed with
        | true, false -> 255uy
        | true, true -> 200uy
        | false, false -> 55uy
        | false, true -> 30uy

    // rectangles for fill
    if SDL.SDL_SetRenderDrawColor (renderer, fill, fill, fill, 0uy) <> 0 then sdlErr ()
    new SDL.SDL_Rect(x = rect1Start.x, y = rect1Start.y, w = rect1End.x - rect1Start.x, h = rect1End.y - rect1Start.y)
    |> fillRect renderer
    new SDL.SDL_Rect(x = rect2Start.x, y = rect2Start.y, w = rect2End.x - rect2Start.x, h = rect2End.y - rect2Start.y)
    |> fillRect renderer
    if SDL.SDL_SetRenderDrawColor (renderer, 0uy, 0uy, 0uy, 0uy) <> 0 then sdlErr ()

    // lines for outline    
    [|rect1Start; rect1End.x @@ rect1Start.y
      rect1End; rect2End.x @@ rect1End.y
      rect2End; rect2Start.x @@ rect2End.y
      rect2Start; rect1Start.x @@ rect2Start.y
      rect1Start|]
    |> drawLines renderer

let drawGui window renderer pianoKeys =
    List.iter (drawPianoKey renderer) pianoKeys

let rectContainsPoint (topLeft, bottomRight) point =
    point.x > topLeft.x && point.x < bottomRight.x
    && point.y > topLeft.y && point.y < bottomRight.y

let updateGui pianoKeys =
    let x, y = ref 0, ref 0
    let leftMouseDown = SDL.SDL_GetMouseState (x, y) &&& SDL.SDL_BUTTON(SDL.SDL_BUTTON_LEFT) <> 0u
    let mousePosition = !x @@ !y
    pianoKeys |> List.map (fun pianoKey ->
        let (rect1, rect2) = pianoKeyBounds pianoKey
        { pianoKey with
            pressed = leftMouseDown && (rectContainsPoint rect1 mousePosition || rectContainsPoint rect2 mousePosition) })

let rec runGuiLoop window renderer gui =
    let events = pollEvents ()
    if events |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_QUIT)
    then ()
    else
        (*
        for event in events do
            if event.``type`` = SDL.SDL_EventType.SDL_MOUSEMOTION then
                printfn "%i %i" event.motion.x event.motion.y *)
        let gui = updateGui gui
        if SDL.SDL_SetRenderDrawColor (renderer, 220uy, 220uy, 230uy, 0uy) <> 0 then sdlErr ()
        if SDL.SDL_RenderClear renderer <> 0 then sdlErr ()
        drawGui window renderer gui
        SDL.SDL_RenderPresent renderer
        
        // delay (so we don't hog the CPU) and repeat gui loop
        Thread.Sleep 20
        runGuiLoop window renderer gui

[<EntryPoint>]
let main argv =
    try
        if SDL.SDL_Init SDL.SDL_INIT_VIDEO <> 0 then sdlErr ()
        let window = SDL.SDL_CreateWindow ("Synth", SDL.SDL_WINDOWPOS_UNDEFINED, SDL.SDL_WINDOWPOS_UNDEFINED, 840, 480, SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE)
        try
            let renderer = SDL.SDL_CreateRenderer (window, -1, SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
            try
                if renderer = IntPtr.Zero then sdlErr ()
                let gui = initGui ()
                runGuiLoop window renderer gui
            finally
                SDL.SDL_DestroyRenderer renderer
        finally
            SDL.SDL_DestroyWindow window
    finally
        SDL.SDL_Quit ()
    0