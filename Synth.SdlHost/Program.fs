open Microsoft.FSharp.NativeInterop
open SDL2
open Synth
open System
open System.Threading

// suppress warnings from use of pointers generating potentially unverifiable code
#nowarn "51"
#nowarn "9"

let sdlErr () = failwith (SDL.SDL_GetError ())

// default enum function is garbage...
let inline enum (value: 'T) : 'Enum = LanguagePrimitives.EnumOfValue value

let rec pollEvents () =
    let mutable event = ref Unchecked.defaultof<_>
    if SDL.SDL_PollEvent event = 0 then [] else !event :: pollEvents ()

type Vector2<'t> =
    { x: 't; y: 't }
    static member ( + ) (v1, v2) = { x = v1.x + v2.x; y = v1.y + v2.y }
    static member ( - ) (v1, v2) = { x = v1.x - v2.x; y = v1.y - v2.y }
    static member ( ~-) v = { x = -v.x; y = -v.y }
type PianoKey = {
    noteAndOctave: Note * int
    natural: bool
    position: Vector2<int>
    cutoutWidth1: int; cutoutWidth2: int }

let (@@) x y = { x = x; y = y }

let whiteKeySize = 25 @@ 100
let blackKeySize = 14 @@ 56

let drawLines renderer lines =
    let lines = lines |> Array.map (fun (x, y) -> new SDL.SDL_Point(x = x, y = y))
    if SDL.SDL_RenderDrawLines (renderer, lines, lines.Length) <> 0 then sdlErr ()

let fillRect renderer rect =
    let mutable rect = rect
    if SDL.SDL_RenderFillRect (renderer, NativePtr.toNativeInt &&rect) <> 0 then sdlErr ()

/// Calculates two bounding boxes that encompass a piano key
let pianoKeyDimensions pianoKey =
    let size = if pianoKey.natural then whiteKeySize else blackKeySize
    let rect1Start = pianoKey.position.x + pianoKey.cutoutWidth1, pianoKey.position.y
    let rect1End = pianoKey.position.x + size.x - pianoKey.cutoutWidth2, pianoKey.position.y + blackKeySize.y
    let rect2Start = (pianoKey.position.x, pianoKey.position.y + blackKeySize.y)
    let rect2End = pianoKey.position.x + size.x, pianoKey.position.y + size.y
    (rect1Start, rect1End), (rect2Start, rect2End)

let initGui () =
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

            yield { noteAndOctave = note, octave; natural = true
                    position = (keyboardOctaveStart + x @@ 0) + pianoKeyboardPosition
                    cutoutWidth1 = leftOverlap; cutoutWidth2 = rightOverlap }
            
        // black keys
        for (note, x) in sharps do
            yield { noteAndOctave = note, octave; natural = false;
                    position = (keyboardOctaveStart + x @@ 0) + pianoKeyboardPosition
                    cutoutWidth1 = 0; cutoutWidth2 = 0 }]

let drawPianoKey renderer pianoKey =
    // rect1 and rect2 are the top (variably sized) and lower (uniform sized) half of the piano key, respectively
    let (rect1Start, rect1End), (rect2Start, rect2End) = pianoKeyDimensions pianoKey
    let fill = if pianoKey.natural then 255uy else 64uy

    // rectangles for fill
    if SDL.SDL_SetRenderDrawColor (renderer, fill, fill, fill, 0uy) <> 0 then sdlErr ()
    new SDL.SDL_Rect(x = fst rect1Start, y = snd rect1Start, w = fst rect1End - fst rect1Start, h = snd rect1End - snd rect1Start)
    |> fillRect renderer
    new SDL.SDL_Rect(x = fst rect2Start, y = snd rect2Start, w = fst rect2End - fst rect2Start, h = snd rect2End - snd rect2Start)
    |> fillRect renderer
    if SDL.SDL_SetRenderDrawColor (renderer, 0uy, 0uy, 0uy, 0uy) <> 0 then sdlErr ()

    // lines for outline    
    [|rect1Start; fst rect1End, snd rect1Start;
      rect1End; fst rect2End, snd rect1End;
      rect2End; fst rect2Start, snd rect2End;
      rect2Start; fst rect1Start, snd rect2Start;
      rect1Start|]
    |> drawLines renderer

let drawGui window renderer pianoKeys =
    List.iter (drawPianoKey renderer) pianoKeys

let rec runGuiLoop window renderer gui =
    let events = pollEvents ()
    if events |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_QUIT)
    then ()
    else
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