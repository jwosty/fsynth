open Microsoft.FSharp.NativeInterop
open SDL2
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
let (@@) x y = { x = x; y = y }
type PianoKey = { natural: bool; position: Vector2<int>; size: Vector2<int>; cutoutWidth1: int; cutoutWidth2: int }

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
    let rect1Start = pianoKey.position.x + pianoKey.cutoutWidth1, pianoKey.position.y
    let rect1End = pianoKey.position.x + pianoKey.size.x - pianoKey.cutoutWidth2, pianoKey.position.y + blackKeySize.y
    let rect2Start = (pianoKey.position.x, pianoKey.position.y + blackKeySize.y)
    let rect2End = pianoKey.position.x + pianoKey.size.x, pianoKey.position.y + pianoKey.size.y
    (rect1Start, rect1End), (rect2Start, rect2End)

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

let drawGui window renderer =
    drawPianoKey renderer { natural = true; position = 50 @@ 20; size = whiteKeySize; cutoutWidth1 = 5; cutoutWidth2 = 3 }

let rec runGuiLoop window renderer =
    let events = pollEvents ()
    if events |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_QUIT)
    then ()
    else
        if SDL.SDL_SetRenderDrawColor (renderer, 220uy, 220uy, 230uy, 0uy) <> 0 then sdlErr ()
        if SDL.SDL_RenderClear renderer <> 0 then sdlErr ()
        drawGui window renderer
        SDL.SDL_RenderPresent renderer
        
        // delay (so we don't hog the CPU) and repeat gui loop
        Thread.Sleep 20
        runGuiLoop window renderer

[<EntryPoint>]
let main argv =
    try
        if SDL.SDL_Init SDL.SDL_INIT_VIDEO <> 0 then sdlErr ()
        let window = SDL.SDL_CreateWindow ("Synth", SDL.SDL_WINDOWPOS_UNDEFINED, SDL.SDL_WINDOWPOS_UNDEFINED, 640, 480, SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE)
        try
            let renderer = SDL.SDL_CreateRenderer (window, -1, SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
            try
                if renderer = IntPtr.Zero then sdlErr ()
                runGuiLoop window renderer
            finally
                SDL.SDL_DestroyRenderer renderer
        finally
            SDL.SDL_DestroyWindow window
    finally
        SDL.SDL_Quit ()
    0