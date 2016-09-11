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

let whiteKeySize = 25, 100
let blackKeySize = 14, 56

let drawPianoKey renderer ((r, g, b), (x, y), (width, height), cutoutWidth1, cutoutWidth2) =
    let rect1Start, rect1End = (x + cutoutWidth1, y), (x + width - cutoutWidth2, y + snd blackKeySize)
    let rect2Start, rect2End = (x, y + snd blackKeySize), (x + width, y + height)
    if SDL.SDL_SetRenderDrawColor (renderer, r, g, b, 0uy) <> 0 then sdlErr ()
    let mutable rect1 = new SDL.SDL_Rect(x = fst rect1Start, y = snd rect1Start, w = fst rect1End - fst rect1Start, h = snd rect1End - snd rect1Start)
    let mutable rect2 = new SDL.SDL_Rect(x = fst rect2Start, y = snd rect2Start, w = fst rect2End - fst rect2Start, h = snd rect2End - snd rect2Start)
    if SDL.SDL_RenderFillRect (renderer, NativePtr.toNativeInt &&rect1) <> 0 then sdlErr ()
    if SDL.SDL_RenderFillRect (renderer, NativePtr.toNativeInt &&rect2) <> 0 then sdlErr ()
    if SDL.SDL_SetRenderDrawColor (renderer, 0uy, 0uy, 0uy, 0uy) <> 0 then sdlErr ()
    let stroke =
        [|rect1Start; fst rect1End, snd rect1Start;
          rect1End; fst rect2End, snd rect1End;
          rect2End; fst rect2Start, snd rect2End;
          rect2Start; fst rect1Start, snd rect2Start;
          rect1Start|]
        |> Array.map (fun (x, y) -> new SDL.SDL_Point(x = x, y = y))
    if SDL.SDL_RenderDrawLines (renderer, stroke, stroke.Length) <> 0 then sdlErr ()

let drawGui window renderer =
    //let mutable rect = new SDL.SDL_Rect(x = 10, y = 10, w = 25, h = 100)
    //let rectPtr = NativePtr.toNativeInt &&rect
    //if SDL.SDL_SetRenderDrawColor (renderer, 255uy, 255uy, 255uy, 0uy) <> 0 then sdlErr ()
    //if SDL.SDL_RenderFillRect (renderer, rectPtr) <> 0 then sdlErr ()
    //if SDL.SDL_SetRenderDrawColor (renderer, 0uy, 0uy, 0uy, 0uy) <> 0 then sdlErr ()
    //if SDL.SDL_RenderDrawRect (renderer, rectPtr) <> 0 then sdlErr ()
    drawPianoKey renderer ((255uy, 255uy, 255uy), (50, 20), whiteKeySize, 5, 3)

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