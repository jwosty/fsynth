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

let drawGui window renderer =
    let mutable rect = new SDL.SDL_Rect(x = 10, y = 10, w = 25, h = 100)
    let rectPtr = NativePtr.toNativeInt &&rect
    if SDL.SDL_SetRenderDrawColor (renderer, 255uy, 255uy, 255uy, 0uy) <> 0 then sdlErr ()
    if SDL.SDL_RenderFillRect (renderer, rectPtr) <> 0 then sdlErr ()
    if SDL.SDL_SetRenderDrawColor (renderer, 0uy, 0uy, 0uy, 0uy) <> 0 then sdlErr ()
    if SDL.SDL_RenderDrawRect (renderer, rectPtr) <> 0 then sdlErr ()

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