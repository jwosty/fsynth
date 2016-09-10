open SDL2
open System.Threading

let check x = if x <> 0 then failwith (SDL.SDL_GetError ())

// default enum function is garbage...
let inline enum (value: 'T) : 'Enum = LanguagePrimitives.EnumOfValue value

let rec pollEvents () =
    let mutable event = ref Unchecked.defaultof<_>
    if SDL.SDL_PollEvent event = 0 then [] else !event :: pollEvents ()

let rec runGuiLoop window =
    let events = pollEvents ()
    if events |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_QUIT)
    then ()
    else
        Thread.Sleep 20
        runGuiLoop window

[<EntryPoint>]
let main argv =
    try
        check <| SDL.SDL_Init SDL.SDL_INIT_VIDEO
        let window = SDL.SDL_CreateWindow ("Synth", SDL.SDL_WINDOWPOS_UNDEFINED, SDL.SDL_WINDOWPOS_UNDEFINED, 400, 300, enum 0u)
        SDL.SDL_ShowWindow window
        runGuiLoop window
        SDL.SDL_DestroyWindow window
    finally
        SDL.SDL_Quit ()
    0