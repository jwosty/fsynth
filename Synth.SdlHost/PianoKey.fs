namespace Synth.SdlHost
open SDL2
open Synth
open Synth.SdlHost.HelperFunctions

type PianoKey = {
    noteAndOctave: Note * int
    position: Vector2<int>
    natural: bool
    pressed: bool
    cutoutWidth1: int; cutoutWidth2: int }

type MidiEvent = | NoteOn of Note * int | NoteOff of Note * int

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PianoKey =
    let whiteKeySize = 25 @@ 100
    let blackKeySize = 14 @@ 56
    
    /// Calculates two bounding boxes that encompass a piano key
    let bounds pianoKey =
        let size = if pianoKey.natural then whiteKeySize else blackKeySize
        let rect1Start = pianoKey.position.x + pianoKey.cutoutWidth1 @@ pianoKey.position.y
        let rect1End = pianoKey.position.x + size.x - pianoKey.cutoutWidth2 @@ pianoKey.position.y + blackKeySize.y
        let rect2Start = pianoKey.position.x @@ pianoKey.position.y + blackKeySize.y
        let rect2End = pianoKey.position.x + size.x@@ pianoKey.position.y + size.y
        (rect1Start, rect1End), (rect2Start, rect2End)
    
    let update (mousePosition, leftMouseDown) pianoKey =
        let (rect1, rect2) = bounds pianoKey
        let pressed' = leftMouseDown && (rectContainsPoint rect1 mousePosition || rectContainsPoint rect2 mousePosition)
        let events =
            if pressed' = pianoKey.pressed
            then []
            else [(if pressed' then NoteOn else NoteOff) pianoKey.noteAndOctave]
        { pianoKey with pressed = pressed' }, events
    
    let draw renderer pianoKey =
        // rect1 and rect2 are the top (variably sized) and lower (uniform sized) half of the piano key, respectively
        let (rect1Start, rect1End), (rect2Start, rect2End) = bounds pianoKey    
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