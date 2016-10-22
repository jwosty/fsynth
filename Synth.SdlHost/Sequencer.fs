namespace Synth.SdlHost
open OpenGL
open SDL2
open Synth
open Synth.SdlHost.HelperFunctions
open System.Diagnostics
open System.Runtime.InteropServices

type SequencerNote =
    { /// The pitch pitch of this note
      noteAndOctave: Note * int
      /// The starting beat of this note
      start: float
      /// The number of beats that this note spans
      duration: float
      /// An integer that can be used to uniquely refer to this note
      id: int }
type Sequencer =
    { notes: SequencerNote list
      bpm: float
      beat: float
      paused: bool }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sequencer =
    let highestNote = B, 5
    
    /// Updates the sequencer, also returning midi events generated and if the governing stopwatch's state should be changed (true = start, false = pause)
    let update (audioController: AudioController) beat (sdlEvents: SDL.SDL_Event list) sequencer =
        let lastBeat = sequencer.beat
        let togglePause = sdlEvents |> List.exists (fun ev -> ev.``type`` = SDL.SDL_EventType.SDL_KEYDOWN && ev.key.keysym.sym = SDL.SDL_Keycode.SDLK_SPACE)
        let paused = if togglePause then not sequencer.paused else sequencer.paused
        let midiEvents =
            [for note in sequencer.notes do
                if note.start >= lastBeat && note.start < beat then
                    yield NoteOn(note.noteAndOctave, note.id)
                let noteStop = note.start + note.duration
                if noteStop >= lastBeat && noteStop < beat then
                    yield NoteOff(note.id)]
        { sequencer with paused = paused; beat = beat }, midiEvents, if togglePause then Some(not paused) else None
    
    /// Returns a list of vertices (clockwise, starting at top-left) representing the bounds of a note widget
    let noteVertices note =
        let x, y = float32 note.start, float32 (Note.noteToKeyIndex note.noteAndOctave)
        let width, height = float32 note.duration, 1.f
        [x @@ y; x + width @@ y; x + width @@ y + height; x @@ y + height; x @@ y]
    
    /// Creates a ready-to-use VBO of the fills of the note widgets
    let createFillVAO sequencer =
        let vertices =
            [for note in sequencer.notes do
                // Tesselate the mesh into triangles
                yield! List.concat (List.windowed 3 (noteVertices note))]
        let colors = List.init vertices.Length (fun _ -> vec3(0, 0.75, 0))
        VertexArrayObject.fromVerticesAndColors BufferUsageHint.StaticDraw BufferUsageHint.StaticDraw BeginMode.Triangles vertices colors
    
    /// Creates a ready-to-use VBO of the outlines of the note widgets
    let createOutlineVAO sequencer =
        let vertices =
            [for note in sequencer.notes do
                // Transform "line strip" data into "lines" data
                for (v1, v2) in List.pairwise (noteVertices note) do
                    yield v1
                    yield v2]
        let colors = List.init vertices.Length (fun _ -> vec3(0, 0.5, 0))
        VertexArrayObject.fromVerticesAndColors BufferUsageHint.StaticDraw BufferUsageHint.StaticDraw BeginMode.Lines vertices colors
    
    let createPlayheadVAO height =
        let vertices = [0 @@ 0; 0 @@ height]
        let colors = List.init vertices.Length (fun _ -> vec3(0.2, 0.25, 0.2))
        VertexArrayObject.fromVerticesAndColors BufferUsageHint.StaticDraw BufferUsageHint.StaticDraw BeginMode.Lines vertices colors