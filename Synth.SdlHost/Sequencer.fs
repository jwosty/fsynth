namespace Synth.SdlHost
open OpenGL
open SDL2
open Synth
open Synth.SdlHost.HelperFunctions
open System.Diagnostics
open System.Runtime.InteropServices

type SequencerNote =
    { noteAndOctave: Note * int
      /// The starting beat of this note
      start: float
      /// The number of beats that this note spans
      duration: float
      /// If this note is being playing, this contains the ID of the AudioController voice corresponding to this note
      id: int option }
type Sequencer = { notes: SequencerNote list; bpm: float }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sequencer =
    let highestNote = B, 5
    
    /// Steps the sequencer forward in time and sends MIDI commands to an audio controller
    // TODO: make this functional by delegating the actual note starting/stopping to the GUI object that calls this
    let update lastBeat beat (audioController: AudioController) sequencer =
        { sequencer with
            notes =
                sequencer.notes |> List.map (fun note ->
                    match note.id with
                    | None ->
                        // Note is not playing; check if we need to start it
                        if note.start >= lastBeat && note.start < beat
                        then { note with id = Some(audioController.NoteOn note.noteAndOctave) }
                        else note
                    | Some(id) ->
                        let noteStop = note.start + note.duration
                        // Note is playing; check if we need to stop it
                        if noteStop >= lastBeat && noteStop < beat then
                            audioController.NoteOff id
                            { note with id = None }
                        else note) }
    
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
        VertexArrayObject.fromVerticesAndColors BufferUsageHint.StaticDraw BufferUsageHint.StaticDraw vertices colors
    
    /// Creates a ready-to-use VBO of the outlines of the note widgets
    let createOutlineVAO sequencer =
        let vertices =
            [for note in sequencer.notes do
                // Transform "line strip" data into "lines" data
                for (v1, v2) in List.pairwise (noteVertices note) do
                    yield v1
                    yield v2]
        let colors = List.init vertices.Length (fun _ -> vec3(0, 0.5, 0))
        VertexArrayObject.fromVerticesAndColors BufferUsageHint.StaticDraw BufferUsageHint.StaticDraw vertices colors