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
    
    /// Creates a ready-to-use VBO of the outlines of the note widgets
    let createOutlineVAO sequencer =
        let vertices =
            [for note in sequencer.notes do
                let x = float32 note.start
                let y = float32 (Note.noteToKeyIndex note.noteAndOctave)
                let width = float32 note.duration
                let height = 1.f
                // Transform "line strip" data into "lines" data
                for (v1, v2) in List.pairwise [x @@ y; x + width @@ y; x + width @@ y + height; x @@ y + height; x @@ y] do
                    yield v1
                    yield v2]
        let colors = List.init vertices.Length (fun _ -> vec3(0.f, 1.f, 0.))
        VertexArrayObject.fromVerticesAndColors BufferUsageHint.StaticDraw BufferUsageHint.StaticDraw vertices colors