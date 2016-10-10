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
        let vao = Gl.GenVertexArray ()
        Gl.BindVertexArray vao
        
        let vertices =
            [|for note in sequencer.notes do//[0.f, 53.f, 2.f;    1.f, 55.f, 1.f;    1.f, 0.f, 10.f] do// in sequencer.notes.[0..0] do
                let x = float32 note.start
                let y = float32 (Note.noteToKeyIndex note.noteAndOctave)
                let width = float32 note.duration
                let height = 1.f
                for (v1, v2) in [|x, y; x + width, y; x + width, y + height; x, y + height; x, y|] |> Array.pairwise do
                    yield fst v1; yield snd v1
                    yield fst v2; yield snd v2|]
        //let vertices = [|0.f; 0.f;  10.f; 0.f;  10.f; 10.f;  0.f; 10.f;  0.f; 0.f|]
        let vertexBuffer = Gl.GenBuffer ()
        Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
        Gl.BufferData (BufferTarget.ArrayBuffer, vertices.Length * sizeof<float32>, vertices, BufferUsageHint.StaticDraw)
        Gl.EnableVertexAttribArray 0
        // vertex is parameter index 0 in shader
        Gl.VertexAttribPointer (0, 2, VertexAttribPointerType.Float, false, 0, 0n)
        
        let outlineColors =
            [|for note in sequencer.notes do
                  for i in 0..10 do
                      yield 0.f; yield 0.6f; yield 0.f|]
        let outlineColorBuffer = Gl.GenBuffer ()
        Gl.BindBuffer (BufferTarget.ArrayBuffer, outlineColorBuffer)
        Gl.BufferData (BufferTarget.ArrayBuffer, outlineColors.Length * sizeof<float32>, outlineColors, BufferUsageHint.StaticDraw)
        Gl.EnableVertexAttribArray 1
        // inColor is parameter index 1 in shader
        Gl.VertexAttribPointer (1, 3, VertexAttribPointerType.Float, false, 0, 0n)
        
        new VertexArrayObject(vao, vertices.Length / 2, vbos = [vertexBuffer; outlineColorBuffer])