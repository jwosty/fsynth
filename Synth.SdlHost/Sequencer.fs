namespace Synth.SdlHost
open OpenGL
open SDL2
open Synth
open Synth.SdlHost.HelperFunctions
open System
open System.Diagnostics
open System.Runtime.InteropServices

/// An action that the playhead takes (used to synchronize the sequencer's clock with its model)
type PlayheadAction = | Play | Pause | JumpToStart

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
      paused: bool
      draggedNoteAndOffset: (int * float) option }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sequencer =
    let highestNote = B, 5
    
    /// Updates the sequencer, also returning midi events generated and how the governing stopwatch's state might need to be changed
    let update (audioController: AudioController) (modelSpaceMousePosition: Vector2) beat (sdlEvents: SDL.SDL_Event list) sequencer =
        let lastBeat = sequencer.beat
        let keydowns = sdlEvents |> List.choose (fun event -> if event.``type`` = SDL.SDL_EventType.SDL_KEYDOWN then Some(event.key) else None)
        let togglePause = keydowns |> List.exists (fun key -> key.keysym.sym = SDL.SDL_Keycode.SDLK_SPACE)
        let jumpToStart = keydowns |> List.exists (fun key -> key.keysym.sym = SDL.SDL_Keycode.SDLK_LEFT)
        
        let paused = if togglePause then not sequencer.paused else sequencer.paused
        let midiEvents =
            [for note in sequencer.notes do
                // midi note should be started if the note starts, or if this note is at t=0 and the playhead is being reset while playing (that last condition is a bug workaround)
                if note.start >= lastBeat && note.start < beat || (not paused && jumpToStart && note.start = 0.) then
                    yield NoteOn(note.noteAndOctave, note.id)
                let noteStop = note.start + note.duration
                // midi note should be stopped if the note ends, or if the midi note is currently playing and the playead is being reset
                if noteStop >= lastBeat && noteStop < beat || (beat > note.start && beat < noteStop && jumpToStart) then
                    yield NoteOff(note.id)]
        let playheadAction =
            if togglePause then Some((if paused then Pause else Play))
            elif jumpToStart then Some(JumpToStart)
            else None
        
        let notes, hadRedraws =
            if sdlEvents |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_KEYDOWN && event.key.keysym.scancode = SDL.SDL_Scancode.SDL_SCANCODE_DOWN) then
                sequencer.notes |> List.map (fun sequencerNote -> { sequencerNote with noteAndOctave = Note.noteToKeyIndex sequencerNote.noteAndOctave - 1 |> Note.keyIndexToNote }), true
            elif sdlEvents |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_KEYDOWN && event.key.keysym.scancode = SDL.SDL_Scancode.SDL_SCANCODE_UP) then
                sequencer.notes |> List.map (fun sequencerNote -> { sequencerNote with noteAndOctave = Note.noteToKeyIndex sequencerNote.noteAndOctave + 1 |> Note.keyIndexToNote }), true
            else sequencer.notes, false
        let redraws = if hadRedraws then notes else []
        
        // Detect a note drag start or stop
        let draggedNoteAndOffset =
            if sdlEvents |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN) then
                let time, keyIndex = float modelSpaceMousePosition.x, int modelSpaceMousePosition.y
                notes |> List.tryPick (fun note ->
                    if keyIndex = Note.noteToKeyIndex note.noteAndOctave && time >= note.start && time < (note.start + note.duration)
                    then Some(note.id, float modelSpaceMousePosition.x - float note.start)
                    else None)
            elif sdlEvents |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_MOUSEBUTTONUP) then
                None
            else sequencer.draggedNoteAndOffset
        
        // React to note drag
        let notes, redraws =
            match draggedNoteAndOffset with
            | Some(id, offset) ->
                List.fold (fun (notes, redraws) note ->
                    if note.id = id then
                        let note =
                            { note with noteAndOctave = Note.keyIndexToNote (int modelSpaceMousePosition.y)
                                        start = float modelSpaceMousePosition.x - offset }
                        note :: notes, note :: redraws
                    else note :: notes, note :: redraws)
                    ([], redraws)
                    sequencer.notes
            | None -> notes, redraws
        
        { sequencer with notes = notes; paused = paused; beat = beat; draggedNoteAndOffset = draggedNoteAndOffset }, midiEvents, playheadAction, redraws
    
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
                // TODO: Get rid of redundant middle triangle that is redundant; this produces 3 triangles where you only actually need 2
                yield! List.concat (List.windowed 3 (noteVertices note))]
        let colors = List.init vertices.Length (fun _ -> vec3(0, 0.75, 0))
        Mesh.create BufferUsageHint.StaticDraw BufferUsageHint.StaticDraw BeginMode.Triangles vertices colors
    
    /// Creates a ready-to-use VBO of the outlines of the note widgets
    let createOutlineVAO sequencer =
        let vertices =
            [for note in sequencer.notes do
                // Transform "line strip" data into "lines" data
                for (v1, v2) in List.pairwise (noteVertices note) do
                    yield v1
                    yield v2]
        let colors = List.init vertices.Length (fun _ -> vec3(0, 0.5, 0))
        Mesh.create BufferUsageHint.StaticDraw BufferUsageHint.StaticDraw BeginMode.Lines vertices colors
    
    let createPlayheadVAO height =
        let vertices = [0 @@ 0; 0 @@ height]
        let colors = List.init vertices.Length (fun _ -> vec3(0.2, 0.25, 0.2))
        Mesh.create BufferUsageHint.StaticDraw BufferUsageHint.StaticDraw BeginMode.Lines vertices colors
    
    /// Update the sequencer VAOs to reflect a PianoKey state
    let updateVAOs (sequencerNotesOutlineMesh: Mesh) (sequencerNotesFillMesh: Mesh) sequencerNote =
        let vertices = noteVertices sequencerNote
        
        Gl.BindBuffer (BufferTarget.ArrayBuffer, sequencerNotesOutlineMesh.VertexVBO)
        let vs = [for (v1, v2) in List.pairwise vertices do yield v1; yield v2]
        submitVec2Data (sequencerNote.id * 8) [for (v1, v2) in List.pairwise vertices do yield v1; yield v2]
        
        Gl.BindBuffer (BufferTarget.ArrayBuffer, sequencerNotesFillMesh.VertexVBO)
        let verts = (List.concat (List.windowed 3 (noteVertices sequencerNote)))
        submitVec2Data (sequencerNote.id * 9) (List.concat (List.windowed 3 vertices))