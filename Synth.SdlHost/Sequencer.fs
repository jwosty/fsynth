namespace Synth.SdlHost
open Microsoft.FSharp.NativeInterop
open OpenGL
open SDL2
open Synth
open Synth.SdlHost.HelperFunctions
open System
open System.Diagnostics
open System.Runtime.InteropServices

#nowarn "9"

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
    static member highestNote = B, 5

type SequencerView(notesFillMesh, notesOutlineMesh, playheadMesh) =
    /// Keeps track of time for live playback
    member val BeatStopwatch = new Stopwatch()
    
    // TODO: Matrices could work in the model instead of the view
    /// Transforms the sequencer widget as a whole
    member val ModelMatrix = Matrix4.CreateTranslation (vec3 (0.f, float32 PianoKey.whiteKeySize.y + 1.f, 0.f)) with get, set
    /// Transforms all of the note widglets
    member val NotesModelMatrix =
        Matrix4.CreateTranslation (vec3 (0.f, -1.f * float32 (Note.noteToKeyIndex Sequencer.highestNote + 1), 0.f))
      * Matrix4.CreateScaling (vec3 (30.f, -10.f, 1.f))
        with get, set
    /// Transforms the playhead
    member val PlayheadModelMatrix = Matrix4.Identity with get, set
    
    member val NotesFillMesh: Mesh = notesFillMesh with get, set
    member val NotesOutlineMesh: Mesh = notesOutlineMesh with get, set
    member val PlayheadMesh: Mesh = playheadMesh with get, set
    
    interface IDisposable with
        override this.Dispose () =
            dispose this.NotesFillMesh
            dispose this.NotesOutlineMesh
            dispose this.PlayheadMesh

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sequencer =
    let noteFillColor = vec3(0, 0.75, 0)
    let noteOutlineColor = vec3(0, 0.5, 0)
    
    /// Updates the sequencer, also returning midi events generated and how the governing stopwatch's state might need to be changed
    let update (audioController: AudioController) (modelSpaceMousePosition: Vector2) beat keyboard (sdlEvents: SDL.SDL_Event list) sequencer =
        let keyDowns = sdlEvents |> List.choose (fun event -> if event.``type`` = SDL.SDL_EventType.SDL_KEYDOWN then Some(event.key) else None)
        let keyUps = sdlEvents |> List.choose (fun event -> if event.``type`` = SDL.SDL_EventType.SDL_KEYUP then Some(event.key) else None)
        
        let lastBeat = sequencer.beat
        let keydowns = sdlEvents |> List.choose (fun event -> if event.``type`` = SDL.SDL_EventType.SDL_KEYDOWN then Some(event.key) else None)
        let togglePause = keydowns |> List.exists (fun key -> key.keysym.sym = SDL.SDL_Keycode.SDLK_SPACE)
        let jumpToStart = keydowns |> List.exists (fun key -> key.keysym.sym = SDL.SDL_Keycode.SDLK_LEFT)
        
        let paused = if togglePause then not sequencer.paused else sequencer.paused
        let playheadAction =
            if togglePause then Some((if paused then Pause else Play))
            elif jumpToStart then Some(JumpToStart)
            else None
        let midiEvents =
            [for note in sequencer.notes do
                // midi note should be started if the note starts, or if this note is at t=0 and the playhead is being reset while playing (that last condition is a bug workaround)
                if note.start >= lastBeat && note.start < beat || (not paused && jumpToStart && note.start = 0.) then
                    yield NoteOn(note.noteAndOctave, note.id)
                let noteStop = note.start + note.duration
                // midi note should be stopped if the note ends, or if the midi note is currently playing and the playead is being reset or paused
                if noteStop >= lastBeat && noteStop < beat || (beat > note.start && beat < noteStop && (playheadAction = Some(JumpToStart) || playheadAction = Some(Pause))) then
                    yield NoteOff(note.id)]
        
        let notes, hadRedraws =
            if keyDowns |> List.exists (fun key -> key.keysym.scancode = SDL.SDL_Scancode.SDL_SCANCODE_DOWN) then
                sequencer.notes |> List.map (fun sequencerNote -> { sequencerNote with noteAndOctave = Note.noteToKeyIndex sequencerNote.noteAndOctave - 1 |> Note.keyIndexToNote }), true
            elif keyUps |> List.exists (fun key -> key.keysym.scancode = SDL.SDL_Scancode.SDL_SCANCODE_UP) then
                sequencer.notes |> List.map (fun sequencerNote -> { sequencerNote with noteAndOctave = Note.noteToKeyIndex sequencerNote.noteAndOctave + 1 |> Note.keyIndexToNote }), true
            else sequencer.notes, false
        let redraws = if hadRedraws then notes else []
        
        // windows/command/meta
        let cmdDown = NativePtr.get keyboard (int SDL.SDL_Scancode.SDL_SCANCODE_LGUI) = 1uy || NativePtr.get keyboard (int SDL.SDL_Scancode.SDL_SCANCODE_RGUI) = 1uy
        let altDown = NativePtr.get keyboard (int SDL.SDL_Scancode.SDL_SCANCODE_LALT) = 1uy || NativePtr.get keyboard (int SDL.SDL_Scancode.SDL_SCANCODE_RALT) = 1uy
        let leftMouseDown = sdlEvents |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN)
        let leftMouseUp = sdlEvents |> List.exists (fun event -> event.``type`` = SDL.SDL_EventType.SDL_MOUSEBUTTONUP)
        
        // Detect note creation
        let notes, redraws =
            if cmdDown && leftMouseDown then
                // obviously not the most efficient, but this isn't gonna be any kind of bottleneck
                let usedIds = sequencer.notes |> List.map (fun note -> note.id) |> set
                let possibleIds = set [0 .. Set.maxElement usedIds + 1]
                let newId = possibleIds - usedIds |> Set.minElement
                let newNote = { noteAndOctave = Note.keyIndexToNote (int modelSpaceMousePosition.y); start = float modelSpaceMousePosition.x; duration = 1.; id = newId }
                newNote :: notes, [newNote]
            elif altDown && leftMouseDown then
                // Delete notes that the mouse is alt-clicking
                notes |> List.filter (fun note -> not (float modelSpaceMousePosition.x > note.start && float modelSpaceMousePosition.x < note.start + note.duration
                                                       && int modelSpaceMousePosition.y = Note.noteToKeyIndex note.noteAndOctave)), redraws
            else notes, redraws
        
        // Detect a note drag start or stop
        let draggedNoteAndOffset =
            if (not (cmdDown || altDown)) && leftMouseDown then
                let time, keyIndex = float modelSpaceMousePosition.x, int modelSpaceMousePosition.y
                notes |> List.tryPick (fun note ->
                    if keyIndex = Note.noteToKeyIndex note.noteAndOctave && time >= note.start && time < (note.start + note.duration)
                    then Some(note.id, float modelSpaceMousePosition.x - float note.start)
                    else None)
            elif (not (cmdDown || altDown)) && leftMouseUp then
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
        [for note in sequencer.notes do
            // Tesselate the mesh into triangles
            let vs = List.windowed 3 (noteVertices note)
            yield! vs.[0]
            yield! vs.[2]]
        |> List.map (fun vertex -> vertex, noteFillColor)
        |> Mesh.create BeginMode.Triangles
    
    /// Creates a ready-to-use VBO of the outlines of the note widgets
    let createOutlineVAO sequencer =
        [for note in sequencer.notes do
            // Transform "line strip" data into "lines" data
            for (v1, v2) in List.pairwise (noteVertices note) do
                yield v1
                yield v2]
        |> List.map (fun vertex -> vertex, noteOutlineColor)
        |> Mesh.create BeginMode.Lines
    
    let createPlayheadVAO height =
        let color = vec3(0.2, 0.25, 0.2)
        Mesh.create BeginMode.Lines [0 @@ 0, color; 0 @@ height, color]
    
    /// Update the sequencer VAOs to reflect a PianoKey state
    let updateVAOs (sequencerView: SequencerView) sequencerNote =
        let vertices = noteVertices sequencerNote
        
        [let vs = List.windowed 3 vertices
         yield! vs.[0]
         yield! vs.[2]]
        |> List.map (fun vertex -> vertex, noteFillColor)
        |> Mesh.updateVertices (sequencerNote.id * 2) sequencerView.NotesFillMesh
        
        [for (v1, v2) in List.pairwise vertices do yield v1; yield v2]
        |> List.map (fun vertex -> vertex, noteOutlineColor)
        |> Mesh.updateVertices (sequencerNote.id * 4) sequencerView.NotesOutlineMesh