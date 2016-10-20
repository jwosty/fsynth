namespace Synth
open Microsoft.FSharp.NativeInterop
open PortAudioSharp
open Synth
open Synth.HelperFunctions
open System

#nowarn "9"

type MidiEvent =
    | NoteOn of noteAndOctave: (Note * int) * id: int
    | NoteOff of id: int

type AudioController(sampleRate, nodesBlueprint: Map<SignalNodeID, SignalNode>, outputNodeId, ?signalAmplitude) =
    let mutable sampleTime = 0u
    let deltaTime = 1. / float sampleRate
    let mutable noteInstances = []
    let notesMonitor = new Object()
    let mutable paAudioDevice = None
    let mutable nextNoteId = 0
    
    member this.SampleRate = sampleRate
    member val OutputAmplitude = defaultArg signalAmplitude 1. with get, set
    
    member this.AudioRequestedCallbackDelegate =
        PortAudio.PaStreamCallbackDelegate(fun (inputBuffer: nativeint)
                                               (outputBuffer: nativeint)
                                               (framesPerBuffer: uint32)
                                               (timeInfo: byref<PortAudio.PaStreamCallbackTimeInfo>)
                                               (statusFlags: PortAudio.PaStreamCallbackFlags)
                                               (userData: nativeint) ->
        // Don't use userData because it seems to cause all sorts of runtime corruption
        let outputBuffer: nativeptr<float32> = NativePtr.ofNativeInt outputBuffer
        
        for i in 0..(int framesPerBuffer - 1) do
            lock notesMonitor (fun () ->
                let sampleValue = List.fold (fun acc noteInstance -> NoteInstance.sample outputNodeId noteInstance + acc) 0. noteInstances
                NativePtr.set outputBuffer i (float32 (sampleValue * this.OutputAmplitude))
                noteInstances <- NoteInstance.updateMany outputNodeId deltaTime noteInstances)
            
            sampleTime <- sampleTime + 1u
        PortAudio.PaStreamCallbackResult.paContinue)
    
    /// Start streaming audio
    member this.Start () =
        let audio =
            match paAudioDevice with
            | Some(audio) -> audio
            | None -> new Audio(1, 1, sampleRate, 1024u, this.AudioRequestedCallbackDelegate)
        paAudioDevice <- Some(audio)
        audio.Start ()
    /// Stop streaming audio
    member this.Stop () = paAudioDevice |> Option.iter (fun audio -> audio.Stop ())
    /// Start playing a note, associating it with an ID by which to refer
    member this.NoteOn ((note, octave), id) =
        lock notesMonitor (fun () ->
            let newNote = { id = id; frequency = Note.noteToFrequency (note, octave); nodes = nodesBlueprint; time = 0.; timeSinceRelease = None }
            noteInstances <- newNote :: noteInstances)
            //nextNoteId <- nextNoteId + 1)
        //nextNoteId - 1
    /// Release a note instance
    member this.NoteOff id =
        lock notesMonitor (fun () ->
            noteInstances <- noteInstances |> List.map (fun noteInstance ->
                if noteInstance.id = id
                then { noteInstance with timeSinceRelease = Some(0.)}
                else noteInstance))
    member this.RecieveEvent midiEvent =
        match midiEvent with
        | NoteOn(noteAndOctave, id) -> this.NoteOn (noteAndOctave, id)
        | NoteOff(id) -> this.NoteOff id
    
    interface IDisposable with
        override this.Dispose () =
            this.Stop ()
            paAudioDevice |> Option.iter (fun audio -> audio.Dispose ())