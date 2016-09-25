namespace Synth
open Microsoft.FSharp.NativeInterop
open PortAudioSharp
open Synth
open Synth.HelperFunctions
open System

#nowarn "9"

type AudioController(sampleRate, nodesBlueprint: Map<SignalNodeID, SignalNode>, outputNodeId, ?signalAmplitude) =
    let mutable sampleTime = 0u
    let deltaTime = 1. / float sampleRate
    let mutable noteInstances = []
    let oscMonitor = new Object()
    //let mutable outputNodeId = outputNodeId
    let mutable paAudioDevice = None

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
            lock oscMonitor (fun () ->
                let sampleValue = noteInstances |> List.fold (fun acc noteInstance -> NoteInstance.sample outputNodeId noteInstance + acc) 0.
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
    /// Start playing a note
    member this.NoteOn (note, octave) =
        lock oscMonitor (fun () ->
            noteInstances <- { noteAndOctave = (note, octave); nodes = nodesBlueprint; time = 0.; timeSinceRelease = None } :: noteInstances)
    /// Stop playing a note
    member this.NoteOff (note, octave) =
        lock oscMonitor (fun () ->
            noteInstances <- noteInstances |> List.mapFirst (fun noteInstance ->
                if noteInstance.noteAndOctave = (note, octave)
                then Some({ noteInstance with timeSinceRelease = Some(0.) })
                else None))

    interface IDisposable with
        override this.Dispose () =
            this.Stop ()
            paAudioDevice |> Option.iter (fun audio -> audio.Dispose ())