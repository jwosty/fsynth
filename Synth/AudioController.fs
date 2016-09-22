namespace Synth
open Microsoft.FSharp.NativeInterop
open PortAudioSharp
open Synth
open System

#nowarn "9"

type AudioController(sampleRate, oscillatorBlueprint: Map<int,_>, outputNodeId, ?signalAmplitude) =
    let mutable sampleTime = 0u
    let deltaTime = 1. / float sampleRate
    /// Each note that is playing gets its own oscillator, which in turn is the collection of nodes that build it up
    let mutable oscillatorInstances = Map.empty
    let oscMonitor = new Object()
    let mutable outputNodeId = outputNodeId
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
        // Don't use userData because it seems to causes all sorts of runtime corruption
        let outputBuffer: nativeptr<float32> = NativePtr.ofNativeInt outputBuffer
        
        for i in 0..(int framesPerBuffer - 1) do
            lock oscMonitor (fun () ->
                // Sample active oscillator instances 
                let sampleValue =
                    oscillatorInstances
                    |> Map.map (fun (note, octave) (oscillatorNodes, time, timeSinceRelease) ->
                        SignalNode.sample (Note.noteToFrequency (note, octave)) time timeSinceRelease oscillatorNodes oscillatorNodes.[outputNodeId])
                    |> Map.fold (fun acc _ sample -> acc + sample) 0.
                NativePtr.set outputBuffer i (float32 (sampleValue * this.OutputAmplitude))
                // Move oscillators forward in time
                // TODO: refactor oscillatorNodes vars into another type (something like NoteInstance)
                oscillatorInstances <-
                    oscillatorInstances
                    |> Map.map (fun (note, octave) (oscillatorNodes, time, timeSinceRelease) ->
                        let oscillatorNodes =
                            oscillatorNodes |> Map.map (fun id oscillatorNode ->
                                let oscillatorNode = SignalNode.update (Note.noteToFrequency (note, octave)) deltaTime time timeSinceRelease oscillatorNodes oscillatorNode
                                oscillatorNode)
                        // there is only one time/timeSinceRelease per oscillator instance -- note the difference between nodes and instances!!
                        oscillatorNodes, time + deltaTime, Option.map ((+) deltaTime) timeSinceRelease)
                    // cull notes that are completely off (past the release duration)
                    |> Map.filter (fun (note, octave) (oscillatorNodes, time, timeSinceRelease) ->
                        match timeSinceRelease with
                        // a note is off if it's tSinceRelease value is >= all the longest ADSR envelope release value (or zero, if no ADSRs present)
                        | Some(timeSinceRelease) ->
                            let longestRelease =
                                oscillatorNodes
                                |> Map.toList |> List.choose (fun (nodeId, oscillatorNode) ->
                                    match oscillatorNode with
                                    | ADSREnvelopeNode(_, _, _, release) -> Some(release)
                                    | _ -> Some(0.))
                                |> List.max
                            timeSinceRelease < longestRelease
                        | None -> true))

            sampleTime <- sampleTime + 1u
        PortAudio.PaStreamCallbackResult.paContinue)

    /// Start streaming audio
    member this.Start () =
        let audio =
            match paAudioDevice with
            | Some(audio) -> audio
            | None -> new Audio(1, 1, sampleRate, 64u, this.AudioRequestedCallbackDelegate)
        paAudioDevice <- Some(audio)
        audio.Start ()
    /// Stop streaming audio
    member this.Stop () = paAudioDevice |> Option.iter (fun audio -> audio.Stop ())
    /// Start playing a note
    member this.NoteOn (note, octave) =
        lock oscMonitor (fun () ->
            oscillatorInstances <- oscillatorInstances |> Map.add (note, octave) (oscillatorBlueprint, 0., None))
    /// Stop playing a note
    member this.NoteOff (note, octave) =
        lock oscMonitor (fun () ->
            match Map.tryFind (note, octave) oscillatorInstances with
            | Some(oscillatorInstance, time, timeSinceRelease) ->
                // don't remove the instance; just send it into the release phase
                let timeSinceRelease = match timeSinceRelease with | Some(t) -> Some(t) | None -> Some(0.)
                oscillatorInstances <- oscillatorInstances |> Map.add (note, octave) (oscillatorInstance, time, timeSinceRelease)
            | None -> ())

    interface IDisposable with
        override this.Dispose () =
            this.Stop ()
            paAudioDevice |> Option.iter (fun audio -> audio.Dispose ())