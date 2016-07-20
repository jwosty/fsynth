namespace Synth
open Microsoft.FSharp.NativeInterop
open PortAudioSharp
open Synth
open System

#nowarn "9"

type AudioController(sampleRate, oscillatorBlueprint: Map<_,_>, outputNodeId, ?signalAmplitude) as this =
    let mutable sampleTime = 0u
    let deltaTime = 1.f / float32 sampleRate
    let pi = float32 Math.PI
    /// Each note that is playing gets its own oscillator, which in turn is the collection of nodes that build it up
    let mutable oscillatorInstances = Map.empty
    let mutable outputNodeId = outputNodeId
    let mutable paAudioDevice = None
    
    //member val PaAudioDevice = new Audio(1, 1, sampleRate, 64u, this.AudioRequestedCallbackDelegate)
    member this.SampleRate = sampleRate
    member val OutputAmplitude = defaultArg signalAmplitude 1.f with get, set

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
            //let sample = nodes.[outputNodeId] |> SignalNode.sample nodes
            //nodes <- nodes |> Map.map (fun nodeId node -> SignalNode.update deltaTime nodes node)
            // Sample active oscillator instances 
            let sampleValue =
                oscillatorInstances
                |> Map.map (fun (note, octave) oscillatorNodes ->
                    SignalNode.sample (Note.frequency note octave) oscillatorNodes oscillatorNodes.[outputNodeId])
                |> Map.fold (fun acc _ sample -> acc + sample) 0.f
            NativePtr.set outputBuffer i (sampleValue * this.OutputAmplitude)
            // Move oscillators forward in time
            oscillatorInstances <- oscillatorInstances |> Map.map (fun (note, octave) oscillatorNodes ->
                oscillatorNodes |> Map.map (fun id oscillatorNode ->
                    SignalNode.update (Note.frequency note octave) deltaTime oscillatorNodes oscillatorNode))
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
    member this.StartNote (note, octave) =
        oscillatorInstances <- oscillatorInstances |> Map.add (note, octave) oscillatorBlueprint
    /// Stop playing a note
    member this.StopNote (note, octave) =
        oscillatorInstances <- oscillatorInstances |> Map.remove (note, octave)

    interface IDisposable with
        override this.Dispose () =
            this.Stop ()
            paAudioDevice |> Option.iter (fun audio -> audio.Dispose ())