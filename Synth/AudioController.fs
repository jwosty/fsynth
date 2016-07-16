namespace Synth
open Microsoft.FSharp.NativeInterop
open PortAudioSharp
open Synth
open System

#nowarn "9"

type AudioController(sampleRate, nodes: Map<_,_>, outputNodeId, ?signalAmplitude) =
    let mutable sampleTime = 0u
    let deltaTime = 1.f / float32 sampleRate
    let pi = float32 Math.PI
    let mutable nodes = nodes
    let mutable outputNodeId = outputNodeId

    member this.PaAudio = new Audio(1, 1, sampleRate, 64u, this.AudioRequestedCallbackDelegate)
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
        //let mutable time = float32 sampleTime / float32 sampleRate
        
        for i in 0..(int framesPerBuffer - 1) do
            // Time in seconds
            //let time = float32 sampleTime / float32 this.SampleRate
            // Sum oscillators signals and ensure the output stays in bounds
            //let sample = (SignalSource.sample time sourceNode) |> min 1.f |> max -1.f
            let sample = nodes.[outputNodeId] |> SignalNode.sample nodes
            NativePtr.set outputBuffer i (sample * this.OutputAmplitude)
            nodes <- nodes |> Map.map (fun nodeId node -> SignalNode.update deltaTime nodes node)
            sampleTime <- sampleTime + 1u
        PortAudio.PaStreamCallbackResult.paContinue)

    member this.Start () = this.PaAudio.Start ()
    member this.Stop () = this.PaAudio.Stop ()

    interface IDisposable with
        override this.Dispose () = this.PaAudio.Dispose ()