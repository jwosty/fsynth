module Synth.ConsoleHost.Main
open System
open PortAudioSharp
open Synth

[<EntryPoint>]
let main argv =
    let nodes =
        [1, GeneratorNode({ genFunc = Waveform.square; phase = 0.f }, Constant (Note.frequency E 5), Constant 400.f, Constant (Note.frequency C 4))
         2, GeneratorNode({ genFunc = Waveform.sin; phase = 0.f }, Input 1, Constant 0.75f, Constant 0.f)]
        |> Map.ofList
    use a = new AudioController(44100, nodes, 2, 0.75f)

    a.Start ()

    PortAudio.Pa_Sleep 5000

    a.Stop ()
    0