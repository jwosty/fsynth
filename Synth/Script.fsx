#load "SignalNode.fs"
open Synth
open System

// 1. consider the three numeric constants as a record struct to improve readability
// 2. consider rename phase->timeT for clarity

let node = GeneratorNode({ genFunc = Waveform.square; phase = 0. }, Constant 440., Constant 1., Constant 0.)

// "time" is since the beginning of the note
SignalNode.sample 123. 456. None Map.empty node

[for i in 0. .. 0.01 .. 1. -> i, Waveform.square (i * 2. * Math.PI)] |> List.iter (fun (x, y) -> printfn "%f\t%f" x y)
[for i in 0. .. 0.1 .. 1. -> i, Waveform.triangle i] |> List.iter (fun (x, y) -> printfn "%f\t%f" x y)

//let node' = SignalNode.update 123. 456.

#r "../PortAudioSharp.dll"
#load "AudioController.fs"

(*
#r "/Users/jwostenberg/Code/FSharp/Synth/Synth/../packages/MathNet.Numerics.3.11.1/lib/net40/MathNet.Numerics.dll"
#load "../packages/FSharp.Charting.Gtk.0.90.14/FSharp.Charting.Gtk.fsx"
#load "SignalNode.fs"
open FSharp.Charting
open MathNet.Numerics.IntegralTransforms
open Synth
open System
open System.Numerics

let fft data =
    let result = [| for real in data -> new Complex(real, 0.) |]
    Fourier.Forward (result, FourierOptions.Default)
    result |> Seq.take (result.Length / 2) |> Seq.map (fun c -> c.Magnitude)

let ifft data =
    let result = [| for real in data -> new Complex(real, 0.) |]
    Fourier.Inverse (result, FourierOptions.Default)
    result |> Seq.map (fun c -> c.Magnitude)

let sine freq t = sin (t * freq * Math.PI * 2.)
let sawtooth freq t : float = ((t * freq) % 1.) * 2. - 1.
let square x t = float (sign (sawtooth x t))
let triangle x t = abs (sawtooth x t * 2.) - 1.

let sampleRate = 1024.
let samples =
    [ for sample in 0. .. (sampleRate - 1.) ->
        let t = sample / sampleRate
        square 5. t ]
Chart.Line samples
Chart.Line (fft samples)

let freqs = fft samples |> Seq.toList |> List.mapi (fun hz weight -> hz, weight)

let samples2 =
    [ for sample in 0. .. (sampleRate - 1.) ->
        let t = sample / sampleRate
        freqs |> List.map (fun (hz, weight) -> (sine (float hz) t) * weight) |> List.sum ]
Chart.Line samples2
Chart.Line (fft samples)


let data = [|1.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|] |> Array.map (fun x -> new Complex(x, 0.))
Fourier.Forward data
Fourier.Inverse data
data |> Array.map (fun c -> c.Real, c.Imaginary)
*)