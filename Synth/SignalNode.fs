namespace Synth
open System

type Note =
    | C | CS | D | DS | E | F | FS | G | GS | A | AS | B
    override this.ToString () =
        match this with
        | C -> "C" | CS -> "C#"
        | D -> "D" | DS -> "D#"
        | E -> "E"
        | F -> "F" | FS -> "F#"
        | G -> "G" | GS -> "G#"
        | A -> "A" | AS -> "A#"
        | B -> "B"

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Note =
    let isSharp = function | CS | DS | FS | GS | AS -> true | _ -> false
    let isNatural = isSharp >> not
    let noteKeyIndices =
        [C; CS; D; DS; E; F; FS; G; GS; A; AS; B]
        |> List.mapi (fun i n -> n, (i + 4))
        |> Map.ofList
    let noteOctaveIndex note octave = noteKeyIndices.[note] + (12 * (octave - 1))
    let frequency note octave =
        2. ** (float (noteOctaveIndex note octave - 49) / 12.) * 440. |> float32

type GeneratorState = { genFunc: (float32 -> float32); phase: float32 }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GeneratorState =
    let update deltaTime frequency state = { state with phase = state.phase + (frequency * deltaTime) }
    let sample state = state.genFunc state.phase
                
type SignalNodeID = int
type SignalParameter = | Constant of float32 | Input of SignalNodeID
type SignalNode =
    | GeneratorNode of
        generator: GeneratorState
        * frequency: SignalParameter * amplitude: SignalParameter * bias: SignalParameter
    //| MixerNode of (SignalParameter * SignalParameter) list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SignalNode =
    let rec sampleParameter (nodes: Map<_,_>) parameter =
        match parameter with
        | Constant(x) -> x
        | Input(nodeId) -> sample nodes (nodes.[nodeId])
    and sample nodes node =
        match node with
        | GeneratorNode(state, freq, ampl, bias) ->
            let ampl = sampleParameter nodes ampl
            let bias = sampleParameter nodes bias
            GeneratorState.sample state * ampl + bias
    let update deltaTime nodes node =
        match node with
        | GeneratorNode(state, freq, ampl, bias) ->
            GeneratorNode(GeneratorState.update deltaTime (sampleParameter nodes freq) state, freq, ampl, bias)
    (* /// Samples whatever the connection is directed to at a single point in time
    let rec sampleConnection time connection =
        match connection with
        | Constant(x) -> x
        | Input(node) -> sample time node
    /// Samples the given oscillator at a single point in time
    and sample time node =
        match node with
        | GeneratorNode(f, freq, ampl, bias) ->
            // Bias is the mean value of a signal; can be thought of as the y-offset
            let freq, ampl, bias = sampleConnection time freq, sampleConnection time ampl, sampleConnection time bias
            f (time * freq) * ampl + bias
        | MixerNode(inputs) ->
            inputs
            |> List.map (fun (input, inputAmpl) -> sampleConnection time input * sampleConnection time inputAmpl)
            |> List.sum *)

/// Unit waveforms
module Waveform =
    let sin x = sin (x * 2.f * float32 Math.PI)
    let sawtooth x = (x % 1.f) * 2.f - 1.f
    let square x = float32 (sign (sawtooth x))
    let triangle x = abs (sawtooth x * 2.f) - 1.f