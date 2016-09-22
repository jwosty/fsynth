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

    // There are 3 concepts going on here: "note" means note+octave (e.g. A 4 or C 3), "key index" is one
    // number representing a note on a piano keyboard, and "frequency" is just that (hertz)

    let noteToKeyIndexMapping =
        [C; CS; D; DS; E; F; FS; G; GS; A; AS; B]
        |> List.mapi (fun i n -> n, (i + 4))
        |> Map.ofList
    let keyIndexToNoteMapping =
        [A; AS; B; C; CS; D; DS; E; F; FS; G; GS]
        |> List.mapi (fun i n -> i, n)
        |> Map.ofList
    let noteToKeyIndex (note, octave) = (12 * (octave - 1)) + noteToKeyIndexMapping.[note]
    let keyIndexToNote keyIndex =
        let octave = (keyIndex + 8) / 12
        let note = keyIndexToNoteMapping.[(keyIndex - 1) % 12]
        note, octave
    let keyIndexToFrequency keyIndex =
        2. ** (float (keyIndex - 49) / 12.) * 440.
    let frequencyToKeyIndex frequency =
        let log2 n = log n / log 2.
        (12. * log2 (frequency / 440.)) + 49.
    let noteToFrequency = keyIndexToFrequency << noteToKeyIndex

type GeneratorState =
    { /// A function that creates the core waveform from radians (repeats every 2π)
      genFunc: (float -> float)
      /// Location within the repeating wave function, in radians
      phase: float }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GeneratorState =
    let update deltaTime frequency state = { state with phase = (state.phase + (frequency * deltaTime)) % 1. }
    let sample state = state.genFunc state.phase

type SignalNodeID = int
type SignalParameter =
    /// Parameter is set to a single unchanging value
    | Constant of float
    /// Parameter is controlled by the output of another node
    | Input of SignalNodeID
    /// Parameter is controlled by the frequency of the note that is played
    | MidiInput
type SignalNode =
    | GeneratorNode of
        generator: GeneratorState
        * frequency: SignalParameter * amplitude: SignalParameter * bias: SignalParameter
    | MixerNode of (SignalParameter * SignalParameter) list
    | ADSREnvelopeNode of attack: (float * float) * decay: (float * float) * release: float

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SignalNode =
    let rec sampleParameter midiInputFreq time timeSinceRelease (nodes: Map<_,_>) parameter =
        match parameter with
        | Constant(x) -> x
        | Input(nodeId) -> sample midiInputFreq time timeSinceRelease nodes (nodes.[nodeId])
        | MidiInput -> midiInputFreq
    
    and sample midiInputFreq (time: float) (timeSinceRelease: float option) nodes node =
        match node with
        | GeneratorNode(state, freq, ampl, bias) ->
            let ampl = sampleParameter midiInputFreq time timeSinceRelease nodes ampl
            let bias = sampleParameter midiInputFreq time timeSinceRelease nodes bias
            GeneratorState.sample state * ampl + bias
        | MixerNode(signals) ->
            signals
            |> List.map (fun (signal, gain) ->
                sampleParameter midiInputFreq time timeSinceRelease nodes signal
                * sampleParameter midiInputFreq time timeSinceRelease nodes gain)
            |> List.reduce (+)
        // t = duration, a = amplitude
        | ADSREnvelopeNode((attackDuration, attackAmpl), (decayDuration, decayAmpl), releaseDuration) ->
            // a1 and a2 are the two amplitudes to interpolate between, and x is a value from 0 to 1
            // indicating how far between a1 and a2 to interpolate
            let x, a1, a2 =
                match timeSinceRelease with
                | None ->
                    let tDecay = attackDuration + decayDuration
                    if time < attackDuration then time / attackDuration, 0., attackAmpl
                    elif time < tDecay then (time - attackDuration) / decayDuration, attackAmpl, decayAmpl
                    else 0., decayAmpl, decayAmpl
                | Some(timeSinceRelease) -> timeSinceRelease / releaseDuration, decayAmpl, 0.
            // interpolate
            a1 + (x * (a2 - a1))
    
    let update midiInputFreq deltaTime time timeSinceRelease nodes node =
        match node with
        | GeneratorNode(state, freq, ampl, bias) ->
            GeneratorNode(GeneratorState.update deltaTime (sampleParameter midiInputFreq time timeSinceRelease nodes freq) state, freq, ampl, bias)
        | MixerNode(_) | ADSREnvelopeNode(_) -> node

/// Unit waveforms
module Waveform =
    let sin x = sin (x * 2. * Math.PI)
    let sawtooth x = (x % 1.) * 2. - 1.
    let square x = float (sign (sawtooth x))
    let triangle x = abs (sawtooth x * 2.) - 1.