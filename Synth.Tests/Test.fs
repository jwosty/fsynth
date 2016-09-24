namespace Synth.Tests
open FsUnit
open NUnit.Framework
open Synth
open Synth.SignalNode
open System

module Note =
    let [<Test>] helloTest () = 2 + 3 |> should equal 5

    let [<Test>] ``notes have names`` () = string Note.CS |> should equal "C#"
    let [<Test>] ``some notes are sharp`` () = Note.isSharp Note.CS |> should be True
    let [<Test>] ``sharps are not natural`` () = Note.isNatural Note.CS |> should be False
    let [<Test>] ``some notes are natural`` () = Note.isNatural Note.C |> should be True
    let [<Test>] ``naturals are not sharp`` () = Note.isSharp Note.C |> should be False

    let [<Test>] ``B3 is the 39th note on the piano keyboard`` () = Note.noteToKeyIndex (Note.B, 3) |> should equal 39
    let [<Test>] ``B4 is the 51th note on the piano keyboard`` () = Note.noteToKeyIndex (Note.B, 4) |> should equal 51
    let [<Test>] ``C5 is adjacent to B4`` () = Note.noteToKeyIndex (Note.C, 5) |> should equal 52
    let [<Test>] ``Note -> index -> that note`` () = (C, 4) |> Note.noteToKeyIndex |> Note.keyIndexToNote |> should equal (C, 4)
    let [<Test>] ``A4 is 440 hz`` () = Note.noteToFrequency (Note.A, 4) |> should equal 440.
    let [<Test>] ``A3 is 220 hz`` () = Note.noteToFrequency (Note.A, 3) |> should equal 220.

module Waveform =
    let [<Test>] ``Sine waveform is a unit waveform`` () =
        Waveform.sin 0. |> should (equalWithin 1e-10) 0.
        Waveform.sin 0.25 |> should greaterThan 0.
        Waveform.sin 0.5 |> should (equalWithin 1e-10) 0.
        Waveform.sin 0.75 |> should lessThan 0.
        Waveform.sin 1. |> should (equalWithin 1e-10) 0.
    
    let [<Test>] ``Sawtooth waveform is a unit waveform`` () =
        Waveform.sawtooth 0. |> should equal -1.
        Waveform.sawtooth 0.5 |> should equal 0.
        Waveform.sawtooth 1. |> should equal -1.
    
    let [<Test>] ``Square waveform is a unit waveform`` () =
        Waveform.square 0. |> should equal -1.
        Waveform.square 0.6 |> should equal 1.
        Waveform.square 1. |> should equal -1.
    
    let [<Test>] ``Square waveform erroneously has a value of 0. at x=0.5`` () = Waveform.square 0.5 |> should (equalWithin 1e-10) 0.
    
    let [<Test>] ``Triangle waveform is a unit waveform`` () =
        Waveform.triangle 0. |> should equal 1.
        Waveform.triangle 0.5 |> should equal -1.
        Waveform.triangle 1. |> should equal 1.

module SignalNode =
    let [<Test>] ``Simple square node's output is -1 at t=0`` () =
        let node = GeneratorNode({ genFunc = Waveform.square; phase = 0. }, Constant 440., Constant 1., Constant 0.)
        SignalNode.sample 123. 456. None Map.empty node |> should equal -1.0