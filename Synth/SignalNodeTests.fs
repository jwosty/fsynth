module Synth.Tests.SignalNode

open FsUnit
open NUnit.Framework
open Synth
open Synth.SignalNode

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
//let [<Test>] ``Octave order is CDEFGAB`` () =
//    let c3Index = 