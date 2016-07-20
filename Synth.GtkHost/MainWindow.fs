namespace Synth.GtkHost
open System
open Gtk
open Synth

type MainWindow() as this =
    inherit Window("MainWindow")

    let synthController =
        let oscillator =
            [1, GeneratorNode({ genFunc = Waveform.sin; phase = 0.f }, MidiInput, Constant 0.25f, Constant 0.f)]
            |> Map.ofList
        new AudioController(44100, oscillator, 1)

    do
        let mainVbox = new VBox()
        this.Add mainVbox
        let mainHbox = new HBox()
        mainVbox.PackStart (mainHbox, false, false, 0u)
        let keyboard = new MidiKeyboard()
        mainHbox.PackStart (keyboard, false, false, 0u)
        synthController.Start ()
        keyboard.NotePressEvent.Add (fun note -> synthController.StartNote note)
        keyboard.NoteReleaseEvent.Add (fun note -> synthController.StopNote note)

    do this.SetDefaultSize (400, 300)
    do this.DeleteEvent.AddHandler (fun o e -> this.OnDeleteEvent(o, e))
    do this.ShowAll ()
    
    member this.OnDeleteEvent(o, e:DeleteEventArgs) = 
        Application.Quit ()
        e.RetVal <- true
        synthController.Stop ()