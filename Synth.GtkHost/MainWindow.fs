namespace Synth.GtkHost
open System
open Gtk

type MainWindow() as this =
    inherit Window("MainWindow")

    do
        let mainVbox = new VBox()
        this.Add mainVbox
        let mainHbox = new HBox()
        mainVbox.PackStart (mainHbox, false, false, 50u)
        let keyboard = new MidiKeyboard()
        mainHbox.PackStart (keyboard, false, false, 40u)
        keyboard.NotePressEvent.Add (printfn "start %A")
        keyboard.NoteReleaseEvent.Add (printfn "stop %A")

    do this.SetDefaultSize (400, 300)
    do this.DeleteEvent.AddHandler (fun o e -> this.OnDeleteEvent(o, e))
    do this.ShowAll ()
    
    member this.OnDeleteEvent(o, e:DeleteEventArgs) = 
        Application.Quit ()
        e.RetVal <- true