namespace Synth.GtkHost
open System
open Gtk

module Main =
    [<EntryPoint>]
    let main argv = 
        Application.Init()
        use win = new MainWindow()
        win.Show()
        Application.Run()
        0