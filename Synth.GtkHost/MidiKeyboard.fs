namespace Synth.GtkHost
open Cairo
open Gdk
open Gtk
open System
open Synth

type KeyState = | Released | Pressed

type MidiKey(note, cutoutWidth1, cutoutWidth2, key) as this =
    inherit DrawingArea()

    do
        this.AddEvents
            (   int EventMask.ButtonPressMask
            ||| int EventMask.ButtonReleaseMask
            ||| int EventMask.EnterNotifyMask
            ||| int EventMask.LeaveNotifyMask
            ||| int EventMask.KeyPressMask
            ||| int EventMask.KeyReleaseMask)
        this.CanFocus <- true

    /// Keyboard as in the typing kind here
    let mutable pressedByKeyboard = false
    let mutable pressedByMouse = false
    let pressEvent = new Event<_>()
    let releaseEvent = new Event<_>()

    member this.Pressed = pressedByKeyboard || pressedByMouse

    static member WhiteKeySize = 25, 100
    static member BlackKeySize = 14, 56

    [<CLIEvent>]
    member this.PressEvent = pressEvent.Publish
    [<CLIEvent>]
    member this.ReleaseEvent = releaseEvent.Publish

    member this.Note = note    
    member this.FillColor =
        match Note.isNatural note, this.Pressed with
        | true, false -> new Cairo.Color(1., 1., 1., 1.)
        | true, true -> new Cairo.Color(0.9, 0.9, 0.9, 1.)
        | false, false -> new Cairo.Color(0.25, 0.25, 0.25, 1.)
        | false, true -> new Cairo.Color(0.15, 0.15, 0.15, 1.)

    override this.OnSizeRequested (requisition : Gtk.Requisition byref) =
        if Note.isNatural note then
            requisition.Width <- fst MidiKey.WhiteKeySize
            requisition.Height <- snd MidiKey.WhiteKeySize
        else
            requisition.Width <- fst MidiKey.BlackKeySize
            requisition.Height <- snd MidiKey.BlackKeySize

    override this.OnExposeEvent (ev: EventExpose) =
        base.OnExposeEvent ev |> ignore        

        use cr = CairoHelper.Create this.GdkWindow

        cr.Translate (0.5, 0.5)

        // Drawing white vs black keys is only different with respect to size and color        
        let width, height = float (this.Allocation.Width - 1), float (this.Allocation.Height - 1)

        let cutoutHeight = float (snd MidiKey.BlackKeySize) - 1.

        // Top left path
        match Option.map float cutoutWidth1 with
        | None -> cr.MoveTo (0., 0.)
        | Some(cutoutWidth) ->
            let cutoutWidth = cutoutWidth - 1.
            cr.MoveTo (0., cutoutHeight)
            cr.LineTo (cutoutWidth, cutoutHeight)
            cr.LineTo (cutoutWidth, 0.)

        // Top right path
        match Option.map float cutoutWidth2 with
        | None -> cr.LineTo (width, 0.)
        | Some(cutoutWidth) ->            
            let cutoutX = width - cutoutWidth + 1.
            cr.LineTo (cutoutX, 0.)
            cr.LineTo (cutoutX, cutoutHeight)
            cr.LineTo (width, cutoutHeight)
        
        // Bottom path
        cr.LineTo (width, height)
        cr.LineTo (0., height)

        // Fill and stroke
        cr.ClosePath ()
        cr.SetSourceColor this.FillColor
        cr.FillPreserve ()
        cr.LineWidth <- 1.
        cr.SetSourceRGB (0., 0., 0.)
        cr.Stroke ()

        true

    member this.Press fromKeyboard =
        if fromKeyboard then pressedByKeyboard <- true else pressedByMouse <- true
        this.QueueDraw ()
        pressEvent.Trigger note

    member this.Release fromKeyboard =
        if fromKeyboard then pressedByKeyboard <- false else pressedByMouse <- false
        this.QueueDraw ()
        releaseEvent.Trigger note

    override this.OnButtonPressEvent ev =
        base.OnButtonPressEvent ev |> ignore
        this.Press false
        true

    override this.OnButtonReleaseEvent ev =
        base.OnButtonReleaseEvent ev |> ignore
        this.Release false
        true

    override this.OnEnterNotifyEvent ev =
        base.OnEnterNotifyEvent ev |> ignore
        if ev.State &&& ModifierType.Button1Mask = ModifierType.Button1Mask then
            this.Press false
        true

    override this.OnLeaveNotifyEvent ev =
        base.OnLeaveNotifyEvent ev |> ignore
        if ev.State &&& ModifierType.Button1Mask = ModifierType.Button1Mask then
            this.Release false
        true

type MidiEvent = | NoteStart of Note | NoteStop of Note

[<System.ComponentModel.ToolboxItem(true)>]
type MidiKeyboard() as this =
    inherit Fixed()

    let notePressEvent = new Event<_>()
    let noteReleaseEvent = new Event<_>()

    do
        // HACK: White/black key widget layering is determined by order of placement in parent widget.
        // Idk if there's really a "right" way to do this in GTK, though.
        let sharps = [CS, 16; DS, 44; FS, 86; GS, 114; AS, 142]

        // Layout white keys
        for (note, x) in [C, 0; D, 24; E, 48; F, 72; G, 96; A, 120; B, 144] do
            // Look for a key that overlaps on the left and determine how much it overlaps
            let leftOverlap =
                sharps |> List.tryFind (fun (note, kx) -> kx < x && kx + fst MidiKey.BlackKeySize > x)
                |> Option.map (fun (note, kx) -> kx + fst MidiKey.BlackKeySize - x)
            // Look for a key that overlaps on the right and determinate how much it overlaps
            let rightOverlap =
                sharps |> List.tryFind (fun (k, kx) -> kx > x && kx < x + fst MidiKey.WhiteKeySize)
                |> Option.map (fun (k, kx) -> x + fst MidiKey.WhiteKeySize - kx)
            let k = new MidiKey(note, leftOverlap, rightOverlap, 'f')
            this.Put (k, x, 0)
            k.PressEvent.Add notePressEvent.Trigger
            k.ReleaseEvent.Add noteReleaseEvent.Trigger
        
        // Layout black keys
        for (note, x) in sharps do
            let k = new MidiKey(note, None, None, 'f')
            this.Put (k, x, 0)
            k.PressEvent.Add notePressEvent.Trigger
            k.ReleaseEvent.Add noteReleaseEvent.Trigger

    [<CLIEvent>]
    member this.NotePressEvent = notePressEvent.Publish
    [<CLIEvent>]
    member this.NoteReleaseEvent = noteReleaseEvent.Publish