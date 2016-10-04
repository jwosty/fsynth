namespace Synth.SdlHost.Widget
open Synth
open Synth.HelperFunctions
open OpenGL

type SignalNode = { position: Synth.SdlHost.Vector2<float32> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SignalNode =
    let createFillVAO ()  =
        let vao = Gl.GenVertexArray ()
        Gl.BindVertexArray vao
        
        let vertices = [|0.f; |]
        let vertexBuffer = Gl.GenBuffer ()
        
        vao