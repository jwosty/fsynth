namespace Synth.SdlHost
open OpenGL
open System
open Synth

type VertexArrayObject(id: uint32, count: int, vbos: uint32 list) =
    member val Id = id
    member val Count = count
    member val VBOs = vbos
    
    interface IDisposable with
        override this.Dispose () =
            for vbo in vbos do Gl.DeleteBuffer vbo
            Gl.DeleteVertexArrays (1, [|this.Id|])

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VertexArrayObject =
    let draw beginMode (vao: VertexArrayObject) =
        Gl.BindVertexArray vao.Id
        Gl.DrawArrays (beginMode, 0, vao.Count)