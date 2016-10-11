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
    
    let fromVerticesAndColors verticesHintMode colorsHintMode vertices colors =
        if Seq.length vertices <> Seq.length vertices then raise (new System.ArgumentException("Vertex and color data had different lengths.", "colors"))
        let vao = Gl.GenVertexArray ()
        Gl.BindVertexArray vao
        
        let flatVertices = [|for (vertex: Vector2) in vertices do yield vertex.x; yield vertex.y|]
        let vertexBuffer = Gl.GenBuffer ()
        Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
        Gl.BufferData (BufferTarget.ArrayBuffer, sizeof<float32> * flatVertices.Length, flatVertices, verticesHintMode)
        Gl.EnableVertexAttribArray 0
        // vertex is parameter index 0 in shader
        Gl.VertexAttribPointer (0, 2, VertexAttribPointerType.Float, false, 0, 0n)
        
        let flatColors = [|for (color: Vector3) in colors do yield color.x; yield color.y; yield color.z|]
        let colorBuffer = Gl.GenBuffer ()
        Gl.BindBuffer (BufferTarget.ArrayBuffer, colorBuffer)
        Gl.BufferData (BufferTarget.ArrayBuffer, sizeof<float32> * flatColors.Length, flatColors, colorsHintMode)
        Gl.EnableVertexAttribArray 1
        // color is parameter index 1 in shader
        Gl.VertexAttribPointer (1, 3, VertexAttribPointerType.Float, false, 0, 0n)
        
        new VertexArrayObject(vao, Seq.length flatColors, [vertexBuffer; colorBuffer])