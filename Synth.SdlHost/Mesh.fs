namespace Synth.SdlHost
open OpenGL
open System
open Synth

type Mesh(vaoId: uint32, count: int, vertexType: BeginMode, vertexVBO: uint32, colorVBO: uint32) =
    member val VAOId = vaoId
    member val Count = count
    member val VertexType = vertexType
    member val VertexVBO = vertexVBO
    member val ColorVBO = colorVBO
    
    interface IDisposable with
        override this.Dispose () =
            Gl.DeleteBuffer this.VertexVBO
            Gl.DeleteBuffer this.ColorVBO
            Gl.DeleteVertexArrays (1, [|this.VAOId|])

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Mesh =
    let draw (mesh: Mesh) =
        Gl.BindVertexArray mesh.VAOId
        Gl.DrawArrays (mesh.VertexType, 0, mesh.Count)
    
    let create verticesHintMode colorsHintMode vertexType vertices colors =
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
        
        new Mesh(vao, Seq.length flatColors, vertexType, vertexBuffer, colorBuffer)