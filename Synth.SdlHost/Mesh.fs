namespace Synth.SdlHost
open OpenGL
open System
open System.Runtime.InteropServices
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
    let create verticesHintMode colorsHintMode vertexType vertices colors =
        if Seq.length vertices <> Seq.length vertices then raise (new System.ArgumentException("Vertex and color data had different lengths.", "colors"))
        let vao = Gl.GenVertexArray ()
        Gl.BindVertexArray vao
        
        let flatVertices = [|for (vertex: Vector2) in vertices do yield vertex.x; yield vertex.y|]
        let vertexBuffer = Gl.GenBuffer ()
        Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
        // give the buffer 2x the capacity it actually needs for now, so we can add more things to it
        Gl.BufferData (BufferTarget.ArrayBuffer, sizeof<float32> * flatVertices.Length * 2, flatVertices, verticesHintMode)
        Gl.EnableVertexAttribArray 0
        // vertex is parameter index 0 in shader
        Gl.VertexAttribPointer (0, 2, VertexAttribPointerType.Float, false, 0, 0n)
        
        let flatColors = [|for (color: Vector3) in colors do yield color.x; yield color.y; yield color.z|]
        let colorBuffer = Gl.GenBuffer ()
        Gl.BindBuffer (BufferTarget.ArrayBuffer, colorBuffer)
        // give the buffer 2x the capacity it actually needs for now, so we can add more things to it
        Gl.BufferData (BufferTarget.ArrayBuffer, sizeof<float32> * flatColors.Length * 2, flatColors, colorsHintMode)
        Gl.EnableVertexAttribArray 1
        // color is parameter index 1 in shader
        Gl.VertexAttribPointer (1, 3, VertexAttribPointerType.Float, false, 0, 0n)
        
        new Mesh(vao, Seq.length flatColors, vertexType, vertexBuffer, colorBuffer)
    
    /// Draw all elements of a mesh in the current GL context
    let draw (mesh: Mesh) =
        Gl.BindVertexArray mesh.VAOId
        Gl.DrawArrays (mesh.VertexType, 0, mesh.Count)
    
    /// Draw specific object vertices of a mesh in the current GL context
    let drawObjects elementsPerObject elements (mesh: Mesh) =
        let verticesPerElement =
            match mesh.VertexType with
            | BeginMode.Triangles -> 3
            | BeginMode.Lines -> 2
        let stride = verticesPerElement * elementsPerObject
        let indices =
            [|for element in elements do
                for i in 0 .. (stride - 1) do
                    yield (element * stride) + i|]
        Gl.BindVertexArray mesh.VAOId
        // This is probably relatively costly
        let indicesBuffer = Gl.GenBuffer ()
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indicesBuffer)
        Gl.BufferData (BufferTarget.ElementArrayBuffer, sizeof<uint32> * indices.Length, indices, BufferUsageHint.StaticDraw)
        Gl.DrawElements (mesh.VertexType, indices.Length, DrawElementsType.UnsignedInt, 0n)
        Gl.DeleteBuffer indicesBuffer