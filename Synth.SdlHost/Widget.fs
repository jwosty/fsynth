namespace Synth.SdlHost
open HelperFunctions
open OpenGL
open System

type WidgetView(modelMatrix: Matrix4, meshes: VertexArrayObject list) =
    member val ModelMatrix = modelMatrix with get, set
    member val Meshes = meshes
    interface IDisposable with
        override this.Dispose () =
            this.Meshes |> List.iter dispose