namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data

(*
    Node.
*)

type INode =
    /// Cell occupied by this node.
    abstract member Cell : Cell2d
    abstract member Layers : ILayer[]
    abstract member SampleWindow : Box2l
    abstract member SampleExponent : int
    abstract member SubNodes : INode option[] option
    abstract member WithLayers : ILayer[] -> INode
    abstract member GetLayer<'b> : Durable.Def -> Layer<'b>

[<AutoOpen>]
module INodeExtensions = 
    type INode with
        member this.IsInnerNode with get() = this.SubNodes.IsSome
        member this.IsLeafNode  with get() = this.SubNodes.IsNone
        member this.SampleWindowBoundingBox with get() = this.Layers.[0].BoundingBox
        member this.AllSamples with get() =
            let e = this.Layers.[0].SampleExponent
            let w = this.Layers.[0].SampleWindow
            let xMaxIncl = int w.Size.X - 1
            let yMaxIncl = int w.Size.Y - 1

            let samples = Array.zeroCreate (int w.Size.X * int w.Size.Y)
            let mutable i = 0
            for y = 0 to yMaxIncl do
                for x = 0 to xMaxIncl do
                    samples.[i] <- Cell2d(w.Min.X + int64 x, w.Min.Y + int64 y, e)
                    i <- i + 1
            samples

type Node(cell : Cell2d, layers : ILayer[], subNodes : INode option[] option) =

    

    do
        if layers.Length = 0 then
            failwith "No layers. Invariant fe0e56d7-9bc2-4f61-8b36-0ed7fcc4bc56."
        let w = layers.[0].SampleWindow
        let e = layers.[0].SampleExponent
        let bb = cell.BoundingBox
        for layer in layers do
            if layer.SampleExponent <> e then failwith "Layers exponent mismatch."
            if layer.SampleWindow <> w then failwith "Layers window mismatch."
            if not(bb.Contains(layer.Mapping.BoundingBox)) then 
                invalidArg "layers" (sprintf "Layer %A is outside node bounds." layer.Def.Id)
            
        if subNodes.IsSome && subNodes.Value.Length <> 4 then 
            invalidArg "subNodes" "Invariant 20baf723-cf32-46a6-9729-3b4e062ceee5."

    new (cell : Cell2d, layers : ILayer[]) = Node(cell, layers, None)

    member this.GetLayer<'a>(def : Durable.Def) : Layer<'a> =
        layers |> Array.find (fun x -> x.Def.Id = def.Id) :?> Layer<'a>

    interface INode with
        member _.Cell with get() = cell
        member _.Layers with get() = layers
        member _.SampleWindow with get() = layers.[0].SampleWindow
        member _.SampleExponent with get() = layers.[0].SampleExponent
        member _.SubNodes with get() = subNodes
        member _.WithLayers (newLayers : ILayer[]) = Node(cell, newLayers, subNodes) :> INode
        member this.GetLayer<'b> (def : Durable.Def) = this.GetLayer<'b>(def)

module Node =

    let internal GenerateLodLayers (subNodes : INode option[]) =

        let subNodes = subNodes |> Array.choose id
        invariant (subNodes.Length > 0) "641ef4b4-65b3-4e76-bbb6-c7046452801a."

        let subNodeExponent = subNodes.[0].SampleExponent
        invariant (subNodes |> Array.forall (fun n -> n.SampleExponent = subNodeExponent)) "0615df58-7c58-4b48-8be4-3f872140bbb2."

        let result =
            subNodes |> Seq.map (fun x -> x.Layers) |> Seq.collect id
            |> Seq.groupBy (fun x -> x.Def)
            |> Seq.map (fun (_, xs) ->
                let maxExponent = xs |> Seq.map (fun x -> x.SampleExponent) |> Seq.max
                xs 
                |> Seq.map (fun layer ->
                    let mutable l = layer
                    while l.SampleExponent < maxExponent do l <- l.ResampleUntyped()
                    l
                    )
                |> Layer.Merge
                )
            |> Seq.choose id
            |> Seq.map (fun layer -> layer.ResampleUntyped())
            |> Seq.toArray

        let selfExponent = result.[0].SampleExponent
        invariant (selfExponent = subNodeExponent + 1) "4a8cbec0-4e14-4eb4-a21a-0af370cc1d81."
        invariant (result |> Array.forall (fun n -> n.SampleExponent = selfExponent)) "5296dbf9-5ab4-4529-a473-39cda2d0eb5f."

        result