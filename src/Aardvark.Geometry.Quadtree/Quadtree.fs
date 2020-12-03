namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System

#nowarn "1337"

(*
    Quadtree.
*)

type BuildConfig = {
    /// Node is split if width and/or height is greater than 2^SplitLimitPowerOfTwo.
    SplitLimitPowerOfTwo : int
}
with
    static member Default = { SplitLimitPowerOfTwo = 8 }

module Quadtree =

    let rec private tryCount a b (root : QNodeRef) =
        match root.TryGetInMemory() with 
        | None -> 0 
        | Some n -> 
            match n.SubNodes with 
            | None -> a 
            | Some ns -> b + (ns |> Array.sumBy (tryCount a b))

    let rec CountNodes root = root |> tryCount 1 1
    let rec CountLeafs root = root |> tryCount 1 0
    let rec CountInner root = root |> tryCount 0 1



    let rec private build (config : BuildConfig) (rootCell : Cell2d) (layers : ILayer[]) : QNode =
    
        invariant (layers.Length > 0)                                                       "dccf4ce3-b163-41b7-9bd9-0a3e7b76e3a5"
        invariant (layers |> Array.groupBy (fun l -> l.SampleExponent) |> Array.length = 1) "4071f97e-1809-422a-90df-29c774cedc7b"

        let layerExponent = layers.[0].SampleExponent

        let ebb = layers |> Seq.map(fun x -> x.BoundingBox) |> Box2d
        let localResolution = rootCell.Exponent - layerExponent
        let needToSplit = localResolution > config.SplitLimitPowerOfTwo
        
        if needToSplit then
                    
            let subLayers = rootCell.Children |> Array.map (fun subCell ->
                let subBox = subCell.GetBoundsForExponent(layerExponent)
                let subLayers = layers |> Array.choose (fun l -> l.WithWindow subBox)
                (subCell, subLayers) 
                )

            let subNodes = subLayers |> Array.map (fun (subCell, subLayers) ->
                match subLayers.Length with
                | 0 -> NoNode
                | _ -> let n = build config subCell subLayers
                       InMemoryNode n
                )

            let children = rootCell.Children
            for i = 0 to 3 do
                match subNodes.[i].TryGetInMemory() with
                | Some x -> invariant (x.Cell = children.[i])                               "15f2c6c3-6f5b-4ac0-9ec0-8ab968ac9c2e"
                | None -> ()

            let lodLayers = QNode.generateLodLayers subNodes rootCell

            QNode(Guid.NewGuid(), ebb, rootCell, config.SplitLimitPowerOfTwo, lodLayers, Some subNodes)
        
        else
        
            QNode(ebb, rootCell, config.SplitLimitPowerOfTwo, layers)

    /// At least 1 layer is required, and
    /// all layers must have the same sample exponent and sample window.
    let Build (config : BuildConfig) ([<ParamArray>] layers : ILayer[]) : QNodeRef =
        
        invariantm (layers.Length > 0) "Can't build quadtree with 0 layers."                "6216df3f-279c-415f-a435-bdb35d274e39"

        let layerExponents = layers |> Array.groupBy (fun x -> x.SampleExponent)
        invariantm (layerExponents.Length = 1) "All layers must have same resolution."      "a25e5f58-c22a-436e-81be-69afb2b37492"

        let layerWindows = layers |> Array.groupBy (fun x -> x.SampleWindow)
        invariantm (layerWindows.Length = 1) "All layers must have same samples window."    "36488503-b5b8-4d80-8992-b713e7552480"
            
        let sampleExponent = layers.[0].SampleExponent
        let minRootExponent = sampleExponent + config.SplitLimitPowerOfTwo

        let globalBounds = layers |> Array.map Layer.BoundingBox |> Box2d
        let mutable rootCell = Cell2d(globalBounds)
        while rootCell.Exponent < minRootExponent do
            rootCell <- rootCell.Parent

        build config rootCell layers |> InMemoryNode

    /// Returns new merged quadtree. Immutable merge.
    let Merge (domination : Dominance) (first : QNodeRef) (second : QNodeRef) =
        Merge.merge domination first second

    /// Save quadtree. Returns id of root node, or Guid.Empty if empty quadtree.
    let Save (options : SerializationOptions) (qtree : QNodeRef) : Guid =
        Defs.init ()
        match qtree with
        | InMemoryNode n -> n.Save options
        | OutOfCoreNode (id, _) -> id
        | NoNode -> Guid.Empty

    /// Load quadtree with given id.
    /// Returns the tree's root node, with children being loaded lazily.
    /// If id does not exist, then `NoNode` is returned.
    let Load (options : SerializationOptions) (id : Guid) : QNodeRef =
        Defs.init ()
        QNode.Load options id

    /// Returns true if quadtree contains a layer with given semantic.
    let ContainsLayer (semantic : Durable.Def) (qtree : QNodeRef) : bool =
        qtree.ContainsLayer(semantic)

    /// Throws if no such layer.
    let GetLayer<'a> (semantic : Durable.Def) (qtree : QNodeRef) : Layer<'a> =
        qtree.GetLayer<'a>(semantic)

    /// Throws if no such layer.
    let GetLayerUntyped (semantic : Durable.Def) (qtree : QNodeRef) : ILayer =
        qtree.GetLayer(semantic)

    /// Throws if no such layer.
    let TryGetLayer<'a> (semantic : Durable.Def) (qtree : QNodeRef) : Layer<'a> option =
        qtree.TryGetLayer<'a>(semantic)

    /// Throws if no such layer.
    let TryGetLayerUntyped (semantic : Durable.Def) (qtree : QNodeRef) : ILayer option =
        qtree.TryGetLayer(semantic)

    /// Replace all occurences of 'oldSemantic' with 'newSemantic'.
    /// Returns (true, <newUpdatedOctree>) if 'oldSemantic' exists and is replaced.
    /// Returns (false, 'qtree') if 'oldSemantic' does not exist.
    /// Throws if quadtree contains both 'oldSemantic' and 'newSemantic'.
    let UpdateLayerSemantic (oldSemantic : Durable.Def, newSemantic : Durable.Def) (qtree : QNodeRef) : bool * QNodeRef =
        qtree.UpdateLayerSemantic(oldSemantic, newSemantic)