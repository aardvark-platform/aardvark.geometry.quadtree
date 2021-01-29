namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open Aardvark.Geometry.Quadtree.Serialization
open System
open System.Threading

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

    /// Count in-memory nodes. Out-of-core nodes are handled like leaf nodes.
    let rec private tryCount (outOfCore : bool) (a : int) (b : int) (root : QNodeRef) =
        let recurse = tryCount outOfCore a b
        match root with
        | NoNode                    -> 0
        | InMemoryNode _            -> a
        | OutOfCoreNode (_, load)   -> if outOfCore then load() |> recurse else a
        | InMemoryInner n           -> b + (n.SubNodes |> Array.sumBy recurse)
        | InMemoryMerge n           -> b + (n.First |> recurse) + (n.Second |> recurse)

    /// Count number of nodes in quadtree.
    /// If outOfCore is false, then out-of-core nodes are handled like leafs.
    let rec CountNodes outOfCore root = root |> tryCount outOfCore 1 1

    /// Count number of leaf nodes in quadtree.
    /// If outOfCore is false, then out-of-core nodes are handled like leafs.
    let rec CountLeafs outOfCore root = root |> tryCount outOfCore 1 0

    /// Count number of inner nodes in quadtree.
    /// If outOfCore is false, then out-of-core nodes are handled like leafs.
    let rec CountInner outOfCore root = root |> tryCount outOfCore 0 1

    let printStructure (outOfCore : bool) (n : QNodeRef) =

        let rec print indent (n : QNodeRef) =
            match n with
            | NoNode                  -> printfn "%sNoNode" indent
            | InMemoryNode n          -> printfn "%sInMemoryNode %A" indent n.Cell
            | OutOfCoreNode (id,load) -> if outOfCore then
                                            print indent (load())
                                         else
                                            printfn "%sOutOfCoreNode %A" indent id
            | InMemoryInner n       -> printfn "%sInMemoryInner %A" indent n.Cell
                                       for n in n.SubNodes do print (indent + "  ") n
            | InMemoryMerge n       -> printfn "%sInMemoryMerge %A" indent n.Cell
                                       print (indent + "  ") n.First
                                       print (indent + "  ") n.Second

        print "" n


    let rec private build (config : BuildConfig) (rootCell : Cell2d) (layers : ILayer[]) : QNodeRef =
    
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
                       n
                )

            let children = rootCell.Children
            for i = 0 to 3 do
                match subNodes.[i] with
                | InMemoryNode x    -> invariant (x.Cell = children.[i])   "15f2c6c3-6f5b-4ac0-9ec0-8ab968ac9c2e"
                | InMemoryInner x       -> invariant (x.Cell = children.[i])   "817d1b42-8fd9-465c-9cac-4d197a9a5bb9"
                | NoNode            -> ()
                | _                 -> failwith "Todo 6f5d63bc-ece6-49ef-b634-879f81a4fc36."

            let result = { Id = Guid.NewGuid(); ExactBoundingBox = ebb; Cell = rootCell; SplitLimitExponent = config.SplitLimitPowerOfTwo; SubNodes = subNodes }
            result |> InMemoryInner
        
        else
            
            let layerSet = LayerSet(layers)
            QNode(ebb, rootCell, config.SplitLimitPowerOfTwo, layerSet) |> InMemoryNode

    /// At least 1 layer is required, and
    /// all layers must have the same sample exponent and sample window.
    let Build (config : BuildConfig) ([<ParamArray>] layers : ILayer[]) : QNodeRef =
        
        invariantm (layers.Length > 0) (fun()->"Can't build quadtree with 0 layers.")               "6216df3f-279c-415f-a435-bdb35d274e39"

        let layerExponents = layers |> Array.groupBy (fun x -> x.SampleExponent)
        invariantm (layerExponents.Length = 1) (fun()->"All layers must have same resolution.")     "a25e5f58-c22a-436e-81be-69afb2b37492"

        let layerWindows = layers |> Array.groupBy (fun x -> x.SampleWindow)
        invariantm (layerWindows.Length = 1) (fun()->"All layers must have same samples window.")   "36488503-b5b8-4d80-8992-b713e7552480"
            
        let sampleExponent = layers.[0].SampleExponent
        let minRootExponent = sampleExponent + config.SplitLimitPowerOfTwo

        let globalBounds = layers |> Array.map Layer.BoundingBox |> Box2d
        let mutable rootCell = Cell2d(globalBounds)
        while rootCell.Exponent < minRootExponent do
            rootCell <- rootCell.Parent

        build config rootCell layers

    /// Returns new merged quadtree. Immutable merge.
    let Merge (domination : Dominance) (first : QNodeRef) (second : QNodeRef) =
        Merge.merge domination first second

    /// Save quadtree. Returns id of root node, or Guid.Empty if empty quadtree.
    let Save (options : SerializationOptions) (qtree : QNodeRef) : Guid =
        Defs.init ()
        Serialization.Save options qtree

    /// Load quadtree with given id.
    /// Returns the tree's root node, with children being loaded lazily.
    /// If id does not exist, then `NoNode` is returned.
    let Load (options : SerializationOptions) (id : Guid) : QNodeRef =
        Defs.init ()
        Serialization.Load options id

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

    /// Enumerates node ids of given quadtree.
    let EnumerateKeys = Serialization.EnumerateKeys

    /// Export quadtree with given id from source to target.
    let Export = Serialization.Export