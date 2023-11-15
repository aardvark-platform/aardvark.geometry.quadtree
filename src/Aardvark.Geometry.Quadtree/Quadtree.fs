namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open Aardvark.Geometry.Quadtree.Serialization
open System
open System.Collections.Generic

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

    /// If node is an OutOfCoreNode it is loaded and returned as an InMemoryNode.
    /// Otherwise node is returned.
    let EnsureInMemory (node : QNodeRef)  =
        match node with
        | OutOfCoreNode n -> n.Load()
        | _ -> node

    /// Enumerate all nodes.
    /// Out-of-core nodes are automatically loaded and traversed.
    /// A resulting QNode object x can be simply converted back to a QNodeRef by calling `InMemoryNode x`.
    let EnumerateLeafNodesInMemory (root : QNodeRef) : seq<QNode> = seq {
        
        let stack = Stack<QNodeRef>()
        stack.Push root

        while stack.Count > 0 do

            let node = stack.Pop() |> EnsureInMemory

            match node with
            | NoNode          -> ()
            | InMemoryNode q  -> yield q
            | OutOfCoreNode _ -> failwith "Internal error 7a6995bd-c0d3-4650-8ea5-9764ec3fe26c."
            | InMemoryInner n -> for subNode in n.SubNodes do subNode |> stack.Push
            | InMemoryMerge n -> n.First |> stack.Push
                                 n.Second |> stack.Push
            | LinkedNode n    -> n.Target |> stack.Push
        }

    /// Enumerate all nodes of given quadtree.
    /// If outOfCore is true, then the full tree is traversed, even nodes that are stored out-of-core.
    let EnumerateNodes (outOfCore : bool) (root : QNodeRef) : seq<QNodeRef> = seq {
        
        let stack = Stack<QNodeRef>()
        stack.Push root

        while stack.Count > 0 do

            let node = stack.Pop()

            match node with
            | NoNode          -> ()
            | InMemoryNode n  -> yield node
            | OutOfCoreNode n -> if outOfCore then stack.Push (n.Load()) else yield node
            | InMemoryInner n -> for subNode in n.SubNodes do subNode |> stack.Push
            | InMemoryMerge n -> n.First |> stack.Push
                                 n.Second |> stack.Push
                                 yield node
            | LinkedNode n    -> n.Target |> stack.Push
        }

    /// Enumerate all nodes breadth first.
    /// If outOfCore is false, then out-of-core nodes are handled like leaf nodes.
    let EnumerateNodesBreadthFirst (outOfCore : bool) (root : QNodeRef) : seq<QNodeRef> = seq {

        let queue = Queue<QNodeRef>()
        queue.Enqueue(root)

        while queue.Count > 0 do

            let node = queue.Dequeue()

            match node with
            | NoNode          -> ()
            | InMemoryNode _  -> yield node
            | OutOfCoreNode n -> if outOfCore then n.Load() |> queue.Enqueue else yield node
            | InMemoryInner n -> yield node
                                 for subNode in n.SubNodes do subNode |> queue.Enqueue
            | InMemoryMerge n -> yield node
                                 n.First |> queue.Enqueue
                                 n.Second |> queue.Enqueue
            | LinkedNode n    -> n.Target |> queue.Enqueue
        }

    [<Obsolete("Use EnumerateNodesBreadthFirst instead.")>]
    let enumerateNodesBreadthFirst = EnumerateNodesBreadthFirst
        
    /// Count nodes. If outOfCore is false, then out-of-core nodes are handled like leaf nodes.
    let private tryCount (outOfCore : bool) (weightLeaf : int) (weightInner : int) (root : QNodeRef) =
        
        let mutable count = 0

        let stack = Stack<QNodeRef>()
        stack.Push root

        while stack.Count > 0 do
            let n = stack.Pop()

            //let recurse = tryCount outOfCore a b
            count <- count + match n with
                             | NoNode          -> 0
                             | InMemoryNode _  -> weightLeaf
                             | OutOfCoreNode n -> if outOfCore then 
                                                      let loadedNode = n.Load()
                                                      loadedNode |> stack.Push 
                                                      0 
                                                  else 
                                                      weightLeaf
                             | InMemoryInner n -> for x in n.SubNodes do x |> stack.Push
                                                  weightInner
                             | InMemoryMerge n -> n.First  |> stack.Push
                                                  n.Second |> stack.Push
                                                  weightInner
                             | LinkedNode n    -> n.Target |> stack.Push
                                                  weightInner

        count

    /// Count number of nodes in quadtree.
    /// If outOfCore is false, then out-of-core nodes are handled like leaf nodes.
    let CountNodes outOfCore root = root |> tryCount outOfCore 1 1

    /// Count number of leaf nodes in quadtree.
    /// If outOfCore is false, then out-of-core nodes are handled like leaf nodes.
    let CountLeafNodes outOfCore root = root |> tryCount outOfCore 1 0
    
    [<Obsolete("Use CountLeafNodes instead.")>]
    let CountLeafs = CountLeafNodes

    /// Count number of inner nodes in quadtree.
    /// If outOfCore is false, then out-of-core nodes are handled like leaf nodes.
    let CountInnerNodes outOfCore root = root |> tryCount outOfCore 0 1

    [<Obsolete("Use CountInnerNodes instead.")>]
    let CountInner = CountInnerNodes

    /// Count number of merge nodes in quadtree.
    /// If outOfCore is false, then out-of-core nodes are handled like leaf nodes.
    let CountMergeNodes outOfCore root : int = root |> EnumerateNodes outOfCore |> Seq.filter (fun n -> match n with | InMemoryMerge _ -> true | _ -> false) |> Seq.length

    let PrintStructure (outOfCore : bool) (n : QNodeRef) =

        let rec print indent (n : QNodeRef) =
            match n with
            | NoNode          -> printfn "%sNoNode" indent
            | InMemoryNode n  -> printfn "%sInMemoryNode %A" indent n.Cell
            | OutOfCoreNode n -> if outOfCore then
                                    print indent (n.Load())
                                 else
                                    printfn "%sOutOfCoreNode %A" indent id
            | InMemoryInner n -> printfn "%sInMemoryInner %A" indent n.Cell
                                 for n in n.SubNodes do print (indent + "  ") n
            | InMemoryMerge n -> printfn "%sInMemoryMerge %A %A" indent n.Cell n.Id
                                 printfn "%s  SECOND:" indent
                                 print (indent + "  ") n.Second
                                 printfn "%s  FIRST :" indent
                                 print (indent + "  ") n.First
            | LinkedNode n    -> printfn "%sLinkedNode %A" indent n.Target.Cell
                                 print (indent + "  ") n.Target

        print "" n

    [<Obsolete("Use PrintStructure instead.")>]
    let printStructure = PrintStructure


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
                | InMemoryInner x   -> invariant (x.Cell = children.[i])   "817d1b42-8fd9-465c-9cac-4d197a9a5bb9"
                | NoNode            -> ()
                | _                 -> failwith "Todo 6f5d63bc-ece6-49ef-b634-879f81a4fc36."

            let hasMask = subNodes |> Array.exists (fun n -> n.HasMask)
            let result = { Id = Guid.NewGuid(); ExactBoundingBox = ebb; Cell = rootCell; SplitLimitExponent = config.SplitLimitPowerOfTwo; HasMask = hasMask; SubNodes = subNodes }
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

    let Link (id : Guid) (target : QNodeRef) =
        LinkedNode { Id = id; HasMask = target.HasMask; Target = target } 

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
    let GetLayer<'a when 'a : equality> (semantic : Durable.Def) (qtree : QNodeRef) : Layer<'a> =
        qtree.GetLayer<'a>(semantic)

    /// Throws if no such layer.
    let GetLayerUntyped (semantic : Durable.Def) (qtree : QNodeRef) : ILayer =
        qtree.GetLayer(semantic)

    /// Throws if no such layer.
    let TryGetLayer<'a when 'a : equality> (semantic : Durable.Def) (qtree : QNodeRef) : Layer<'a> option =
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