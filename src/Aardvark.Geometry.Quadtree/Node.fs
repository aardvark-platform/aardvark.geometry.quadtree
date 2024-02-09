namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System

(*
    Node.
*)

/// Which data dominates in merge operations.
type Dominance = 
    | FirstDominates 
    | SecondDominates 
    | MoreDetailedOrFirst
    | MoreDetailedOrSecond
with
    member this.Flipped with get() =
        match this with 
        | FirstDominates       -> SecondDominates
        | SecondDominates      -> FirstDominates
        | MoreDetailedOrFirst  -> MoreDetailedOrSecond
        | MoreDetailedOrSecond -> MoreDetailedOrFirst

/// Leaf node with original data.
and QNode(uid : Guid, exactBoundingBox : Box2d, cell : Cell2d, splitLimitExp : int, layers : LayerSet) =

    do
        invariant (exactBoundingBox.IsValid) "661f5dfa-2706-4935-ae4e-1d25f02224ae"

        invariantm (cell.Exponent - splitLimitExp = layers.SampleExponent) 
            (fun () -> "Sample exponent does not match split limit and node cell.")
            "1ec76eaa-534b-4339-b63b-8e0399562bb1"

        let w = layers.SampleWindow
        let e = layers.SampleExponent
        let bb = cell.BoundingBox
        for layer in layers.Layers do
            invariantm (layer.SampleExponent = e) (fun()->"Layers exponent mismatch.")   "7adc422c-effc-4a86-b493-ff1cd0f9e991"
            invariantm (layer.SampleWindow = w)   (fun()->"Layers window mismatch.")     "74a57d1d-6a7f-4f9e-b26c-41a1e79cb989"
            invariantm (bb.Contains(layer.Mapping.BoundingBox)) 
                (fun()->sprintf "Layer %A is outside node bounds." layer.Def.Id)       "dbe069cc-df5c-42c1-bb58-59c5a061ee15"

    new (exactBoundingBox : Box2d, cell : Cell2d, splitLimitExp : int, layers : LayerSet) =
        QNode(Guid.NewGuid(), exactBoundingBox, cell, splitLimitExp, layers)

    new (splitLimitExp : int, layers : LayerSet) =
        QNode(Guid.NewGuid(), layers.BoundingBox, layers.BoundingBox |> Cell2d, splitLimitExp, layers)

    member _.Id with get() = uid

    member _.Cell with get() = cell

    member _.ExactBoundingBox with get() = exactBoundingBox

    /// The maximum tile size, given as width = height = 2^SplitLimitExponent.
    member _.SplitLimitExponent with get() = splitLimitExp

    /// Replace all occurences of 'oldSemantic' with 'newSemantic'.
    /// Returns 'Some <newUpdatedOctree>' if 'oldSemantic' exists and is replaced.
    /// Returns 'None' if 'oldSemantic' does not exist.
    /// Throws if quadtree contains both 'oldSemantic' and 'newSemantic'.
    member this.UpdateLayerSemantic (oldSemantic : Durable.Def, newSemantic : Durable.Def) : QNode option =
        match layers.UpdateLayerSemantic(oldSemantic, newSemantic) with
        | None -> None
        | Some newLayers -> QNode(exactBoundingBox, cell, splitLimitExp, newLayers) |> Some
        
    member _.LayerSet with get() = layers

    member this.ContainsLayer (semantic : Durable.Def) : bool =
        this.TryGetLayer(semantic) |> Option.isSome

    member this.TryGetLayer (semantic : Durable.Def) : ILayer option =
        layers.TryGetLayer(semantic)

    member this.TryGetLayer<'a when 'a : equality> (semantic : Durable.Def) : Layer<'a> option =
        layers.TryGetLayer<'a>(semantic)

    member this.GetLayer(semantic : Durable.Def) : ILayer =
        layers.GetLayer(semantic)

    member this.GetLayer<'a when 'a : equality>(semantic : Durable.Def) : Layer<'a> =
        layers.GetLayer<'a>(semantic)

    member this.WithWindow (w : Box2l) : QNode option =
        match layers.WithWindow(w) with
        | None -> None
        | Some windowedLayers ->
            let ebb = windowedLayers.BoundingBox
            let n = QNode(ebb, cell, splitLimitExp, windowedLayers)
            Some n

    member this.WithLayers (newLayers : LayerSet) : QNode =
        QNode(exactBoundingBox, cell, splitLimitExp, newLayers)

    member this.GetAllSamples () : Cell2d[] =
        layers.SampleWindow.GetAllSamples(layers.SampleExponent)

    member this.GetAllSamplesInsideWindow (window : Box2l) : Cell2d[] =
        invariant (layers.SampleWindow.Contains(window)) "f244101f-975c-4a0a-8c03-20e0618834b4"
        window.GetAllSamples(layers.SampleExponent)

    member this.GetAllSamplesFromFirstMinusSecond (first : Box2l, second : Box2l) : Cell2d[] =
        first.GetAllSamplesFromFirstMinusSecond(second, layers.SampleExponent)

    member this.GetSample (p : V2d) : Cell2d =
        layers.Mapping.GetSampleCell(p)

    /// Resolution level.
    member this.SampleExponent with get() = layers.SampleExponent

    /// Returns 2.0 ^ SampleExponent.
    member this.SampleSize with get() : float = layers.SampleSize

    /// True if there is a layer with a mask.
    member this.HasMask with get() : bool = layers.HasMask

and
    QInnerNode = {
        Id : Guid
        ExactBoundingBox : Box2d
        Cell : Cell2d
        SplitLimitExponent : int
        HasMask : bool
        SubNodes : QNodeRef[]
        }

and
    QMergeNode = {
        Id : Guid
        ExactBoundingBox : Box2d
        Cell : Cell2d
        SplitLimitExponent : int
        HasMask : bool
        Dominance : Dominance
        First : QNodeRef
        Second : QNodeRef
        }
and
    QMultiMergeNode = {
        Id : Guid
        ExactBoundingBox : Box2d
        Cell : Cell2d
        SplitLimitExponent : int
        HasMask : bool
        Nodes : QNodeRef[]
        }
and
    QLinkedNode = {
        Id : Guid
        HasMask : bool
        Target : QNodeRef
        }
and

    QOutOfCoreNode = {
        Id : Guid
        HasMask : bool
        Load :  unit -> QNodeRef
        }
and

    QNodeRef =
    | NoNode
    | InMemoryNode  of QNode
    | OutOfCoreNode of QOutOfCoreNode
    | InMemoryInner of QInnerNode
    | InMemoryMerge of QMergeNode
    //| MultiMerge    of QMultiMergeNode
    | LinkedNode    of QLinkedNode
    with

        /// Forces property Id. Throws exception if NoNode.
        member this.Id with get() =
            match this with
            | NoNode          -> failwith "Id does not exist for NoNode. Error c48422a9-4df6-4d15-9dad-af075a1d52ac."
            | InMemoryNode  n -> n.Id
            | InMemoryInner n -> n.Id
            | InMemoryMerge n -> n.Id
            //| MultiMerge    n -> n.Id
            | OutOfCoreNode n -> n.Load().Id
            | LinkedNode    n -> n.Id

        /// Forces property Cell. Throws exception if NoNode.
        member this.Cell with get() =
            match this with
            | NoNode          -> failwith "Cell does not exist for NoNode. Error c48422a9-4df6-4d15-9dad-af075a1d52ac."
            | InMemoryNode  n -> n.Cell
            | InMemoryInner n -> n.Cell
            | InMemoryMerge n -> n.Cell
            //| MultiMerge    n -> n.Cell
            | OutOfCoreNode n -> n.Load().Cell
            | LinkedNode    n -> n.Target.Cell

         /// Returns node's exact bounding box, or invalid box for NoNode.
        member this.ExactBoundingBox with get() =
            match this with
            | NoNode          -> Box2d.Invalid
            | InMemoryNode  n -> n.ExactBoundingBox
            | InMemoryInner n -> n.ExactBoundingBox
            | InMemoryMerge n -> n.ExactBoundingBox
            //| MultiMerge    n -> n.ExactBoundingBox
            | OutOfCoreNode n -> n.Load().ExactBoundingBox
            | LinkedNode    n -> n.Target.ExactBoundingBox

        /// Forces property SplitLimitExp. Throws exception if NoNode.
        member this.SplitLimitExponent with get() =
            match this with
            | NoNode          -> failwith "SplitLimitExp does not exist for NoNode. Error 4424a37e-dcd8-4ae0-975c-7ae6d926aaa8."
            | InMemoryNode  n -> n.SplitLimitExponent
            | InMemoryInner n -> n.SplitLimitExponent
            | InMemoryMerge n -> n.SplitLimitExponent
            //| MultiMerge    n -> n.SplitLimitExponent
            | OutOfCoreNode n -> n.Load().SplitLimitExponent
            | LinkedNode    n -> n.Target.SplitLimitExponent

        member this.GetFirstNonEmptySubNode() : QNodeRef option =
            match this with
            | NoNode          -> None
            | InMemoryNode  _ -> None
            | InMemoryInner n -> n.SubNodes |> Array.tryFind (fun sn -> match sn with | NoNode -> false | _ -> true)
            | InMemoryMerge n -> Some n.First
            //| MultiMerge    n -> Some n.Nodes[0]
            | OutOfCoreNode n -> n.Load().GetFirstNonEmptySubNode()
            | LinkedNode    n -> n.Target.GetFirstNonEmptySubNode()

        member this.ContainsLayer (semantic : Durable.Def) =
            match this with
            | NoNode          -> false
            | InMemoryNode  n -> n.ContainsLayer semantic
            | InMemoryInner _ -> match this.GetFirstNonEmptySubNode() with
                                 | Some n -> n.ContainsLayer(semantic)
                                 | None -> failwith "Invariant 0a85f195-7feb-4ecb-912d-7fd4e7024951."
            | InMemoryMerge _ -> false
            //| MultiMerge    _ -> false
            | OutOfCoreNode n -> n.Load().ContainsLayer semantic
            | LinkedNode    n -> n.Target.ContainsLayer semantic

        member this.LayerSet with get() : LayerSet option =
            match this with
            | NoNode          -> None
            | InMemoryNode  n -> Some(n.LayerSet)
            | InMemoryInner _ -> None
            | InMemoryMerge _ -> None
            //| MultiMerge    _ -> None
            | OutOfCoreNode n -> n.Load().LayerSet
            | LinkedNode    n -> n.Target.LayerSet

        member this.IsLeafNode with get() =
            match this with
            | NoNode          -> false
            | InMemoryNode  _ -> true
            | InMemoryInner _ -> false
            | InMemoryMerge _ -> false
            //| MultiMerge    _ -> false
            | OutOfCoreNode _ -> true
            | LinkedNode    n -> n.Target.IsLeafNode

        member this.HasMask with get() =
            match this with
            | NoNode          -> false
            | InMemoryNode  n -> n.HasMask
            | InMemoryInner n -> n.HasMask
            | InMemoryMerge n -> n.HasMask
            //| MultiMerge    n -> n.HasMask
            | OutOfCoreNode n -> n.HasMask
            | LinkedNode    n -> n.Target.HasMask

        /// Replace all occurences of 'oldSemantic' with 'newSemantic'.
        /// Returns (true, <newUpdatedOctree>) if 'oldSemantic' exists and is replaced.
        /// Returns (false, 'qtree') if 'oldSemantic' does not exist.
        /// Throws if quadtree contains both 'oldSemantic' and 'newSemantic'.
        member this.UpdateLayerSemantic (oldSemantic : Durable.Def, newSemantic : Durable.Def) : bool * QNodeRef =
            
            let unchanged = (false, this)

            let qnode (n : QNodeRef) =
                n.UpdateLayerSemantic(oldSemantic, newSemantic)

            match this with
            | NoNode                  -> unchanged

            | InMemoryInner n         ->
                let xs = n.SubNodes |> Array.map (fun sn -> sn.UpdateLayerSemantic(oldSemantic, newSemantic))
                match xs |> Array.exists fst with
                | false -> unchanged
                | true  -> let sns = xs |> Array.map snd
                           let updatedNode = { n with SubNodes = sns }
                           (true, InMemoryInner updatedNode)

            | InMemoryMerge n         ->
                let n1 = n.First.UpdateLayerSemantic(oldSemantic, newSemantic)
                let n2 = n.Second.UpdateLayerSemantic(oldSemantic, newSemantic)
                match n1, n2 with
                | (false, _), (false, _) -> unchanged
                | (_, q1), (_, q2)       -> 
                    let updatedNode = { n with First = q1; Second = q2 }
                    (true, InMemoryMerge updatedNode)

            //| MultiMerge n            ->
            //    let ns = n.Nodes |> Array.map (fun x -> x.UpdateLayerSemantic(oldSemantic, newSemantic))
            //    let isNotChanged = ns |> Array.forall (fun (x, _) -> x = false)
            //    if isNotChanged then
            //        unchanged
            //    else
            //        let updatedNode = { n with Nodes = ns |> Array.map snd }
            //        (true, MultiMerge updatedNode)
                    
            | InMemoryNode  n         ->
                match n.UpdateLayerSemantic(oldSemantic, newSemantic) with
                | Some qnode -> (true, InMemoryNode qnode)
                | None       -> unchanged

            | OutOfCoreNode n         -> n.Load() |> qnode

            | LinkedNode n            -> n.Target.UpdateLayerSemantic (oldSemantic, newSemantic)

        /// Throws if no such layer.
        member this.GetLayer def : ILayer =
            match this with
            | NoNode
            | InMemoryInner _
            //| MultiMerge _ 
            | InMemoryMerge _ -> sprintf "Layer not found. %A. Error 2c634f5f-d359-4523-b87a-a96d2522c018." def |> failwith
            | InMemoryNode  n -> n.LayerSet.GetLayer def
            | OutOfCoreNode n -> n.Load().LayerSet.Value.GetLayer def
            | LinkedNode    n -> n.Target.GetLayer def

        /// Throws if no such layer.
        member this.GetLayer<'a when 'a : equality> def = 
            match this with
            | NoNode
            | InMemoryInner _
            //| MultiMerge    _ 
            | InMemoryMerge _ -> sprintf "Layer not found. %A. Error f33f39a9-2394-473f-8dae-76bd8baaaafb." def |> failwith
            | InMemoryNode  n -> n.LayerSet.GetLayer<'a> def
            | OutOfCoreNode n -> n.Load().LayerSet.Value.GetLayer<'a> def
            | LinkedNode    n -> n.Target.GetLayer<'a> def
    
        member this.TryGetLayer def =
            match this with
            | NoNode
            | InMemoryInner _ 
            //| MultiMerge    _ 
            | InMemoryMerge _ -> None
            | InMemoryNode  n -> n.LayerSet.TryGetLayer def
            | OutOfCoreNode n -> match n.Load().LayerSet with
                                 | Some ls -> ls.TryGetLayer def
                                 | None -> None
            | LinkedNode    n -> n.Target.TryGetLayer def

        member this.TryGetLayer<'a when 'a : equality> def =
            match this with
            | NoNode
            | InMemoryInner _
            //| MultiMerge    _ 
            | InMemoryMerge _ -> None
            | InMemoryNode  n -> n.LayerSet.TryGetLayer<'a> def
            | OutOfCoreNode n -> match n.Load().LayerSet with
                                 | Some ls -> ls.TryGetLayer<'a> def
                                 | None -> None
            | LinkedNode    n -> n.Target.TryGetLayer<'a> def

module QNode =

    let toRef (n : QNode option) =
        match n with
        | Some n -> InMemoryNode n
        | None -> NoNode

module QInnerNode =

    let ofSubNode (n : QNodeRef) : QInnerNode =
        let rc = n.Cell.Parent
        match Option.ofNullable(rc.GetQuadrant(n.Cell)) with
        | None -> failwith "Invariant 3ada0081-039d-42c6-baad-94fbf26bf3ce."
        | Some qi ->
            let ns = Array.create 4 NoNode
            ns.[qi] <- n
            { Id = Guid.NewGuid(); ExactBoundingBox = n.ExactBoundingBox; Cell = rc; SplitLimitExponent = n.SplitLimitExponent; HasMask = n.HasMask; SubNodes = ns }

    let ofSubNodes (ns : QNodeRef[]) : QInnerNode =
        invariant (ns.Length = 4) "acf0afb0-5d73-4827-acca-e1707ec6db5a"
        // non-empty subnodes
        let ns' = ns |> Array.choose(fun n -> match n with | NoNode -> None | n -> Some n)
        // no duplicate subnodes allowed
        invariant ((ns' |> Seq.map(fun n -> n.Cell) |>  Seq.distinct |> Seq.length) = ns'.Length) "3242ee2c-c4bb-47aa-91ac-9627eaa98754"

        let isCenterChildren = ns' |> Array.forall(fun n -> n.Cell.TouchesOrigin)
        let rc = if isCenterChildren then Cell2d(ns'.[0].Cell.Exponent + 1)
                 else // all subnodes must be direct children of same root cell (rc)
                      ns' |> Seq.map(fun n -> n.Cell.Parent) |> Seq.distinct |> Seq.exactlyOne

        let ebb = ns' |> Seq.map(fun n -> n.ExactBoundingBox) |> Box2d

        let hasMask = ns |> Array.exists (fun n -> n.HasMask)
        { Id = Guid.NewGuid(); ExactBoundingBox = ebb; Cell = rc; SplitLimitExponent = ns'.[0].SplitLimitExponent; HasMask = hasMask; SubNodes = ns }
            

module QMergeNode =

    let isLegalMergeConstellation (first : QNodeRef) (second : QNodeRef) : bool =
        let isSpecialOverlappingCase = first.Cell.IsCenteredAtOrigin && second.Cell.TouchesOrigin ||
                                       second.Cell.IsCenteredAtOrigin && first.Cell.TouchesOrigin
        let bothTouchOrigin = first.Cell.TouchesOrigin && second.Cell.TouchesOrigin
        let bothCentered = first.Cell.IsCenteredAtOrigin && second.Cell.IsCenteredAtOrigin

        first.Cell = second.Cell || isSpecialOverlappingCase || bothTouchOrigin || bothCentered

    let ofNodes dom (first : QNodeRef) (second : QNodeRef) : QMergeNode =
        invariant (isLegalMergeConstellation first second) "42a0c7ba-cf29-4820-b420-a74d668cb159"
        let ebb = Box2d(first.ExactBoundingBox, second.ExactBoundingBox)
        let cell = first.Cell.Union(second.Cell)
        {
            Id = Guid.NewGuid()
            ExactBoundingBox = ebb; Cell = cell; SplitLimitExponent = first.SplitLimitExponent
            HasMask = first.HasMask || second.HasMask
            Dominance = dom; First = first; Second = second
        }

    