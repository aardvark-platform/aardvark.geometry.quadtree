﻿namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System.Collections.Generic
open System.Collections.Immutable

(*
    Merge.
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
        
module Merge =

    type Buffer<'a> = {
        Semantic : Durable.Def
        Mapping : DataMapping
        Data : 'a[]
        }

    module private Buffer =

        let private undefinedValues = 
            ImmutableDictionary<System.Type, obj>.Empty

              .Add(typeof<float>, nan)
              .Add(typeof<float32>, nanf)

              .Add(typeof<V2d>, V2d.NaN).Add(typeof<V2f>, V2f.NaN)
              .Add(typeof<V3d>, V3d.NaN).Add(typeof<V3f>, V3f.NaN)
              .Add(typeof<V4d>, V4d.NaN).Add(typeof<V4f>, V4f.NaN)

              .Add(typeof<C3b>, C3b.Black).Add(typeof<C3f>, C3f.Black)
              .Add(typeof<C4b>, C4b.Black).Add(typeof<C4f>, C4f.Black)


        let create (semantic : Durable.Def) (w : Box2l) (e : int) =
            let (found, x) = undefinedValues.TryGetValue(typeof<'a>)
            if found then
                let initialValue = x :?> 'a
                {
                    Semantic = semantic
                    Mapping = DataMapping(Cell2d(w.Min, e), V2i w.Size, w)
                    Data = Array.create<'a> (int w.Size.X * int w.Size.Y) initialValue 
                }
            else
                sprintf "Type %A not supported. Error 3025ac52-d99b-41bd-bc4e-16d75773f1b4." typeof<'a> |> failwith

        let add (layer : Layer<'a>) (buffer : Buffer<'a>)  =
            let layerSampleExponent = layer.SampleExponent
            let layerWindow = layer.Mapping.Window
            let bufferSampleExponent = buffer.Mapping.BufferOrigin.Exponent
            let bufferData = buffer.Data
            let xMaxIncl = int layerWindow.SizeX - 1
            let yMaxIncl = int layerWindow.SizeY - 1

            // layer has SAME RESOLUTION as buffer -> direct copy
            if bufferSampleExponent = layerSampleExponent then
                for y = 0 to yMaxIncl do
                    for x = 0 to xMaxIncl do
                        let c = Cell2d(layerWindow.Min.X + int64 x, layerWindow.Min.Y + int64 y, layerSampleExponent)
                        let i = buffer.Mapping.GetBufferIndex c
                        let v = layer.GetSample(Fail, c)
                        bufferData.[i] <- v
                buffer

            // layer has PARENT RESOLUTION of buffer -> copy each sample 2x2 times
            elif bufferSampleExponent = layerSampleExponent - 1 then
                for y = 0 to yMaxIncl do
                    for x = 0 to xMaxIncl do
                        let cLayer = Cell2d(layerWindow.Min.X + int64 x, layerWindow.Min.Y + int64 y, layerSampleExponent)
                        let v = layer.GetSample(Fail, cLayer)
                        for qi = 0 to 3 do
                            let c = cLayer.GetQuadrant(qi)
                            let i = buffer.Mapping.GetBufferIndex c
                            bufferData.[i] <- v
                buffer

            else
                failwith "Invariant 7634631b-cd33-43e9-844b-52c2faba8ddf."

        let addMany (layers : Layer<'a> seq) (buffer : Buffer<'a>) =
            let mutable b = buffer
            for l in layers do b <- add l b
            b

        let toLayer (buffer : Buffer<'a>) =
            Layer(buffer.Semantic, buffer.Data, buffer.Mapping)

    type ComposedLayerResult = {
        Layer : ILayer
        ChildLayer : ILayer
    }



    open Buffer
    let private composeLayersInOrderTyped<'a> 
        (semantic : Durable.Def) (bounds : Cell2d) (sampleExponentResult : int) (targetWindowAtChildLevel : Box2l) 
        (layersInOrder : Layer<'a>[])
        : ComposedLayerResult =
        
        invariant (layersInOrder.Length > 0) "0ea261d0-e3e7-4edb-bc95-0fa1eb1d3a86"

        let e = sampleExponentResult
        let resampler = Resamplers.getTypedResamplerFor<'a> semantic 

        let layerChildLevel = 
            create semantic targetWindowAtChildLevel (e - 1)
            |> addMany layersInOrder
            |> toLayer

        let layerResultLevel = layerChildLevel.Resample ClampToEdge resampler bounds

        //let childLayers = layersInOrder |> Array.filter (fun x -> x.SampleExponent = e - 1)
        //let childLayersWindow = childLayers |> Seq.map (fun x -> x.SampleWindow) |> Box2l
        //let childLayer = (layerChildLevel.WithWindow childLayersWindow).Value

        { Layer = layerResultLevel; ChildLayer = layerChildLevel }




    let private composeLayersInOrder (def : Durable.Def) (bounds : Cell2d) (sampleExponentResult : int) (targetWindowAtChildLevel : Box2l) 
                             (layersInOrder : ILayer[])
                             : ComposedLayerResult =

        let e = sampleExponentResult

        // ensure that all sample exponents are consistent ...
        invariant (layersInOrder |> Array.forall (fun x -> x.SampleExponent = e || x.SampleExponent = e - 1)) "5ec641c3-da8f-4757-bed0-fc5a527572fb"

        let compose convert =
            let layersInOrder' = layersInOrder |> Array.map convert
            composeLayersInOrderTyped def bounds sampleExponentResult targetWindowAtChildLevel layersInOrder'

        let someLayer = layersInOrder |> Seq.head
        match someLayer with

        | :? Layer<bool>    -> (compose (fun x -> x :?> Layer<bool>   ))
        | :? Layer<int8>    -> (compose (fun x -> x :?> Layer<int8>   ))
        | :? Layer<uint8>   -> (compose (fun x -> x :?> Layer<uint8>  ))
        | :? Layer<int16>   -> (compose (fun x -> x :?> Layer<int16>  ))
        | :? Layer<uint16>  -> (compose (fun x -> x :?> Layer<uint16> ))
        | :? Layer<int32>   -> (compose (fun x -> x :?> Layer<int32>  ))
        | :? Layer<uint32>  -> (compose (fun x -> x :?> Layer<uint32> ))
        | :? Layer<int64>   -> (compose (fun x -> x :?> Layer<int64>  ))
        | :? Layer<uint64>  -> (compose (fun x -> x :?> Layer<uint64> ))
        | :? Layer<float>   -> (compose (fun x -> x :?> Layer<float>  ))
        | :? Layer<float32> -> (compose (fun x -> x :?> Layer<float32>))
        | :? Layer<decimal> -> (compose (fun x -> x :?> Layer<decimal>))
        
        | :? Layer<V2d>     -> (compose (fun x -> x :?> Layer<V2d>    ))
        | :? Layer<V2f>     -> (compose (fun x -> x :?> Layer<V2f>    ))
        | :? Layer<V2i>     -> (compose (fun x -> x :?> Layer<V2i>    ))
        | :? Layer<V2l>     -> (compose (fun x -> x :?> Layer<V2l>    ))

        | :? Layer<V3d>     -> (compose (fun x -> x :?> Layer<V3d>    ))
        | :? Layer<V3f>     -> (compose (fun x -> x :?> Layer<V3f>    ))
        | :? Layer<V3i>     -> (compose (fun x -> x :?> Layer<V3i>    ))
        | :? Layer<V3l>     -> (compose (fun x -> x :?> Layer<V3l>    ))

        | :? Layer<V4d>     -> (compose (fun x -> x :?> Layer<V4d>    ))
        | :? Layer<V4f>     -> (compose (fun x -> x :?> Layer<V4f>    ))
        | :? Layer<V4i>     -> (compose (fun x -> x :?> Layer<V4i>    ))
        | :? Layer<V4l>     -> (compose (fun x -> x :?> Layer<V4l>    ))

        | :? Layer<C3b>     -> (compose (fun x -> x :?> Layer<C3b>    ))
        | :? Layer<C3f>     -> (compose (fun x -> x :?> Layer<C3f>    ))
        | :? Layer<C4b>     -> (compose (fun x -> x :?> Layer<C4b>    ))
        | :? Layer<C4f>     -> (compose (fun x -> x :?> Layer<C4f>    ))

        | _ -> failwith <| sprintf "Unsupported layer type %A. Invariant d31887e6-6c87-4b5a-87d7-0cab1fe9ec55." someLayer




    /// all parts must have same semantic ...
    let private createLayer (bounds : Cell2d) (def : Durable.Def) (domination : Dominance)
                            (l1o : ILayer option) (slo1 : array<ILayer option>)
                            (l2o : ILayer option) (slo2 : array<ILayer option>) 
                            : ComposedLayerResult =

        //invariant (domination <> MoreDetailedOrFirst && domination <> MoreDetailedOrSecond) "58d9ee5a-ba81-4e45-b03f-4e2dd780175a"
        invariant (l1o.IsNone || l1o.Value.Def = def) "c8931d12-5472-4f11-ab64-b5c49e1ebbb5"
        invariant (l2o.IsNone || l2o.Value.Def = def) "c9e6f48e-763c-482a-8cb2-909490b978f2"
        invariant (slo1 |> Seq.choose id |> Seq.forall (fun x -> x.Def = def)) "5739daef-3c13-4409-9ba0-78a5a311634c"
        invariant (slo2 |> Seq.choose id |> Seq.forall (fun x -> x.Def = def)) "c2aaadbd-86aa-4115-9d04-42e50ab0f414"

        // ensure that all parts have consistent sample exponents ...
        let f0 = Option.map (fun (x : ILayer) -> x.SampleExponent)
        let f1 = Option.map (fun (x : ILayer) -> x.SampleExponent + 1)
        let sampleExponents = [l1o |> f0; l2o |> f0]|> Seq.append (slo1 |> Seq.map f1) |> Seq.append (slo2 |> Seq.map f1) |> Seq.choose id |> Seq.distinct |> Seq.toArray // order does not matter
        //let sampleExponents = [l1o |> f0; l2o |> f0] |> Seq.choose id |> Seq.distinct |> Seq.toArray // order does not matter
        invariantm (sampleExponents.Length = 1) (sprintf "Inconsistent sample exponents. %A" sampleExponents) "b8269ff8-55f6-4ddc-aacc-7a584d46f598"

        // get sample exponent (for result layer)
        let sampleExponentAtResultLevel = sampleExponents |> Array.exactlyOne

        // compute result 
        let getWinAtChildLevel (lo : ILayer option) = match lo with | Some l -> l.SampleWindowAtChildLevel | None -> Box2l.Invalid
        let w1 = l1o |> getWinAtChildLevel
        let w2 = l2o |> getWinAtChildLevel
        let ws1 = slo1 |> Seq.choose id |> Seq.map (fun x -> x.SampleWindow) |> Box2l
        let ws2 = slo2 |> Seq.choose id |> Seq.map (fun x -> x.SampleWindow) |> Box2l
        
        let finalWindowAtChildLevel = Box2l(w1, w2, ws1, ws2)
        
        let boundsAtChildLevel = bounds.GetBoundsForExponent(sampleExponentAtResultLevel - 1)
        invariantm (boundsAtChildLevel.Contains finalWindowAtChildLevel) 
                   (sprintf "Final window (%A) is out of bounds (%A)." finalWindowAtChildLevel boundsAtChildLevel) 
                   "d3a934ed-5e51-45e1-ad97-a28ee8ca575b"

        let layersInOrder = 
            match domination with
            | FirstDominates  | MoreDetailedOrFirst  -> seq { yield l2o; yield! slo2; yield l1o; yield! slo1 } |> Seq.choose id |> Seq.toArray
            | SecondDominates | MoreDetailedOrSecond -> seq { yield l1o; yield! slo1; yield l2o; yield! slo2 } |> Seq.choose id |> Seq.toArray

        composeLayersInOrder def bounds sampleExponentAtResultLevel finalWindowAtChildLevel layersInOrder

    /// all parts must have same layer set ...
    let private createLayers (bounds : Cell2d) (domination : Dominance)
                             (l1o : ILayer[] option) (slo1 : array<ILayer[] option>)
                             (l2o : ILayer[] option) (slo2 : array<ILayer[] option>) : ComposedLayerResult[] =

        // ensure that all parts have the same layers ...
        let getLayerSemantics (x : ILayer[]) = x |> Seq.map (fun x -> x.Def) |> Set.ofSeq
        let semsets = [l1o; l2o] |> Seq.append slo1 |> Seq.append slo2 |> Seq.choose id |> Seq.map getLayerSemantics |> Seq.distinct |> Seq.toArray // order does not matter
        invariantm (semsets.Length = 1) 
                   (sprintf "Parts have different layers. %A." semsets) "53fea9ea-521b-4eaa-85d4-07169f2d3f26"

        // group by semantic
        let toLayerMap (ls : ILayer[] option) = match ls with | Some ls -> ls |> Seq.map (fun l -> (l.Def, l)) |> Map.ofSeq | None -> Map.empty
        let l1om  = l1o  |>           toLayerMap
        let slo1m = slo1 |> Array.map toLayerMap
        let l2om  = l2o  |>           toLayerMap
        let slo2m = slo2 |> Array.map toLayerMap

        let semantics = semsets |> Seq.exactlyOne
        let result = 
            semantics 
            |> Seq.map (fun sem ->
                let f = Map.tryFind sem
                let arg0 = l1om  |>           f
                let arg1 = slo1m |> Array.map f
                let arg2 = l2om  |>           f
                let arg3 = slo2m |> Array.map f
                let r = createLayer bounds sem domination arg0 arg1 arg2 arg3
                r
                )
            |> Seq.toArray

        result

    /// all parts must have same semantic ...
    let private mergeChildLayer (bounds : Cell2d) (def : Durable.Def) (slo : array<ILayer option>) 
                            : ComposedLayerResult =

        invariant (slo |> Seq.choose id |> Seq.forall (fun x -> x.Def = def)) "dc42eeb2-313d-46c8-a37c-64028860fa2d"

        // ensure that all parts have consistent sample exponents ...
        let f1 = Option.map (fun (x : ILayer) -> x.SampleExponent + 1)
        let sampleExponents = slo |> Seq.map f1 |> Seq.choose id |> Seq.distinct |> Seq.toArray // order does not matter
        invariantm (sampleExponents.Length = 1) (sprintf "Inconsistent sample exponents. %A" sampleExponents) "574de6be-e3e4-4449-91e0-c30da21b1b72"

        // get sample exponent (for result layer)
        let sampleExponentAtResultLevel = sampleExponents |> Array.exactlyOne

        // compute result 
        let getWinAtChildLevel (lo : ILayer option) = match lo with | Some l -> l.SampleWindowAtChildLevel | None -> Box2l.Invalid
        let ws = slo |> Seq.choose id |> Seq.map (fun x -> x.SampleWindow) |> Box2l
        
        let finalWindowAtChildLevel = ws
        
        let boundsAtChildLevel = bounds.GetBoundsForExponent(sampleExponentAtResultLevel - 1)
        invariantm (boundsAtChildLevel.Contains finalWindowAtChildLevel) 
                   (sprintf "Final window (%A) is out of bounds (%A)." finalWindowAtChildLevel boundsAtChildLevel) 
                   "f6369b37-4e38-4214-888b-85a6daca9bca"

        composeLayersInOrder def bounds sampleExponentAtResultLevel finalWindowAtChildLevel (slo |> Seq.choose id |> Seq.toArray)

    /// all parts must have same layer set ...
    let private mergeChildLayers (bounds : Cell2d) (slo : array<ILayer[] option>) : ComposedLayerResult[] =

        // ensure that all parts have the same layers ...
        let getLayerSemantics (x : ILayer[]) = x |> Seq.map (fun x -> x.Def) |> Set.ofSeq
        let semsets = slo |> Seq.choose id |> Seq.map getLayerSemantics |> Seq.distinct |> Seq.toArray // order does not matter
        invariantm (semsets.Length = 1) 
                   (sprintf "Parts have different layers. %A." semsets) "671a4567-2720-4082-9373-b5edc8311ee9"

        // group by semantic
        let toLayerMap (ls : ILayer[] option) = match ls with | Some ls -> ls |> Seq.map (fun l -> (l.Def, l)) |> Map.ofSeq | None -> Map.empty
        let slom = slo |> Array.map toLayerMap

        let semantics = semsets |> Seq.exactlyOne
        let result = 
            semantics 
            |> Seq.map (fun sem ->
                let f = Map.tryFind sem
                let arg = slom |> Array.map f
                let r = mergeChildLayer bounds sem arg
                r
                )
            |> Seq.toArray

        result









    

    type CellRelation =
        | DisjointCells
        | PartiallyOverlappingCells
        | IdenticalCells
        | FirstCellContainsSecond
        | SecondCellContainsFirst

    let cellRelationOf (a : Cell2d) (b : Cell2d) =
        if a.Intersects(b) then
            match a.Contains(b), b.Contains(a) with
            | false, false -> PartiallyOverlappingCells
            | false, true  -> SecondCellContainsFirst
            | true, false  -> FirstCellContainsSecond
            | true, true   -> IdenticalCells
        else
            DisjointCells



    /// Non-centered tree.
    type Tree =
        | Leaf of QNode
        | Tree of QNode

    type Children = Children of root : Cell2d * subnodes : Tree option[]


    module Tree =

        let ofQNode (root : QNode) : Tree =
            invariant (not(root.Cell.IsCenteredAtOrigin)) "40e61038-1cda-4b27-8cc8-f23c13878b46"
            if root.IsLeafNode then Leaf(root) else Tree(root)

        let inline qnode n = match n with | Leaf n | Tree n -> n
        let inline cell n = match n with | Leaf n | Tree n -> n.Cell
        let inline isLeaf n = (qnode n).IsLeafNode
        let inline isCentered n = (cell n).IsCenteredAtOrigin
        let inline exactBounds n = (qnode n).ExactBoundingBox
        let inline splitLimitExponent n = (qnode n).SplitLimitExponent

        let tryGetChildren n : Children option =
            match (qnode n).SubNodes with
            | Some ns -> Children(cell n, ns |> Array.map(fun n -> n.TryGetInMemory()) |> Array.map(Option.map(ofQNode))) |> Some
            | None    -> None

    type Tree with
        member this.Cell with get() = Tree.cell this
        member this.ExactBounds with get() = Tree.exactBounds this
        member this.SplitLimitExponent with get() = Tree.splitLimitExponent this


    /// Centered tree.
    type CenterTree =
        | CenterLeaf of QNode 
        | CenterTree of QNode

    type CenterChildren = CenterChildren of root : Cell2d * subnodes : Tree option[]



    module CenterTree =

        let ofQNode (root : QNode) : CenterTree =
            invariant (root.Cell.IsCenteredAtOrigin) "1553b069-5e00-429f-932e-e75a7fe64ede"
            if root.IsLeafNode then CenterLeaf(root) else CenterTree(root)

        let inline qnode n = match n with | CenterLeaf n | CenterTree n -> n
        let inline cell n = match n with | CenterLeaf n | CenterTree n -> n.Cell
        let inline isLeaf n = (qnode n).IsLeafNode
        let inline isCentered n = (cell n).IsCenteredAtOrigin
        let inline exactBounds n = (qnode n).ExactBoundingBox
        let inline splitLimitExponent n = (qnode n).SplitLimitExponent

        let tryGetChildren n : CenterChildren option =
            match (qnode n).SubNodes with
            | Some ns -> CenterChildren(cell n, ns |> Array.map(fun n -> n.TryGetInMemory()) |> Array.map(Option.map(Tree.ofQNode))) |> Some
            | None    -> None

    type CenterTree with
        member this.Cell with get() = CenterTree.cell this

    /// Centered or non-centered tree.
    type AnyTree =
        | NonCentered of Tree
        | Centered of CenterTree

    module AnyTree =

        let ofQNode (root : QNode) : AnyTree =
            if root.Cell.IsCenteredAtOrigin then
                root |> CenterTree.ofQNode |> Centered
            else 
                root |> Tree.ofQNode |> NonCentered

        let inline qnode n = match n with | NonCentered n -> Tree.qnode n | Centered n    -> CenterTree.qnode n
        let inline cell n = (qnode n).Cell
        let inline isLeaf n = (qnode n).IsLeafNode
        let inline isCentered n = (cell n).IsCenteredAtOrigin
        let inline ebbox n = (qnode n).ExactBoundingBox
        let inline ebboxContains first second = (ebbox first).Contains(ebbox second)

    type AnyTree with
        member this.ExactBoundingBox with get() = AnyTree.ebbox this


    type SubtreeRelation =
        /// non-centered child (e-1)
        | DirectChild    of child : Tree * qi : int
        /// non-centered child (e-n where n>1)
        | IndirectChild  of child : Tree * qi : int
        /// centered parent Cell2d(e) -> centered child Cell2d(e-1)
        | NestedDirect   of child : CenterTree
        /// centered parent Cell2d(e) -> centered child Cell2d(e-n where n>1)
        | NestedIndirect of child : CenterTree
        /// child is not in a subtree relation with respect to root
        | InvalidSubtree of child : AnyTree * root : Cell2d

    module SubtreeRelation =

        let ofTree (root : Cell2d) (child : Tree) : SubtreeRelation =
            let c = child.Cell 
            if root.Exponent > c.Exponent then
                let qi = root.GetQuadrant(c)
                if qi.HasValue then
                    let isDirect = c.Exponent + 1 = root.Exponent
                    if isDirect then DirectChild(child, qi.Value) else IndirectChild(child, qi.Value)
                else
                    InvalidSubtree(NonCentered child, root)
            else
                InvalidSubtree(NonCentered child, root)

        let ofAnyTree (root : Cell2d) (child : AnyTree) : SubtreeRelation =
            let c = AnyTree.cell child
            if root.Exponent > c.Exponent then
                let isDirect = c.Exponent + 1 = root.Exponent
                let qi = root.GetQuadrant(c)
                match root.IsCenteredAtOrigin, qi.HasValue, child with
                | _,     true,  NonCentered x -> if isDirect then DirectChild(x, qi.Value) else IndirectChild(x, qi.Value)
                | true,  false, Centered    x -> if isDirect then NestedDirect(x)          else NestedIndirect(x)
                | _ -> InvalidSubtree(child, root)
            else
                InvalidSubtree(child, root)
            

    type MergeRelation =
        | MergeWinner       of winner : AnyTree * loser : AnyTree
        | MergeDisjoint     of commonRootCell : Cell2d * first : SubtreeRelation * second : SubtreeRelation
        | SameRoot          of first : AnyTree * second : AnyTree * dominance : Dominance
        | MergeSubtree      of parent : AnyTree * child : SubtreeRelation * dominance : Dominance
        | MergeNested       of centeredLarge : CenterTree * centeredSmall : CenterTree * dominance : Dominance
        | MergeOverlapping  of centered : CenterTree * other : Tree * dominance : Dominance

    module MergeRelation =

        let rec create (dominance : Dominance) (first : AnyTree) (second : AnyTree) : MergeRelation =

            let c1 = AnyTree.cell first
            let c2 = AnyTree.cell second

            //let failWithConfiguration error =
            //    sprintf "Configuration (first=%A, second=%A, dominance=%A). Error %s." 
            //            c1 c2 dominance error |> failwith

            if dominance = FirstDominates && (AnyTree.ebboxContains first second) then
                MergeWinner (first, second)
            elif dominance = SecondDominates && (AnyTree.ebboxContains second first) then
                MergeWinner (second, first)
            else
                match cellRelationOf c1 c2 with
        
                | DisjointCells ->
                    let rc = c1.Union(c2)
                    let rel  = SubtreeRelation.ofTree    rc
                    let rel' = SubtreeRelation.ofAnyTree rc
                    match first, second with
                    | NonCentered a, NonCentered b -> MergeDisjoint (rc, rel a, rel b)
                    | Centered    a, NonCentered b -> MergeDisjoint (rc, rel' first, rel b)
                    | NonCentered a, Centered    b -> MergeDisjoint (rc, rel a, rel' second)
                    | _ -> sprintf "Disjoint cells required: %A %A. Error 92660bcf-6c81-451a-850e-f6dc99e96286." (AnyTree.cell first) (AnyTree.cell second) |> failwith
        
                | PartiallyOverlappingCells ->
                    // can happen with one centered cell and another non-centered cell
                    match first, second with
                    | Centered a, NonCentered b -> MergeOverlapping(centered = a, other = b, dominance = dominance)
                    | NonCentered a, Centered b -> MergeOverlapping(centered = b, other = a, dominance = dominance.Flipped)
                    | _ -> failwith "Error 3d1df6b5-bedf-4ade-b2c0-47d74d8841d9."
        
                | SecondCellContainsFirst ->
                    create dominance.Flipped second first
                
                | FirstCellContainsSecond ->
                    if dominance = FirstDominates && (AnyTree.ebbox first).Contains((AnyTree.ebbox second)) then
                        MergeWinner (first, second)
                    else
                        match first, second with
                        | Centered    _, NonCentered _
                        | NonCentered _, NonCentered _ -> MergeSubtree (first, second |> SubtreeRelation.ofAnyTree c1, dominance)
                        | Centered    a, Centered    b -> MergeNested  (a, b, dominance)
                        | NonCentered a, Centered    b -> failwith "todo: create NonCentered a, Centered b"

                | IdenticalCells ->
                    invariant (c1 = c2) "8c6566b8-66b8-492d-8e30-05fefd210cb7"
                    SameRoot (first, second, dominance)
                        
       
    
    let private ofIndexedSubnodes (root : Cell2d) (children : (int * Tree) list) : (Cell2d * Tree option[]) =
        invariant (children.Length > 0 && children.Length <= 4)                 "0cf7c961-6e82-4117-935c-b83043fc0465"
        let ns = Array.create 4 None
        for (i, n) in children do
            invariant (i >= 0 && i < 4)                                         "bfb84957-3024-443e-b026-3e7db0f45c74"
            invariant (ns.[i].IsNone)                                           "1ccd65be-6c4e-4135-af5d-d25af3aed1da"
            if root.IsCenteredAtOrigin then
                let c = n.Cell
                invariant (c.Exponent + 1 = root.Exponent && c.TouchesOrigin)   "bb213b45-6f28-4ca4-9254-f2cebbf12b00"
            else
                invariant ((Tree.cell n).Parent = root)                         "cd9bd921-74aa-4719-89f5-ca28a7bd40f5"
            ns.[i] <- Some n
        (root, ns)

    
    module Children =

        let ofIndexedSubnodes (root : Cell2d) (children : (int * Tree) list) : Children =
            invariant (not root.IsCenteredAtOrigin) "a89439d5-1490-4120-b451-50be51a0e262"
            ofIndexedSubnodes root children |> Children

        let extract  x = match x with | Children (a,b) -> (a,b)
        
        let splitLimitExponent x =
            extract x |> snd |> Seq.choose(Option.map(fun x -> x.SplitLimitExponent)) |> Seq.head

        let merge dom mf c1 c2 =
            let (root1, ns1) = extract c1
            let (root2, ns2) = extract c2
            invariant (root1 = root2) "88703a21-e46e-4f27-87cf-410544c9c04b"
            let f a b = match a, b with 
                        | None, None -> None | Some x, None | None, Some x -> Some x
                        | Some x, Some y ->
                            let ebx = Tree.exactBounds x
                            let eby = Tree.exactBounds y
                            if   dom = FirstDominates && ebx.Contains(eby) then Some x
                            elif dom = SecondDominates && eby.Contains(ebx) then Some y
                            else mf x y |> Some
            Children(root1, Array.map2 f ns1 ns2)

    
    module CenterChildren =

        let ofIndexedSubnodes (root : Cell2d) (children : (int * Tree) list) : CenterChildren =
            invariant (root.IsCenteredAtOrigin) "5beee604-5064-460f-9b81-cf98aaca0b3b"
            let cexp = children |> Seq.map (fun (_,t) -> t.Cell.Exponent) |> Seq.distinct |> Seq.exactlyOne
            invariantm (root.Exponent = cexp + 1) (sprintf "root exp = %d; child exp = %d" root.Exponent cexp) "135ace7c-3d64-41ea-8ec9-6a440a2ad5d5"
            ofIndexedSubnodes root children |> CenterChildren

        let extract x = match x with | CenterChildren (a,b) -> (a,b)

        let check x = if extract x |> snd |> Seq.choose id |> Seq.isEmpty then
                        failwith "Empty center children. Error 11effdb1-d6cb-4168-945e-3e32840603b0."
                      else
                        x

        let splitLimitExponent x =
            extract x |> snd |> Seq.choose(Option.map(fun x -> x.SplitLimitExponent)) |> Seq.head

        let merge dom mf (c1 : CenterChildren) (c2 : CenterChildren) : CenterChildren =
            let (root1, ns1) = extract c1
            let (root2, ns2) = extract c2
            invariant (root1 = root2) "2720c598-05a9-4a2d-80ec-78caf82cf3d7"
            let f a b = match a, b with 
                        | None, None -> None | Some x, None | None, Some x -> Some x
                        | Some x, Some y ->
                            let ebx = Tree.exactBounds x
                            let eby = Tree.exactBounds y
                            if   dom = FirstDominates && ebx.Contains(eby) then Some x
                            elif dom = SecondDominates && eby.Contains(ebx) then Some y
                            else mf x y |> Some
            CenterChildren(root1, Array.map2 f ns1 ns2)

    type CenterChildren with
        member this.Cell with get() = this |> CenterChildren.extract |> fst


    type LeafNode       = LeafNode of node : AnyTree

    module LeafNode =

        let inline ofTree (n : Tree) : LeafNode =
            invariant (Tree.isLeaf n) "b4b2cda5-5ea8-4e0a-883d-c422a6afd760"
            n |> NonCentered |> LeafNode

        let inline ofCenterTree (n : CenterTree) : LeafNode =
            invariant (CenterTree.isLeaf n) "8ca00fe1-a5df-4fbd-99d5-a109d43cf6bc"
            n |> Centered |> LeafNode

        let inline toAnyTree (n : LeafNode) : AnyTree = match n with | LeafNode n -> n

        let inline qnode n = n |> toAnyTree |> AnyTree.qnode
        let inline cell n = (qnode n).Cell
        let inline ebbox n = (qnode n).ExactBoundingBox


    type InnerNode      = InnerNode of node : AnyTree

    module InnerNode =
        let inline ofTree (n : Tree) : InnerNode =
            invariant (not(Tree.isLeaf n)) "cb91131f-2d2c-4fde-ac8c-c2ad715ad7ca"
            n |> NonCentered |> InnerNode

        let inline ofCenterTree (n : CenterTree) : InnerNode =
            invariant (not(CenterTree.isLeaf n)) "a0874e5c-b42f-4310-8b02-cdf44c8ea6f4"
            n |> Centered |> InnerNode

        let inline toAnyTree (n : InnerNode) : AnyTree = match n with | InnerNode n -> n

        let inline qnode n = n |> toAnyTree |> AnyTree.qnode
        let inline cell n = (qnode n).Cell

    
    
    let private split (leaf : LeafNode) : Children =
        let n = leaf |> LeafNode.qnode
        let rc = n.Cell
        let qs = rc.Children
        let sls = n.SplitLayers ()
        let ns = Array.map2 (fun (q : Cell2d) ls -> match ls with
                                                    | Some ls ->
                                                       let ebb = q.BoundingBox.Intersection(LeafNode.ebbox leaf)
                                                       QNode(ebb, q, n.SplitLimitExponent, ls) |> Tree.ofQNode |> Some
                                                    | None -> None
                 ) qs sls
        Children(rc, ns)

    let private splitCentered (node : CenterTree) : CenterChildren =
        let qnode = CenterTree.qnode node
        let ns = qnode.SplitCenteredNodeIntoQuadrantNodesAtSameLevel () |> Array.map(Option.map(Tree.ofQNode))
        CenterChildren(root=qnode.Cell.Parent, subnodes=ns) |> CenterChildren.check
        

    
    let private createNodeFromChildren (children : Children) : Tree =
    
        let sle = Children.splitLimitExponent children
        let (root, ns) = Children.extract children

        let qns = ns |> Array.map (Option.map(Tree.qnode))
        let qnsls = qns |> Array.map (Option.map (fun x -> x.Layers))
        let lrs = mergeChildLayers root qnsls
        let ls = lrs |> Array.map (fun r -> r.Layer)

        let bebbs = qns |> Seq.choose id |> Seq.map (fun x -> x.ExactBoundingBox) |> Seq.toArray
        let desiredEbb = Box2d(bebbs)

        QNode(desiredEbb, root, sle, ls, qns) |> Tree

    let private createNodeFromChildren' (children : CenterChildren) : CenterTree =
    
        let sle = CenterChildren.splitLimitExponent children
        let (root, ns) = CenterChildren.extract children
        let snexp = ns |> Seq.choose(Option.map(fun x -> x.Cell.Exponent)) |> Seq.distinct |> Seq.exactlyOne
        invariant (ns |> Array.exists(Option.isSome)) "e81ff857-15b3-415e-abb7-427f04146fe3"
        invariant (root.Exponent = snexp + 1) "295b29f1-6a9e-4cb0-af7b-f8ad186569a2"

        let qns = ns |> Array.map (Option.map(Tree.qnode))
        let qnsls = qns |> Array.map (Option.map (fun x -> x.Layers))
        let lrs = mergeChildLayers root qnsls
        let ls = lrs |> Array.map (fun r -> r.Layer)

        let bebbs = qns |> Seq.choose id |> Seq.map (fun x -> x.ExactBoundingBox) |> Seq.toArray
        let desiredEbb = Box2d(bebbs)

        QNode(desiredEbb, root, sle, ls, qns) |> CenterTree

    /// Attaches a parent node to n and creates LoD layers for parent.
    let private growParent (n : Tree) : Tree =
        let cell = Tree.cell n
        let parentCell = cell.Parent
        let qi = parentCell.GetQuadrant(cell).Value
        let ns = Array.create 4 None
        ns.[qi] <- Some n
        let children = Children(root = cell.Parent, subnodes = ns)
        createNodeFromChildren children

    /// Attaches a parent node to n and creates LoD layers for parent.
    let private growParent' (n : CenterTree) : CenterTree =
        splitCentered n |> createNodeFromChildren'


    let private createNodeFromLeafs (dom : Dominance) (a : LeafNode) (b : LeafNode) : LeafNode =

        let qa = LeafNode.qnode a
        let qb = LeafNode.qnode b
        invariant (qa.Cell = qb.Cell) "0de68b42-3605-4791-9ca9-0688cc5dcf10"

        let lrs = createLayers qa.Cell dom (Some qa.Layers) Array.empty (Some qb.Layers) Array.empty
        let ls = lrs |> Array.map (fun r -> r.Layer)
        let ebb = Box2d(LeafNode.ebbox a, LeafNode.ebbox b)
        QNode(ebb, qa.Cell, qa.SplitLimitExponent, ls) |> Leaf |> LeafNode.ofTree

    let private createNodeFromLeafAndChildren (dom : Dominance) (a : LeafNode) (b : Children) : InnerNode =
        let qa = LeafNode.qnode a
        let (rootb, qbns) = Children.extract b
        invariant (qa.Cell = rootb) "192bf088-592a-46b6-8910-bf99718e395b"

        let qbns = qbns |> Array.map (Option.map(Tree.qnode))
        let qbnsls = qbns |> Array.map (Option.map (fun x -> x.Layers))
        let lrs = createLayers qa.Cell dom (Some qa.Layers) Array.empty None qbnsls
        let ls = lrs |> Array.map (fun r -> r.Layer)
        
        let aebb = qa.ExactBoundingBox
        let bebbs = qbns |> Seq.choose id |> Seq.map (fun x -> x.ExactBoundingBox) |> Seq.toArray
        let desiredEbb = Box2d(aebb, Box2d(bebbs))

        let n = QNode(desiredEbb, qa.Cell, qa.SplitLimitExponent, ls, qbns)
        
        invariant (n.ExactBoundingBox = desiredEbb) "3f0f2ac0-73c5-4f47-81d2-ceeece30fd5b"

        n |> Tree |> InnerNode.ofTree
            




    

    
    let rec private mergeRec (dom : Dominance) (a : AnyTree) (b : AnyTree) : AnyTree =
        
        (* helpers *)

        
        let growParents (ns : CenterChildren) =
            match ns with | CenterChildren (root, ns) -> CenterChildren(root.Parent, ns |> Array.map(Option.map(growParent)))


        let mergeChildren (dom : Dominance) (ns1 : Children) (ns2 : Children) : Children =
            let mf a' b' = match mergeRec dom (NonCentered a') (NonCentered b') with
                           | NonCentered tree -> tree
                           | _ -> failwith "Invariant 4673872b-d6be-491d-a2a0-b99054afbe65."
            Children.merge dom mf ns1 ns2
            

        let mergeChildren' (dom : Dominance) (ns1 : CenterChildren) (ns2 : CenterChildren) : CenterChildren =
            invariant (ns1.Cell = ns2.Cell) "62a8d91a-3a97-46dd-b69f-aaa02ee98973"
            let mf a' b' = match mergeRec dom (NonCentered a') (NonCentered b') with
                           | NonCentered tree -> tree
                           | _ -> failwith "Invariant 4c6783e3-1299-4a1e-b2d8-ea9f5a15f77a."
            CenterChildren.merge dom mf ns1 ns2

        let mergeOverlapping (dom : Dominance) (centered : CenterTree) (other : Tree) : CenterTree =

            if centered.Cell.Exponent < other.Cell.Exponent then
                let grown = growParent' centered
                let r = mergeRec dom (grown |> Centered) (other |> NonCentered)
                match r with
                | Centered x -> x
                | _ -> failwith "Error 39207884-7188-4b47-a48f-ce27f13fc59b."
            else

                let rc = Cell2d(Box2d(centered.Cell.BoundingBox, other.Cell.BoundingBox))
                invariant (rc.IsCenteredAtOrigin) "1320c782-5905-46c8-9d3a-87e589a52d53"
                invariant (rc.Exponent = other.Cell.Exponent + 1) "7ae77298-6633-4930-a07e-89f165028646"
                
                let quadrantOther = rc.GetQuadrant(other.Cell).Value

                let cns = 
                    (CenterTree.qnode centered).SplitCenteredNodeIntoQuadrantNodesAtSameLevel ()
                    |> Array.map(Option.map(Tree.ofQNode))

                let ns =
                    cns |> Array.mapi(fun i no ->
                        match no, i = quadrantOther with
                        | None, false   -> None
                        | Some n, false -> Some n
                        | None, true    -> Some other
                        | Some n, true  -> 
                            let merged = mergeRec dom (n |> NonCentered) (other |> NonCentered)
                            match merged with
                            | NonCentered m -> Some m
                            | _ -> failwith "Error 680cd32c-9045-4ab1-80e5-141d645212db."
                        )

                invariant (ns |> Seq.choose(Option.map(fun x -> x.Cell.Exponent)) |> Seq.forall(fun e -> e = rc.Exponent - 1)) "bfb006ae-fad8-40ad-b48c-f703787ff24f"

                let cc = CenterChildren(root=rc, subnodes=ns)
                let n = createNodeFromChildren' cc
                n

        let failwith' rel error = sprintf "Invalid merge. %A. Error %s." rel error |> failwith
        
        let checkEbb (x : AnyTree) =
            x
            //let bbActual   = x.ExactBoundingBox
            //let bbImagined = Box2d(a.ExactBoundingBox, b.ExactBoundingBox)
            //invariantm 
            //    (bbActual = bbImagined)
            //    (sprintf "%A = %A" bbActual bbImagined)
            //    "cf1e7718-67e7-4a75-91e5-e651cc1ce39c"
            //x

        let inline leaf x = x |> LeafNode.ofTree
        let inline inner x = x |> InnerNode.ofTree
        let inline splitleaf x = leaf x |> split
        let inline subnodes x = Tree.tryGetChildren x
        let inline subnodes' x = CenterTree.tryGetChildren x

        (* zoo *)
        let relation = MergeRelation.create dom a b
        let result =
            match relation with

            // winner
            | MergeWinner (winner, _)                                           -> winner
            
            // disjoint
            | MergeDisjoint (_ , IndirectChild (x, _), IndirectChild  (y, _))   -> mergeRec dom (x |> growParent |> NonCentered) (y |> growParent |> NonCentered)
            | MergeDisjoint (_ , IndirectChild (x, _), DirectChild    (y, _))   -> mergeRec dom (x |> growParent |> NonCentered) (y               |> NonCentered)
            | MergeDisjoint (_ , IndirectChild (x, _), NestedDirect   (y   ))
            | MergeDisjoint (_ , IndirectChild (x, _), NestedIndirect (y   ))   -> mergeRec dom.Flipped (Centered y) (NonCentered x) 
        
            | MergeDisjoint (_ , DirectChild   (x, _), IndirectChild  (y, _))   -> mergeRec dom (x               |> NonCentered) (y |> growParent |> NonCentered)

            | MergeDisjoint (rc, DirectChild   (x,xi), DirectChild    (y,yi))   -> if rc.IsCenteredAtOrigin then
                                                                                        CenterChildren.ofIndexedSubnodes rc [(xi, x); (yi, y)]
                                                                                        |> createNodeFromChildren' |> Centered                  |> checkEbb
                                                                                   else
                                                                                        Children.ofIndexedSubnodes rc [(xi, x); (yi, y)]
                                                                                        |> createNodeFromChildren  |> NonCentered               |> checkEbb

            | MergeDisjoint (rc, NestedDirect x,       IndirectChild  (y,yi))   -> 
                                                                                   if rc.Exponent = y.Cell.Exponent + 1 then
                                                                                        let ns1 = splitCentered x |> growParents
                                                                                        let ns2 = CenterChildren.ofIndexedSubnodes rc [(yi, y)]
                                                                                        mergeChildren' dom ns1 ns2 |> createNodeFromChildren' |> Centered
                                                                                   else
                                                                                        mergeRec dom (x |> Centered) (y |> growParent |> NonCentered)

            | MergeDisjoint (_ , NestedIndirect x,     IndirectChild  (y, _))   -> mergeRec dom (growParent' x |> Centered) (y |> NonCentered)
                                                                                   |> checkEbb
            
            // collision

            | SameRoot (NonCentered first, NonCentered second, d)               -> invariant (first.Cell = second.Cell) "8746b419-e469-4e57-b6ba-60c3d5aae3b8"
                                                                                   match subnodes first, subnodes second with
                                                                                   | None,     None     -> createNodeFromLeafs           d         (leaf first)  (leaf second) |> LeafNode.toAnyTree    |> checkEbb
                                                                                   | None,     Some ns  -> createNodeFromLeafAndChildren d         (leaf first )  ns           |> InnerNode.toAnyTree   |> checkEbb
                                                                                   | Some ns,  None     -> createNodeFromLeafAndChildren d.Flipped (leaf second)  ns           |> InnerNode.toAnyTree   |> checkEbb
                                                                                   | Some ns1, Some ns2 -> mergeChildren d ns1 ns2 |> createNodeFromChildren |> NonCentered                             |> checkEbb
                                                                                   |> checkEbb
                                                                                   
            | SameRoot (Centered    first, Centered    second, d)               -> invariant (first.Cell = second.Cell) "fc986971-78ae-4fca-a367-eab059e8d68e"
                                                                                   match subnodes' first, subnodes' second with
                                                                                   | None,     None     -> createNodeFromLeafs d (first |> LeafNode.ofCenterTree) (second |> LeafNode.ofCenterTree) |> LeafNode.toAnyTree
                                                                                   | None,     Some ns  -> mergeChildren' d (splitCentered first) (growParents ns)   |> createNodeFromChildren' |> Centered
                                                                                   | Some ns,  None     -> mergeChildren' d (growParents ns) (splitCentered second)  |> createNodeFromChildren' |> Centered
                                                                                   | Some ns1, Some ns2 -> mergeChildren' d ns1 ns2                                  |> createNodeFromChildren' |> Centered
                                                                                   |> checkEbb
                                                                                   
            // subtree
            | MergeSubtree (NonCentered parent,  DirectChild (child, qi), d)    -> 
                                                                                   let qparent = Tree.qnode parent
                                                                                   printfn "%A" qparent.Mapping
                                                                                   let children = Children.ofIndexedSubnodes (Tree.cell parent) [(qi, child)]

                                                                                   let childWindow = (Tree.qnode child).Mapping.Window

                                                                                   let merged =
                                                                                        match subnodes parent with
                                                                                        | None     -> 
                                                                                            let splitParent = splitleaf parent
                                                                                            mergeChildren d splitParent children
                                                                                        | Some ns1 ->
                                                                                            mergeChildren d ns1 children

                                                                                   merged
                                                                                   |> createNodeFromChildren |> NonCentered
                                                                                   |> checkEbb

            | MergeSubtree (Centered parent,     DirectChild (child, qi), d)    -> 
                                                                                   invariant (parent.Cell.Exponent = child.Cell.Exponent + 1) "a66e432b-699a-4b6d-a658-db55c9895963"

                                                                                   let ns2 = CenterChildren.ofIndexedSubnodes parent.Cell [(qi, child)]
                                                                                   
                                                                                   let merged = 
                                                                                        match subnodes' parent with
                                                                                        | None ->
                                                                                                let ns1LeveledUp = splitCentered parent // c needs to level up +1 to preserve layer resolution
                                                                                                invariant (ns1LeveledUp.Cell.Exponent = parent.Cell.Exponent + 1) "a26d4664-3a30-4233-821e-f53be450dff6"
                                                                                                let ns2LeveledUp = growParents ns2
                                                                                                let merged = mergeChildren' d ns1LeveledUp ns2LeveledUp
                                                                                                merged
                                                                                        | Some ns1 ->
                                                                                                let merged = mergeChildren' d ns1 ns2
                                                                                                merged

                                                                                   let result = merged |> createNodeFromChildren'
                                                                                   result |> Centered
                                                                                                     

            | MergeSubtree (parent, IndirectChild (child,  _), d)               -> mergeRec d parent (child |> growParent |> NonCentered) 
                                                                                   |> checkEbb
            

            | MergeSubtree (parent, NestedDirect (child),      d)               -> match parent with
                                                                                   | NonCentered x ->
                                                                                       sprintf "Expected centered parent, but %A. Error 841b5fa0-c130-457b-9632-9d3e637a3377." x |> failwith
                                                                                   | Centered parent ->
                                                                                       let ns1 = splitCentered parent
                                                                                       let ns2 = splitCentered child |> growParents
                                                                                       mergeChildren' d ns1 ns2 |> createNodeFromChildren' |> Centered
                                                                                   |> checkEbb
                                                                                    

            | MergeSubtree (parent, NestedIndirect (child),    dominance)       -> invariant (AnyTree.isCentered parent) "3c46c7a4-660a-4693-b4c1-ea7d4c46d7b3"
                                                                                   mergeRec dominance parent (child |> growParent' |> Centered)
                                                                                   |> checkEbb
                                                                                   

            // nested
            | MergeNested (large, small, d)                                     -> mergeRec d (Centered large) (small |> growParent' |> Centered)
                                                                                   |> checkEbb


            // partially overlapping
            | MergeOverlapping (centered, other, d)                             -> mergeOverlapping d centered other |> Centered |> checkEbb
            

            // invalid
            | SameRoot (Centered    _, NonCentered _, _)                        -> failwith' relation "b29d2af5-376f-467a-99eb-834135d68d4a"
            | SameRoot (NonCentered _, Centered    _, _)                        -> failwith' relation "d14f075a-7dd7-4b3b-a05b-ea4379a49a99"
            | MergeDisjoint (_ , DirectChild _,    NestedDirect  _)             -> failwith' relation "4cf244b3-28b0-4f67-a723-f090ad9ba9eb"
            | MergeDisjoint (_ , DirectChild _,    NestedIndirect  _)           -> failwith' relation "18852a24-7ea2-4676-a85d-e5d53abc6e5e"
            | MergeDisjoint (_ , NestedDirect _,   DirectChild _)               -> failwith' relation "cbad6def-c0fb-4c08-9c81-2f20e24b62db"
            | MergeDisjoint (_ , NestedDirect _,   NestedDirect _)              -> failwith' relation "b77eb555-1014-42a2-9610-2a3834a1b0f7"
            | MergeDisjoint (_ , NestedDirect _,   NestedIndirect _)            -> failwith' relation "576b5b2f-53f0-4657-9473-71b520685df2"
            | MergeDisjoint (_ , NestedIndirect _, NestedDirect _)              -> failwith' relation "77fa2ed2-c160-4e6e-9b4f-c846c43951c4"
            | MergeDisjoint (_ , NestedIndirect _, NestedIndirect _)            -> failwith' relation "3d988409-2238-4a5e-887e-7fae6394db12"
            | MergeDisjoint (_ , NestedIndirect _, DirectChild _)               -> failwith' relation "3f0cf471-74a1-49cd-bf6a-89175d2bac73"
            | MergeDisjoint (_, InvalidSubtree _, _)                            -> failwith' relation "4cc83f47-ffea-4f89-9aa2-b08dbc9bd5a2"
            | MergeDisjoint (_, _, InvalidSubtree _)                            -> failwith' relation "7f06b094-6493-4216-bb4c-69ae01f67ece"
            | MergeSubtree  (_, InvalidSubtree _, _)                            -> failwith' relation "429dcc39-1dae-48f8-8670-d5e6ed2c24f1"

        result |> checkEbb
        

    /// Immutable merge.
    let merge (dominance : Dominance) (first : QNodeRef) (second : QNodeRef) : QNodeRef =

        match first.TryGetInMemory(), second.TryGetInMemory() with
        | None,   None   -> NoNode
        | Some _, None   -> first
        | None,   Some _ -> second
        | Some a, Some b -> 
            
            invariantm (a.SplitLimitExponent = b.SplitLimitExponent)
                "Cannot merge quadtrees with different split limits."   "6222eb6b-a7aa-43c1-9323-e28d6275696b"

            let result = mergeRec dominance (AnyTree.ofQNode a) (AnyTree.ofQNode b)
            
            result |> AnyTree.qnode |> InMemoryNode
