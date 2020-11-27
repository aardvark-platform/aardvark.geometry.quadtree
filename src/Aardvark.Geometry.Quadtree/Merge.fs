namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data

(*
    Merge.
*)

/// Which data dominates in merge operations.
type Dominance = 
    | FirstDominates 
    | SecondDominates 
    | MoreDetailedDominates
with
    member this.Flipped with get() =
        match this with 
        | FirstDominates        -> SecondDominates
        | SecondDominates       -> FirstDominates
        | MoreDetailedDominates -> MoreDetailedDominates
        

module Merge =

    /// all parts must have same semantic ...
    let private createLayer (def : Durable.Def) (domination : Dominance)
                            (l1o : ILayer option) (slo1 : array<ILayer option>)
                            (l2o : ILayer option) (slo2 : array<ILayer option>) 
                            : ILayer =

        invariant (l1o.IsNone || l1o.Value.Def = def) "c8931d12-5472-4f11-ab64-b5c49e1ebbb5"
        invariant (l2o.IsNone || l2o.Value.Def = def) "c9e6f48e-763c-482a-8cb2-909490b978f2"
        invariant (slo1 |> Seq.choose id |> Seq.forall (fun x -> x.Def = def)) "5739daef-3c13-4409-9ba0-78a5a311634c"
        invariant (slo2 |> Seq.choose id |> Seq.forall (fun x -> x.Def = def)) "c2aaadbd-86aa-4115-9d04-42e50ab0f414"

        // ensure that all parts have consistent sample exponents ...
        let f0 = Option.map (fun (x : ILayer) -> x.SampleExponent)
        let f1 = Option.map (fun (x : ILayer) -> x.SampleExponent + 1)
        let sampleExponents = [l1o |> f0; l2o |> f0]|> Seq.append (slo1 |> Seq.map f1) |> Seq.append (slo2 |> Seq.map f1) |> Seq.choose id |> Seq.distinct |> Seq.toArray
        invariantm (sampleExponents.Length = 1) (sprintf "Inconsistent sample exponents. %A" sampleExponents) "b8269ff8-55f6-4ddc-aacc-7a584d46f598"

        // get sample exponent (for result layer)
        let sampleExponent = sampleExponents |> Array.exactlyOne

        // compute result 


        let hasSubLayers1 = slo1 |> Array.exists (fun x -> x.IsSome)
        let hasSubLayers2 = slo2 |> Array.exists (fun x -> x.IsSome)
        let mapWindowToChildLevel (w : Box2l) = Box2l(w.Min * 2L, w.Max * 2L)

        match l1o, l2o with

        | Some l1, Some l2 ->
            let w1 = l1.SampleWindow |> mapWindowToChildLevel
            let w2 = l2.SampleWindow |> mapWindowToChildLevel
            let w = Box2l(w1,w2)
            sprintf "todo A: %A + %A -> %A" w1 w2 w |> failwith

        | Some l1, None    ->
            failwith "todo B"

        | None,    Some l2 ->
            failwith "todo C"

        | None,    None    ->
            failwith "todo D"

        sprintf "todo createLayer (%A)" def.Name |> failwith



    /// all parts must have same layer set ...
    let private createLayers (domination : Dominance)
                             (l1o : ILayer[] option) (slo1 : array<ILayer[] option>)
                             (l2o : ILayer[] option) (slo2 : array<ILayer[] option>) : ILayer[] =

        // ensure that all parts have the same layers ...
        let getLayerSemantics (x : ILayer[]) = x |> Seq.map (fun x -> x.Def) |> Set.ofSeq
        let semsets = [l1o; l2o] |> Seq.append slo1 |> Seq.append slo2 |> Seq.choose id |> Seq.map getLayerSemantics |> Seq.distinct |> Seq.toArray
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
                let r = createLayer sem domination arg0 arg1 arg2 arg3
                r
                )
            |> Seq.toArray

        result


    /// Creates new node from two nodes.
    let private create (cell : Cell2d) (splitLimitExponent : int) (domination : Dominance) 
                       (n1o : QNode option) (sno1 : QNode option[])
                       (n2o : QNode option) (sno2 : QNode option[]) =

        // ensure that optional root nodes coincide with specified root cell
        let check s (n : QNode option) = 
            match n with 
            | None -> ()
            | Some n -> invariantm (n.Cell = cell) 
                            (sprintf "%s node (%A) does not coincide with specified root (%A)." s n.Cell cell) 
                            "3294c70f-7a89-4f75-83d5-ead39cad9ac6"
        check "1st" n1o
        check "2nd" n2o

        // ensure that all subnodes are correctly placed ... 
        for qi = 0 to 3 do
            let q = cell.GetQuadrant(qi)
            invariant (match sno1.[qi] with | Some n -> n.Cell = q | None -> true) "764bc804-3f9f-4031-9ec0-7ffe04531328"
            invariant (match sno2.[qi] with | Some n -> n.Cell = q | None -> true) "50d449be-0a7e-47bc-a3dc-0cc5bfb4fc59"

        // ensure that all subnodes have same resolution and split limit ...
        invariantm 
            ((sno1 |> Seq.append sno2 |> Seq.choose id |> Seq.map (fun n -> (n.SampleExponent, n.SplitLimitExponent))) |> Seq.distinct |> Seq.length < 2)
            "Subnodes have different resolution or split limit." "018b42e9-34a5-4791-94d2-374d7e246f6c"
        
        // compute originalSampleExponent (as smallest of all parts)
        let ose = [n1o; n2o] |> Seq.append sno1 |> Seq.append sno2 |> Seq.choose id |> Seq.map (fun n -> n.OriginalSampleExponent) |> Seq.min

        // compute node layers
        let l1o = n1o |> Option.map (fun n -> n.Layers)
        let slo1 = sno1 |> Array.map (Option.map (fun n -> n.Layers))
        let l2o = n2o |> Option.map (fun n -> n.Layers)
        let slo2 = sno2 |> Array.map (Option.map (fun n -> n.Layers))
        let layers = createLayers domination l1o slo1 l2o slo2

        // result
        QNode(cell, splitLimitExponent, ose, layers) |> InMemoryNode


    /// Merge nodes that do not overlap.
    let private mergeNonOverlappingNodes (domination : Dominance) (nr1 : QNodeRef) (nr2 : QNodeRef) : QNodeRef =
        match nr1.TryGetInMemory(), nr2.TryGetInMemory() with
        | None,    None    -> NoNode
        | Some _,  None    -> nr1
        | None,    Some _  -> nr2
        | Some n1, Some n2 ->

            invariant (n1.SplitLimitExponent = n2.SplitLimitExponent) "5a057fc7-fe39-4c6e-a759-1a49054c34e7"

            invariantm (QNode.Overlap(n1, n2) |> not)
                (sprintf "Nodes must not overlap. First %A. Second %A" n1.Cell n2.Cell)
                "6ada6e09-ef33-4daf-9b0c-4c6dd30f0087"

            // common root cell
            let rc = Cell2d(Box2d(n1.Cell.BoundingBox, n2.Cell.BoundingBox))
            invariant (rc.Contains n1.Cell && rc.Exponent > n1.Cell.Exponent) "e93e27b4-f9a3-484f-a3fa-6e28cb4e803b"
            invariant (rc.Contains n2.Cell && rc.Exponent > n2.Cell.Exponent) "6e4b0a9a-c059-4ba3-8fde-029482326669"

            // quadrant index for n1 and n2
            let qi1 = rc.GetQuadrant(n1.Cell).Value
            let qi2 = rc.GetQuadrant(n2.Cell).Value

            // init two sets of subnodes for root cell
            let sno1 = Array.create 4 None
            sno1.[qi1] <- (nr1 |> QNode.extendUpTo (rc.GetQuadrant(qi1))).TryGetInMemory().Value |> Some
            let sno2 = Array.create 4 None
            sno2.[qi2] <- (nr2 |> QNode.extendUpTo (rc.GetQuadrant(qi2))).TryGetInMemory().Value |> Some

            // create root node from two sets of subnodes
            create rc n1.SplitLimitExponent domination None sno1 None sno2

    /// Merge nodes, where one node is a subnode of the other, or both nodes are the same.
    let rec private mergeOverlappingNodes (domination : Dominance) (firstRef : QNodeRef) (secondRef : QNodeRef) : QNodeRef =

        match firstRef.TryGetInMemory(), secondRef.TryGetInMemory() with
        | None,    None    -> NoNode
        | Some _,  None    -> firstRef
        | None,    Some _  -> secondRef
        | Some n1, Some n2 ->

            invariant (n1.SplitLimitExponent = n2.SplitLimitExponent) "6b6b74c6-f8dc-4177-93ba-99bea3328332"

            invariantm (QNode.Overlap(n1, n2)) 
                (sprintf "Nodes must overlap. First %A. Second %A" n1.Cell n2.Cell)
                "740fb9b2-de34-4c83-85fd-f4149da25c83"

            // common root cell
            let rc = Cell2d(Box2d(n1.Cell.BoundingBox, n2.Cell.BoundingBox))

            let (n1o, sno1) =
                if rc = n1.Cell then
                    invariant (rc.Exponent >= n2.Cell.Exponent)  "16e4749b-2f55-40a1-90d3-4f07701ae146"
                
                    (Some n1, match n1.SubNodes with
                              | None    -> Array.create 4 None
                              | Some ns -> ns |> Array.map (fun n -> n.TryGetInMemory())
                              )

                else
                    invariant (rc = n2.Cell)                    "51a10ae0-1cdf-47de-ab56-8845a801e141"
                    invariant (rc.Exponent > n1.Cell.Exponent)  "58773c53-749a-478e-9f24-12bc701c4a10"
                
                    let sno1 = Array.create 4 None
                    let qi1 = rc.GetQuadrant(n1.Cell).Value
                    sno1.[qi1] <- (firstRef |> QNode.extendUpTo (rc.GetQuadrant(qi1))).TryGetInMemory().Value |> Some

                    (None, sno1)

            let (n2o, sno2) = 
                if rc = n2.Cell then
                    invariant (rc.Exponent >= n1.Cell.Exponent)  "4f24c550-87aa-4772-9463-ea886f1bb81e"
                
                    (Some n2, match n2.SubNodes with
                              | None    -> Array.create 4 None
                              | Some ns -> ns |> Array.map (fun n -> n.TryGetInMemory())
                              )

                else
                    invariant (rc = n1.Cell)                    "1f9777be-9ea0-4117-beb3-0630c2fd8094"
                    invariant (rc.Exponent > n2.Cell.Exponent)  "f263af3f-d77e-4820-b03c-ef9bd65d089a"
                
                    let sno2 = Array.create 4 None
                    let qi2 = rc.GetQuadrant(n2.Cell).Value
                    sno2.[qi2] <- (secondRef |> QNode.extendUpTo (rc.GetQuadrant(qi2))).TryGetInMemory().Value |> Some

                    (None, sno2)

            create rc n1.SplitLimitExponent domination n1o sno1 n2o sno2

    /// Immutable merge.
    let merge (outOfCore : bool) (domination : Dominance) (firstRef : QNodeRef) (secondRef : QNodeRef) : QNodeRef =

        match firstRef.TryGetInMemory(), secondRef.TryGetInMemory() with
        | None,       None        -> NoNode
        | Some _,     None        -> firstRef
        | None,       Some _      -> secondRef
        | Some first, Some second ->

            if first.SplitLimitExponent <> second.SplitLimitExponent then
                failwith "Cannot merge quadtrees with different split limits. Error 6222eb6b-a7aa-43c1-9323-e28d6275696b."

            if QNode.Overlap(first, second) then
                // one quadtree contains the other (there is no partial overlap in quadtrees)
                mergeOverlappingNodes    domination firstRef secondRef 
            else
                // quadtrees are completely separated
                mergeNonOverlappingNodes domination firstRef secondRef
