namespace Aardvark.Geometry.Quadtree

open Aardvark.Base

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

    /// Creates new node from two sets of subnodes.
    let private create (cell : Cell2d) (domination : Dominance) 
                       (n1o : QNode option) (sno1 : QNode option[])
                       (n2o : QNode option) (sno2 : QNode option[]) =

        // ensure that all subnodes are correctly placed ... 
        for qi = 0 to 3 do
            let q = cell.GetQuadrant(qi)
            invariant (match sno1.[qi] with | Some n -> n.Cell = q | None -> true) "764bc804-3f9f-4031-9ec0-7ffe04531328"
            invariant (match sno2.[qi] with | Some n -> n.Cell = q | None -> true) "50d449be-0a7e-47bc-a3dc-0cc5bfb4fc59"

        // ensure that all subnodes have same resolution and split limit ...
        invariantm 
            ((sno1 |> Seq.append sno2 |> Seq.choose id |> Seq.map (fun n -> (n.SampleExponent, n.SplitLimitExponent))) |> Seq.distinct |> Seq.length < 2)
            "Subnodes have different resolution or split limit." "018b42e9-34a5-4791-94d2-374d7e246f6c"
        
        failwith "todo implement"


    /// Merge nodes that do not overlap.
    let private mergeNonOverlappingNodes (domination : Dominance) (nr1 : QNodeRef) (nr2 : QNodeRef) : QNodeRef =
        match nr1.TryGetInMemory(), nr2.TryGetInMemory() with
        | None,    None    -> NoNode
        | Some _,  None    -> nr1
        | None,    Some _  -> nr2
        | Some n1, Some n2 ->

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
            create rc domination None sno1 None sno2

    /// Merge nodes, where one node is a subnode of the other, or both nodes are the same.
    let rec private mergeOverlappingNodes (domination : Dominance) (firstRef : QNodeRef) (secondRef : QNodeRef) : QNodeRef =

        match firstRef.TryGetInMemory(), secondRef.TryGetInMemory() with
        | None,    None    -> NoNode
        | Some _,  None    -> firstRef
        | None,    Some _  -> secondRef
        | Some n1, Some n2 ->

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


            create rc domination n1o sno1 n2o sno2

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
