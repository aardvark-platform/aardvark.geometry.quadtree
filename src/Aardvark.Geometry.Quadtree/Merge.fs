namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open System

(*
    Merge.
*)

/// Which data dominates in merge operations.
type Dominance = 
    | FirstDominates 
    | SecondDominates 
    | MoreDetailedDominates

module Merge =

    /// FirstDominates -> SecondDominates, SecondDominates -> FirstDominates, MoreDetailedDominates -> MoreDetailedDominates
    let private flipDomination d =
           match d with
           | FirstDominates        -> SecondDominates
           | SecondDominates       -> FirstDominates
           | MoreDetailedDominates -> MoreDetailedDominates

    /// True if nodes overlap.
    let inline private overlapping (a : QNode) (b : QNode) = a.Cell.Intersects(b.Cell)

   


    let private mergeLayers (domination : Dominance) (a : ILayer[]) (b : ILayer[]) : ILayer[] =

        invariantm (a.Length = b.Length) "Mismatch number of layers."                           "d034fed2-aaa8-4e95-851d-48cc364943e9"

        let mutable merged = Map.empty

        let merge (x : ILayer) : unit =
            let def = x.Def
            match Map.tryFind def.Id merged with

            | None                  ->
                merged <- merged |> Map.add x.Def.Id x

            | Some (first : ILayer) ->
                let second = x
                if first.Def.Id <> second.Def.Id then
                    failwith "Invariant b2af0e01-849d-48a2-8026-686dc72176e2."

                let firstWins = match domination with
                                | FirstDominates        -> true
                                | SecondDominates       -> false
                                | MoreDetailedDominates -> first.SampleExponent < second.SampleExponent

                let handleCollision layers =
                    match Layer.Merge layers with
                    | Some z -> merged <- merged |> Map.add z.Def.Id z
                    | None   -> failwith "Invariant 830238b9-c428-4a15-bf44-08d57ace123a."

                if second.Mapping = first.Mapping then

                    // perfect overlap AND same resolution
                    let winner = if firstWins then first else second
                    merged <- merged |> Map.add def.Id winner

                else

                    // partial overlap OR different resolution
                    let ordered = if firstWins then [second; first] else [first; second]
                    handleCollision ordered

        for x in a do merge x
        for y in b do merge y
        merged |> Map.toArray |> Array.map (fun (_, v) -> v)


    
    
    let rec private mergeOverlappingSameRoot (domination : Dominance) (aRef : QNodeRef) (bRef : QNodeRef) : QNodeRef =

        match aRef.TryGetInMemory(), bRef.TryGetInMemory() with
        | None,   None   -> NoNode
        | Some a, None   -> InMemoryNode a
        | None,   Some b -> InMemoryNode b
        | Some a, Some b ->

            invariant (a.SplitLimitExponent = b.SplitLimitExponent)                             "cb56129b-089f-4f9e-a174-bf9575247277"
            invariant (a.Cell = b.Cell)                                                         "641da2e5-a7ea-4692-a96b-94440453ff1e"
            invariant (a.SampleExponent = b.SampleExponent)                                     "ccf5f5cc-5039-4b3d-aeba-31d0f88be037"
            invariant (a.Layers.Length = b.Layers.Length)                                       "ef0512a4-a18c-46da-8983-9ed57903854c"

            let rootCell = a.Cell
            let ose = min a.OriginalSampleExponent b.OriginalSampleExponent
            let domination = match domination with
                             | MoreDetailedDominates ->
                                if   a.OriginalSampleExponent < b.OriginalSampleExponent then FirstDominates
                                elif a.OriginalSampleExponent > b.OriginalSampleExponent then SecondDominates
                                else MoreDetailedDominates
                             | _ -> domination

            match a.SubNodes, b.SubNodes with
            | None,    None    -> // leaf/leaf
                let layers = mergeLayers domination a.Layers b.Layers
                invariant (a.Layers.Length = layers.Length)                                    "10d73a3d-ce2b-4bf8-a8a5-9123011477c4"
                QNode(Guid.NewGuid(), rootCell, a.SplitLimitExponent, ose, layers, None )
            | Some xs, None    -> // inner/leaf
                let lodLayers = QNode.generateLodLayers xs rootCell
                QNode(Guid.NewGuid(), rootCell, a.SplitLimitExponent, ose, lodLayers, xs)
            | None,    Some ys -> // leaf/inner
                let lodLayers = QNode.generateLodLayers ys rootCell
                QNode(Guid.NewGuid(), rootCell, a.SplitLimitExponent, ose, lodLayers, ys)
            | Some xs, Some ys -> // inner/inner
                let layers = mergeLayers domination a.Layers b.Layers
                invariant (a.Layers.Length = layers.Length)                                     "384e9a83-4c5d-4dfc-92eb-2ebc8084dd60"
                let rootCellChildren = rootCell.Children
                for i = 0 to 3 do
                    let x = xs.[i].TryGetInMemory()
                    let y = ys.[i].TryGetInMemory()

                    match x, y with
                    | None,   None   -> ()
                    | Some x, None   -> invariant (rootCellChildren.[i] = x.Cell)               "1f453ad2-9fe3-4dcb-ba97-4811d9c0c67e"
                    | None,   Some y -> invariant (rootCellChildren.[i] = y.Cell)               "71957893-06df-41e7-b2f7-09d3274e85c7"
                    | Some x, Some y ->
                        invariant (rootCellChildren.[i] = x.Cell)                               "9d0333e3-5839-4354-a7d5-e9ed446b4e6c"
                        invariant (rootCellChildren.[i] = y.Cell)                               "431e8feb-7810-479a-bb68-22c0f33a77c4"
                        invariant (x.Cell = y.Cell)                                             "dfd994a6-f34a-4110-8b2e-752beeb1e031"
                        
                let zs = Array.map2 (fun x y -> mergeOverlappingSameRoot domination x y) xs ys
                
                invariant (xs.Length = 4 && ys.Length = 4 && zs.Length = 4)                     "84392aaf-cf49-4afc-9d6e-923d13ecd8d8"

                QNode(Guid.NewGuid(), rootCell, a.SplitLimitExponent, ose, layers, zs)

            |> InMemoryNode
    
    let private setOrMergeIthSubnode (domination : Dominance) (i : int) (node : QNode) (newSubNodeRef : QNodeRef) : QNode =

        let newSubnode = newSubNodeRef.TryGetInMemory()

        invariant node.SubNodes.IsSome                                                          "f74ba958-cf53-4336-944f-46ef2c2b8893"
        invariant (node.SplitLimitExponent = newSubnode.Value.SplitLimitExponent)               "caff6340-8ecb-4434-bb79-f0adbb44492e"

        match newSubnode with
        | Some n ->
            invariant (node.Cell.GetQuadrant(i) = n.Cell)                                       "f5b92710-39de-4054-a67d-e2fbb1c9212c"
            invariant (node.Cell.Exponent = n.Cell.Exponent + 1)                                "7bb19442-9c94-42ed-b7c7-5c2929c349f5"
            invariant (node.SampleExponent = n.SampleExponent + 1)                              "a3088bca-7bb5-4013-b3e4-5a8ace81f929"
        | None -> ()

        let nss = node.SubNodes.Value |> Array.copy
        nss.[i] <- mergeOverlappingSameRoot domination nss.[i] newSubNodeRef
        let ose = match newSubnode with
                  | Some x -> min node.OriginalSampleExponent x.OriginalSampleExponent
                  | None   -> node.OriginalSampleExponent
        QNode(Guid.NewGuid(), node.Cell, node.SplitLimitExponent, ose, node.Layers, Some nss)

    let rec private mergeOverlappingBothCentered(domination : Dominance) (a : QNode) (b : QNode) : QNode =
        invariant a.Cell.IsCenteredAtOrigin                                                     "41380857-b8c9-4f68-88d1-e279af0667b1"
        invariant b.Cell.IsCenteredAtOrigin                                                     "670db503-29bd-495e-b565-d1a0e45b3c08"
        invariant (a.SplitLimitExponent = b.SplitLimitExponent)                                 "942abce1-ac80-42d4-90e5-4ad0979c1797"
        
        let ose = min a.OriginalSampleExponent b.OriginalSampleExponent

        let domination = 
            match domination with
            | MoreDetailedDominates ->
                if a.OriginalSampleExponent = b.OriginalSampleExponent then MoreDetailedDominates
                else
                    if a.OriginalSampleExponent < b.OriginalSampleExponent then FirstDominates
                    else SecondDominates
            | _ -> domination

        if a.Cell.Exponent = b.Cell.Exponent then
            match a.SubNodes, b.SubNodes with
            | Some xs, Some ys ->
                invariant (xs.Length = 4)                                                       "984c957d-9e34-48f6-9cef-fa8ad0b10de9"
                invariant (ys.Length = 4)                                                       "86617f6e-9527-40b0-ace5-7aab0b22bc25"

                let layers = mergeLayers domination a.Layers b.Layers
                invariant (a.Layers.Length = layers.Length)                                     "6a496764-8e7e-4250-a3d9-88ad049cd2ef"

                let zs = Array.map2 (fun x y -> mergeOverlappingSameRoot domination x y) xs ys
                invariant (zs.Length = 4)                                                       "068e6f2e-cf1f-46f9-8a26-cc8c2e0697ec"

                QNode(Guid.NewGuid(), a.Cell, a.SplitLimitExponent, ose, layers, Some zs)

            | Some _, None ->
                mergeOverlappingBothCentered (flipDomination domination) b a

            | None, Some ys ->
                invariant (ys.Length = 4)                                                       "e57a9e39-5cdd-4620-98cd-83c7c8885c6a"

                let layers = mergeLayers domination a.Layers b.Layers
                invariant (a.Layers.Length = layers.Length)                                     "dcfcf992-7444-47f4-97f1-0cbf1b0672e2"

                QNode(Guid.NewGuid(), a.Cell, a.SplitLimitExponent, ose, layers, Some ys)

            | None, None ->
                
                let layers = mergeLayers domination a.Layers b.Layers
                invariant (a.Layers.Length = layers.Length)                                     "b45a8c18-2184-4a8d-b0f1-18c9baa5d372"

                QNode(Guid.NewGuid(), a.Cell, a.SplitLimitExponent, ose, layers, None)
                
        else
            // grow centered node a upwards to eventually contain node b
            if a.Cell.Exponent < b.Cell.Exponent then

                let parentCell = a.Cell.Parent

                match a.SubNodes with
                | Some xs ->
                    let subnodes = xs |> Array.map (fun sn -> 
                        match sn.TryGetInMemory() with 
                        | None -> NoNode
                        | Some sn -> QNode.extendUpTo sn.Cell.Parent (InMemoryNode sn)
                        )
                    let parentLodLayers = QNode.generateLodLayers subnodes parentCell
                    let aNew = QNode(Guid.NewGuid(), parentCell, a.SplitLimitExponent, a.OriginalSampleExponent, parentLodLayers, Some subnodes) |> Some
                    invariant aNew.IsSome                                                       "eed625d8-976e-4dc1-8548-70e4c3285dc1"
                    invariant (aNew.Value.Cell.Exponent = a.Cell.Exponent + 1)                  "eec56ca0-39a4-4ef6-950e-2eb25a4b4420"
                    invariant (aNew.Value.SampleExponent = a.SampleExponent + 1)                "c4e097db-9e91-4d11-b41d-a99db892a8f4"
                    mergeOverlappingBothCentered domination aNew.Value b
                | None ->
                    let subnodes = 
                        a.Cell.Children 
                        |> Array.map (fun subCell ->
                            let subBox = subCell.GetBoundsForExponent(a.SampleExponent)
                            let subLayers = a.Layers |> Array.map (fun l -> l.WithWindow subBox) |> Array.choose id
                            if subLayers.Length > 0 then
                                let c = Cell2d(subCell.X, subCell.Y, subCell.Exponent + 1)
                                let n = QNode(Guid.NewGuid(), c, a.SplitLimitExponent, a.OriginalSampleExponent, subLayers, None)
                                InMemoryNode n
                            else
                                NoNode
                            )
                    let parentLodLayers = QNode.generateLodLayers subnodes parentCell
                    let aNew = QNode(Guid.NewGuid(), parentCell, a.SplitLimitExponent, a.OriginalSampleExponent, parentLodLayers, Some subnodes) |> Some

                    invariant (aNew.Value.SampleExponent = a.SampleExponent + 1)                "f2c20900-a00b-41c7-a04c-fc7e3da2ce30"

                    mergeOverlappingBothCentered domination aNew.Value b
            else
                mergeOverlappingBothCentered (flipDomination domination) b a
                
    let rec private mergeOverlappingFirstCentered(domination : Dominance) (a : QNode) (b : QNode)  : QNode =
        invariant a.Cell.IsCenteredAtOrigin                                                     "d8bf0eb6-7368-4c92-99b4-b7eafa6567f8"
        invariant (not b.Cell.IsCenteredAtOrigin)                                               "39c66587-2f0f-48b4-9823-24d77df925c5"
        invariant (a.SplitLimitExponent = b.SplitLimitExponent)                                 "183c56a6-0194-4648-b3e6-2a968ec8685a"

        if a.Cell.Contains(b.Cell) then
            let qi = a.Cell.GetQuadrant(b.Cell).Value
            match a.SubNodes with
            | Some xs ->
                let subRootCell = a.Cell.GetQuadrant(qi)
                let bExtended = 
                    match QNode.extendUpTo subRootCell (InMemoryNode b) with
                    | InMemoryNode n    -> n
                    | _                 -> failwith "Invariant 87e552e4-0a5d-495c-8a43-90e0f8aba60c."

                match xs.[qi].TryGetInMemory() with
                | Some qa ->
                    invariant (qa.Cell.Exponent = bExtended.Cell.Exponent)                      "5efd17cd-0d45-47cd-be5e-255c4a4576e7"
                    let m = mergeOverlappingSameRoot domination (InMemoryNode qa) (InMemoryNode bExtended)
                    invariant (match m with | NoNode -> false | _ -> true)                      "886fc65b-4b88-4ac9-80dd-467a83031535"
                    let newA = setOrMergeIthSubnode domination qi a m
                    newA
                | None ->
                    let newA = setOrMergeIthSubnode domination qi a (InMemoryNode bExtended)
                    newA
            | None ->
                // TODO: not implemented
                failwith "No subnode. Invariant 3e9c74b3-813a-4cdb-85f8-50ca689e0fc1."
        else
            match a.SubNodes with
            | Some xs ->
                let subnodes = xs |> Array.map (fun sn -> 
                    match sn.TryGetInMemory() with 
                    | None -> NoNode 
                    | Some sn -> match QNode.extendUpTo sn.Cell.Parent (InMemoryNode sn) with
                                 | InMemoryNode n -> InMemoryNode n
                                 | _              -> failwith "Invariant 125e2b16-01e2-4cb6-bd0e-15e50438293f."
                    )
                let parentCell = a.Cell.Parent
                let parentLodLayers = QNode.generateLodLayers subnodes parentCell
                let aNew = QNode(Guid.NewGuid(), parentCell, a.SplitLimitExponent, a.OriginalSampleExponent, parentLodLayers, Some subnodes) |> Some
                invariant aNew.IsSome                                                           "ee364985-7daa-4837-b6cb-7bcbc21a314f"
                invariant (aNew.Value.SampleExponent = a.SampleExponent + 1)                    "a6919ff2-07ee-4c6e-a350-b9de29953460"
                mergeOverlappingFirstCentered domination aNew.Value b
            | None ->
                // TODO: not implemented
                failwith "No subnode. Invariant 65d23a56-f000-4989-8498-29a15d8ca85d."

    let rec private mergeOverlappingNoneCentered (domination : Dominance) (a : QNode) (b : QNode) =

        invariant (a.Cell.Exponent > b.Cell.Exponent)                                           "4b40bc08-b19d-4f49-b6e5-f321bf1e7dd0"
        invariant (a.Cell.Contains(b.Cell))                                                     "9a44a9ea-2996-46ff-9cc6-c9de1992465d"
        invariant (not(b.Cell.Contains(a.Cell)))                                                "7d3465b9-90c7-4e7d-99aa-67e5383fb124"

        let qi = a.Cell.GetQuadrant(b.Cell).Value
        let qcell = a.Cell.GetQuadrant(qi)

        let a'' = if a.IsLeafNode then 
                    QNode(Guid.NewGuid(), a.Cell, a.SplitLimitExponent, a.OriginalSampleExponent, a.Layers, Some(Array.create 4 NoNode))
                  else 
                    a

        let extendedB = match QNode.extendUpTo qcell (InMemoryNode b) with
                        | InMemoryNode n -> n
                        | _ -> failwith "Invariant 8cdd673e-4ec9-439b-9c23-995dae494a09."

        invariant (qcell = extendedB.Cell)                                                      "2c475ceb-a6c8-4106-ba09-fa34469dfb59"
        let de = qcell.Exponent - b.Cell.Exponent
        invariant (b.SampleExponent + de = extendedB.SampleExponent)                            "950eabd4-e7e8-4498-a514-9a71f76a6369"


        let result = InMemoryNode extendedB |> setOrMergeIthSubnode domination qi a''
        result

    /// Merge overlapping nodes with same roots.
    let rec private mergeOverlappingNodesWithSameRoot (domination : Dominance) (firstRef : QNodeRef) (secondRef : QNodeRef) : QNodeRef =
        
        match firstRef.TryGetInMemory(), secondRef.TryGetInMemory() with
        | None,       None        -> NoNode
        | Some _,     None        -> firstRef
        | None,       Some _      -> secondRef
        | Some first, Some second ->

            invariant (first.Cell               = second.Cell)                                  "ee06efff-0975-4bf4-9909-daac6a2825a3"
            invariant (first.SplitLimitExponent = second.SplitLimitExponent)                    "758ee842-b259-4a68-8fe5-0068924eaae8"
            invariant (first.SampleExponent     = second.SampleExponent)                        "1b6b7f11-0569-499a-8174-05564e7c23ce"

            let commonRootCell = first.Cell
            let sle = first.SplitLimitExponent
            let ose = min first.OriginalSampleExponent second.OriginalSampleExponent

            let sampleWindowsIntersect = first.SampleWindow.Intersects(second.SampleWindow)


            match first.IsLeafNode, second.IsLeafNode, sampleWindowsIntersect with
            // (1) leaf, leaf, _
            | true,  true,  _     ->
                              
                // -> directly merge layers and done
                let layers = mergeLayers domination first.Layers second.Layers
                QNode(Guid.NewGuid(), commonRootCell, sle, ose, layers, None) |> InMemoryNode

            // (2.1) leaf, tree, samples DO NOT intersect
            | true,  false, false ->
                
                // 1st layer data does not intersect 2nd layer data, we can
                // (a) directly merge root layers, and
                let layers = mergeLayers domination first.Layers second.Layers
                // (b) attach tree subnodes to leaf
                QNode(Guid.NewGuid(), commonRootCell, sle, ose, layers, second.SubNodes) |> InMemoryNode

            // (2.2) leaf, tree, samples DO intersect
            | true,  false, true  ->
                
                let firstContainsSecond = first.SampleWindow.Contains(second.SampleWindow)
                let secondContainsFirst = second.SampleWindow.Contains(first.SampleWindow)
                match domination, firstContainsSecond, secondContainsFirst with
                
                | FirstDominates , true, _     -> firstRef  // 1st dominates and covers all     2nd samples -> 1st wins

                | FirstDominates, false, true  ->
                    let layers = mergeLayers domination first.Layers second.Layers
                    QNode(Guid.NewGuid(), commonRootCell, sle, ose, layers, second.SubNodes) |> InMemoryNode
                
                | SecondDominates,    _, true  -> secondRef // 2nd dominates and covers all     1st samples -> 2nd wins 
                
                | SecondDominates, true, false ->           // 2nd dominates and covers part of 1st samples

                    let layers = mergeLayers domination first.Layers second.Layers
                    QNode(Guid.NewGuid(), commonRootCell, sle, ose, layers, second.SubNodes) |> InMemoryNode

                | MoreDetailedDominates, _, _  ->
                    if first.OriginalSampleExponent < second.OriginalSampleExponent then
                        mergeOverlappingNodesWithSameRoot FirstDominates firstRef secondRef
                    elif first.OriginalSampleExponent > second.OriginalSampleExponent then
                        mergeOverlappingNodesWithSameRoot SecondDominates firstRef secondRef
                    else
                        sprintf "not implemented: %A, %A, %A" domination firstContainsSecond secondContainsFirst  |> failwith
                
                | _                            ->
                    sprintf "not implemented: %A, %A, %A" domination firstContainsSecond secondContainsFirst  |> failwith

            // (3) tree, leaf, _
            | false, true, _      ->

                // -> let's flip this around, so we just have one case to implement (see cases 2.x)
                mergeOverlappingNodesWithSameRoot (flipDomination domination) secondRef firstRef

            // (4) tree + tree
            | false, false, _     ->
                
                // -> recursively merge layers
                failwith "not implemented: recursively merge layers"

            

    /// Merge nodes, where one node is a subnode of the other, or both nodes are the same.
    let rec private mergeOverlappingNodes (domination : Dominance) (firstRef : QNodeRef) (secondRef : QNodeRef) : QNodeRef =

        match firstRef.TryGetInMemory(), secondRef.TryGetInMemory() with
        | None,   None            -> NoNode
        | Some _, None            -> firstRef
        | None,   Some _          -> secondRef
        | Some first, Some second ->

            invariant (first.Cell.Intersects(second.Cell) )                                     "7d7198ec-8be6-4f45-8645-06fff2034d65"
            invariant (first.SplitLimitExponent = second.SplitLimitExponent)                    "1f9e78ad-468b-4e39-8341-1dcaec7f051f"

            if   first.Cell = second.Cell then

                mergeOverlappingNodesWithSameRoot domination firstRef secondRef
                 

            elif first.Cell.Exponent < second.Cell.Exponent then

                // first is subnode of second
                // -> let's flip this around, so we just have one case to implement (see below)
                mergeOverlappingNodes (flipDomination domination) secondRef firstRef

            else
            
                // second is subnode of first
                invariant (first.Cell.Exponent > second.Cell.Exponent)                          "eba43fbc-51fa-4531-9a9d-c3f595989571"
                
                // grow second tree upwards until it reaches first tree's root node
                // ... layer data will be lodded on the way up by 'extendUpTo'
                let secondRefExtended = QNode.extendUpTo first.Cell secondRef
                mergeOverlappingNodesWithSameRoot domination firstRef secondRefExtended


    /// Merge nodes that do not overlap
    let private mergeNonOverlappingNodes (domination : Dominance) (firstRef : QNodeRef) (secondRef : QNodeRef) : QNodeRef =
        match firstRef.TryGetInMemory(), secondRef.TryGetInMemory() with
        | None,       None        -> NoNode
        | Some _,     None        -> firstRef
        | None,       Some _      -> secondRef
        | Some first, Some second ->

            if first.Cell.Intersects(second.Cell) then
                failwith "Nodes must not overlap. Error 6ada6e09-ef33-4daf-9b0c-4c6dd30f0087."

            let commonRootCell = Cell2d(Box2d(first.Cell.BoundingBox, second.Cell.BoundingBox))
            let withCommonRoot n = match QNode.extendUpTo commonRootCell n with
                                   | InMemoryNode n -> n
                                   | _              -> failwith "Invariant cefba0df-3631-4da9-a19a-742e844ca8d4."
            let aRooted = (firstRef |> withCommonRoot)
            let bRooted = (secondRef |> withCommonRoot)

            invariant (aRooted.Cell = commonRootCell)                                           "7730eae1-0212-40e5-b114-ff81c0c40762"
            invariant (bRooted.Cell = commonRootCell)                                           "00205e51-46c2-451a-bd7c-5dc45f18acc1"
            invariant (aRooted.SampleExponent = bRooted.SampleExponent)                         "178eedaa-89e2-473b-afbb-1beee112225d"

            mergeOverlappingSameRoot domination (InMemoryNode aRooted) (InMemoryNode bRooted)



    let merge (domination : Dominance) (firstRef : QNodeRef) (secondRef : QNodeRef) : QNodeRef =

        match firstRef.TryGetInMemory(), secondRef.TryGetInMemory() with
        | None,    None    -> NoNode
        | Some _,  None    -> firstRef
        | None,    Some _  -> secondRef
        | Some first, Some second ->

            if first.SplitLimitExponent <> second.SplitLimitExponent then
                failwith "Cannot merge quadtrees with different split limits. Error 6222eb6b-a7aa-43c1-9323-e28d6275696b."

            if overlapping first second then 
                mergeOverlappingNodes    domination firstRef secondRef 
            else
                mergeNonOverlappingNodes domination firstRef secondRef
