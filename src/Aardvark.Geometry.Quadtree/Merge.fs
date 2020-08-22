namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open System

(*
    Merge.
*)

module Merge =

    let inline private intersecting (a : QNode) (b : QNode) = a.Cell.Intersects(b.Cell)

    let rec private extendUpTo (root : Cell2d) (node : QNodeRef) : QNodeRef =
        match node.TryGetInMemory() with
        | None -> NoNode
        | Some node ->
            invariant (root.Contains(node.Cell))                                                "a48ca4ab-3f20-45ff-bd3c-c08f2a8fcc15"
            invariant (root.Exponent >= node.Cell.Exponent)                                     "cda4b28d-4449-4db2-80b8-40c0617ecf22"
            invariant (root.BoundingBox.Contains(node.SampleWindowBoundingBox))                 "3eb5c9c4-a78e-4788-b1b2-2727564524ee"
            
            if root = node.Cell then
                InMemoryNode node
            else
                invariant (root.Exponent > node.Cell.Exponent)                                  "b652d9bc-340a-4312-8407-9fdee62ffcaa"

                if root.IsCenteredAtOrigin && node.Cell.IsCenteredAtOrigin then

                    let roots = root.Children
                    let nodes = match node.SubNodes with
                                | Some xs -> xs
                                | None    -> node.Split() |> Array.map InMemoryNode
                    let rootSubNodes = Array.map2 extendUpTo roots nodes
                    let rootLodLayers = QNode.GenerateLodLayers rootSubNodes root

                    let n = QNode(Guid.NewGuid(), root, node.SplitLimitExponent,
                                 node.OriginalSampleExponent, rootLodLayers,
                                 rootSubNodes)

                    InMemoryNode n

                else
                    let isChildOfCenteredRoot = root.IsCenteredAtOrigin && root.Exponent = node.Cell.Exponent + 1

                    let parentCell = if isChildOfCenteredRoot then root else node.Cell.Parent
                    let qi = parentCell.GetQuadrant(node.Cell)
                
                    invariantm qi.HasValue                                                      "09575aa7-38b3-4afa-bb63-389af3301fc0"
                        (sprintf "Parent cell: %A; node cell: %A." parentCell node.Cell)
                
                    let parentSubNodes = Array.create 4 NoNode
                    parentSubNodes.[qi.Value] <- InMemoryNode node
                    let parentLodLayers = QNode.GenerateLodLayers parentSubNodes root
                
                    invariant (node.SampleExponent + 1 = parentLodLayers.[0].SampleExponent)    "7b0fa058-4812-4332-9547-0c33ee7ea7d5"
                
                    let parentNode = QNode(Guid.NewGuid(), parentCell, node.SplitLimitExponent,
                                           node.OriginalSampleExponent, parentLodLayers,
                                           parentSubNodes)
                
                    invariant (parentNode.Cell.Exponent = node.Cell.Exponent + 1)               "27b263d3-7fe5-49fd-96ca-422c37f9a31a"
                    invariant (parentNode.SampleExponent = node.SampleExponent + 1)             "6076b045-9bee-4285-b583-1d3d49bb259a"
                
                    match extendUpTo root (InMemoryNode parentNode) with
                    | InMemoryNode result -> invariant (root.Exponent = result.Cell.Exponent)   "a0d249da-ed0d-4abe-8751-191ad0ffb9f9"
                                             InMemoryNode result
                    | _ -> failwith "Invariant 991571a4-ea05-4142-bd38-a6964319ba97."

    let private mergeLayers (domination : Dominance) (a : ILayer[]) (b : ILayer[]) : ILayer[] =

        invariantm (a.Length = b.Length) "Mismatch number of layers." "d034fed2-aaa8-4e95-851d-48cc364943e9"

        let mutable merged = Map.empty
        let merge (x : ILayer) : unit =
            match Map.tryFind x.Def.Id merged with

            | Some (first : ILayer) ->
                let second = x

                let handleCollision () =
                    match seq {second;first} |> Layer.Merge with
                    | Some z -> merged <- merged |> Map.add z.Def.Id z
                    | None -> ()

                if second.Mapping = first.Mapping then
                    if second.SampleExponent <> first.SampleExponent then
                        match domination, second.SampleExponent < first.SampleExponent with
                        | FirstDominates, _ | MoreDetailedDominates, true  -> merged <- merged |> Map.add second.Def.Id first
                        | SecondDominates, _ | MoreDetailedDominates, false -> merged <- merged |> Map.add first.Def.Id second
                    else
                        match domination with
                        | FirstDominates -> merged <- merged |> Map.add second.Def.Id first
                        | SecondDominates -> merged <- merged |> Map.add first.Def.Id second
                        | MoreDetailedDominates -> handleCollision()
                else
                    handleCollision()

            | None   -> merged <- merged |> Map.add x.Def.Id x

        for x in a do merge x
        for y in b do merge y
        merged |> Map.toArray |> Array.map (fun (_, v) -> v)

    let rec private mergeSameRoot (domination : Dominance) (a : QNodeRef) (b : QNodeRef) : QNodeRef =
        match a.TryGetInMemory(), b.TryGetInMemory() with
        | Some a0, Some b0 ->
            invariant (a0.SplitLimitExponent = b0.SplitLimitExponent)   "cb56129b-089f-4f9e-a174-bf9575247277"
            invariant (a0.Cell = b0.Cell)                               "641da2e5-a7ea-4692-a96b-94440453ff1e"
            invariant (a0.SampleExponent = b0.SampleExponent)           "ccf5f5cc-5039-4b3d-aeba-31d0f88be037"
            invariant (a0.Layers.Length = b0.Layers.Length)             "ef0512a4-a18c-46da-8983-9ed57903854c"

            let rootCell = a0.Cell
            let ose = min a0.OriginalSampleExponent b0.OriginalSampleExponent
            let domination = match domination with
                             | MoreDetailedDominates ->
                                if a0.OriginalSampleExponent < b0.OriginalSampleExponent then FirstDominates
                                elif a0.OriginalSampleExponent > b0.OriginalSampleExponent then SecondDominates
                                else MoreDetailedDominates
                             | _ -> domination

            match a0.SubNodes, b0.SubNodes with
            | Some xs, Some ys -> // inner/inner
                let layers = mergeLayers domination a0.Layers b0.Layers
                invariant (a0.Layers.Length = layers.Length)            "384e9a83-4c5d-4dfc-92eb-2ebc8084dd60"
                let rootCellChildren = rootCell.Children
                for i = 0 to 3 do
                    let x = xs.[i].TryGetInMemory()
                    let y = ys.[i].TryGetInMemory()

                    match x, y with
                    | Some x, Some y ->
                        invariant (rootCellChildren.[i] = x.Cell)       "9d0333e3-5839-4354-a7d5-e9ed446b4e6c"
                        invariant (rootCellChildren.[i] = y.Cell)       "431e8feb-7810-479a-bb68-22c0f33a77c4"
                        invariant (x.Cell = y.Cell)                     "dfd994a6-f34a-4110-8b2e-752beeb1e031"
                    | Some x, None ->
                        invariant (rootCellChildren.[i] = x.Cell)       "1f453ad2-9fe3-4dcb-ba97-4811d9c0c67e"
                    | None, Some y ->
                        invariant (rootCellChildren.[i] = y.Cell)       "71957893-06df-41e7-b2f7-09d3274e85c7"
                    | None, None ->
                        ()
                        
                let zs = Array.map2 (mergeSameRoot domination) xs ys
                
                invariant (xs.Length = 4)                               "84392aaf-cf49-4afc-9d6e-923d13ecd8d8"
                invariant (ys.Length = 4)                               "087924cb-b42e-4f03-9385-26744c702b04"
                invariant (zs.Length = 4)                               "d972f5f1-5c2d-460c-9d98-5e6fc1fade99"

                QNode(Guid.NewGuid(), rootCell, a0.SplitLimitExponent, ose, layers, zs) |> InMemoryNode
            
            | Some xs, None    -> // inner/leaf
                let lodLayers = QNode.GenerateLodLayers xs rootCell
                QNode(Guid.NewGuid(), rootCell, a0.SplitLimitExponent, ose, lodLayers, xs) |> InMemoryNode
            
            | None,    Some ys -> // leaf/inner
                let lodLayers = QNode.GenerateLodLayers ys rootCell
                QNode(Guid.NewGuid(), rootCell, a0.SplitLimitExponent, ose, lodLayers, ys) |> InMemoryNode
            
            | None,    None    -> // leaf/leaf
                let layers = mergeLayers domination a0.Layers b0.Layers
                invariant (a0.Layers.Length = layers.Length)            "10d73a3d-ce2b-4bf8-a8a5-9123011477c4"
                QNode(Guid.NewGuid(), rootCell, a0.SplitLimitExponent, ose, layers, None) |> InMemoryNode

        | Some a, None   -> InMemoryNode a
        | None,   Some b -> InMemoryNode b
        | None,   None   -> NoNode
    
    let private setOrMergeIthSubnode (domination : Dominance) (i : int) (node : QNode) (newSubNodeRef : QNodeRef) : QNode =

        let newSubnode = newSubNodeRef.TryGetInMemory()

        invariant node.SubNodes.IsSome                                              "f74ba958-cf53-4336-944f-46ef2c2b8893"
        invariant (node.SplitLimitExponent = newSubnode.Value.SplitLimitExponent)   "caff6340-8ecb-4434-bb79-f0adbb44492e"

        match newSubnode with
        | Some n ->
            invariant (node.Cell.GetQuadrant(i) = n.Cell)                           "f5b92710-39de-4054-a67d-e2fbb1c9212c"
            invariant (node.Cell.Exponent = n.Cell.Exponent + 1)                    "7bb19442-9c94-42ed-b7c7-5c2929c349f5"
            invariant (node.SampleExponent = n.SampleExponent + 1)                  "a3088bca-7bb5-4013-b3e4-5a8ace81f929"
        | None -> ()

        let nss = node.SubNodes.Value |> Array.copy
        nss.[i] <- mergeSameRoot domination nss.[i] newSubNodeRef
        let ose = match newSubnode with
                  | Some x -> min node.OriginalSampleExponent x.OriginalSampleExponent
                  | None   -> node.OriginalSampleExponent
        QNode(Guid.NewGuid(), node.Cell, node.SplitLimitExponent, ose, node.Layers, Some nss)

    let rec private mergeIntersectingBothCentered (domination : Dominance) (a : QNode) (b : QNode) : QNode =
        invariant a.Cell.IsCenteredAtOrigin                                         "41380857-b8c9-4f68-88d1-e279af0667b1"
        invariant b.Cell.IsCenteredAtOrigin                                         "670db503-29bd-495e-b565-d1a0e45b3c08"
        invariant (a.SplitLimitExponent = b.SplitLimitExponent)                     "942abce1-ac80-42d4-90e5-4ad0979c1797"
        
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
                invariant (xs.Length = 4)                                           "984c957d-9e34-48f6-9cef-fa8ad0b10de9"
                invariant (ys.Length = 4)                                           "86617f6e-9527-40b0-ace5-7aab0b22bc25"

                let layers = mergeLayers domination a.Layers b.Layers
                invariant (a.Layers.Length = layers.Length)                         "6a496764-8e7e-4250-a3d9-88ad049cd2ef"

                let zs = Array.map2 (mergeSameRoot domination) xs ys
                invariant (zs.Length = 4)                                           "068e6f2e-cf1f-46f9-8a26-cc8c2e0697ec"

                QNode(Guid.NewGuid(), a.Cell, a.SplitLimitExponent, ose, layers, Some zs)

            | Some _, None ->
                mergeIntersectingBothCentered (flipDomination domination) b a

            | None, Some ys ->
                invariant (ys.Length = 4)                                           "e57a9e39-5cdd-4620-98cd-83c7c8885c6a"

                let layers = mergeLayers domination a.Layers b.Layers
                invariant (a.Layers.Length = layers.Length)                         "dcfcf992-7444-47f4-97f1-0cbf1b0672e2"

                QNode(Guid.NewGuid(), a.Cell, a.SplitLimitExponent, ose, layers, Some ys)

            | None, None ->
                
                let layers = mergeLayers domination a.Layers b.Layers
                invariant (a.Layers.Length = layers.Length)                         "b45a8c18-2184-4a8d-b0f1-18c9baa5d372"

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
                        | Some sn -> extendUpTo sn.Cell.Parent (InMemoryNode sn)
                        )
                    let parentLodLayers = QNode.GenerateLodLayers subnodes parentCell
                    let aNew = QNode(Guid.NewGuid(), parentCell, a.SplitLimitExponent, a.OriginalSampleExponent, parentLodLayers, Some subnodes) |> Some
                    invariant aNew.IsSome                                           "eed625d8-976e-4dc1-8548-70e4c3285dc1"
                    invariant (aNew.Value.Cell.Exponent = a.Cell.Exponent + 1)      "eec56ca0-39a4-4ef6-950e-2eb25a4b4420"
                    invariant (aNew.Value.SampleExponent = a.SampleExponent + 1)    "c4e097db-9e91-4d11-b41d-a99db892a8f4"
                    mergeIntersectingBothCentered domination aNew.Value b
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
                    let parentLodLayers = QNode.GenerateLodLayers subnodes parentCell
                    let aNew = QNode(Guid.NewGuid(), parentCell, a.SplitLimitExponent, a.OriginalSampleExponent, parentLodLayers, Some subnodes) |> Some

                    invariant (aNew.Value.SampleExponent = a.SampleExponent + 1)    "f2c20900-a00b-41c7-a04c-fc7e3da2ce30"

                    mergeIntersectingBothCentered domination aNew.Value b
            else
                mergeIntersectingBothCentered (flipDomination domination) b a
                
    let rec private mergeIntersectingFirstCentered (domination : Dominance) (a : QNode) (b : QNode) : QNode =
        invariant a.Cell.IsCenteredAtOrigin                                         "d8bf0eb6-7368-4c92-99b4-b7eafa6567f8"
        invariant (not b.Cell.IsCenteredAtOrigin)                                   "39c66587-2f0f-48b4-9823-24d77df925c5"
        invariant (a.SplitLimitExponent = b.SplitLimitExponent)                     "183c56a6-0194-4648-b3e6-2a968ec8685a"

        if a.Cell.Contains(b.Cell) then
            let qi = a.Cell.GetQuadrant(b.Cell).Value
            match a.SubNodes with
            | Some xs ->
                let subRootCell = a.Cell.GetQuadrant(qi)
                let bExtended = 
                    match extendUpTo subRootCell (InMemoryNode b) with
                    | InMemoryNode n    -> n
                    | _                 -> failwith "Invariant 87e552e4-0a5d-495c-8a43-90e0f8aba60c."

                match xs.[qi].TryGetInMemory() with
                | Some qa ->
                    invariant (qa.Cell.Exponent = bExtended.Cell.Exponent)          "5efd17cd-0d45-47cd-be5e-255c4a4576e7"
                    let m = mergeSameRoot domination (InMemoryNode qa) (InMemoryNode bExtended)
                    invariant (match m with | NoNode -> false | _ -> true)          "886fc65b-4b88-4ac9-80dd-467a83031535"
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
                    | Some sn -> match extendUpTo sn.Cell.Parent (InMemoryNode sn) with
                                 | InMemoryNode n -> InMemoryNode n
                                 | _              -> failwith "Invariant 125e2b16-01e2-4cb6-bd0e-15e50438293f."
                    )
                let parentCell = a.Cell.Parent
                let parentLodLayers = QNode.GenerateLodLayers subnodes parentCell
                let aNew = QNode(Guid.NewGuid(), parentCell, a.SplitLimitExponent, a.OriginalSampleExponent, parentLodLayers, Some subnodes) |> Some
                invariant aNew.IsSome                                               "ee364985-7daa-4837-b6cb-7bcbc21a314f"
                invariant (aNew.Value.SampleExponent = a.SampleExponent + 1)        "a6919ff2-07ee-4c6e-a350-b9de29953460"
                mergeIntersectingFirstCentered domination aNew.Value b
            | None ->
                // TODO: not implemented
                failwith "No subnode. Invariant 65d23a56-f000-4989-8498-29a15d8ca85d."

    let rec private mergeIntersecting (domination : Dominance) (a : QNodeRef) (b : QNodeRef) : QNodeRef =
        match a.TryGetInMemory(), b.TryGetInMemory() with
        | Some a', Some b' ->

            invariant (a'.SplitLimitExponent = b'.SplitLimitExponent)                   "1f9e78ad-468b-4e39-8341-1dcaec7f051f"

            if   a'.Cell.Exponent = b'.Cell.Exponent then

                invariant (a'.SampleExponent = b'.SampleExponent)                       "a1db923a-ad0d-4a16-9c34-e8721bd00902"

                match a'.Cell.IsCenteredAtOrigin, b'.Cell.IsCenteredAtOrigin with
                | true, true   -> mergeIntersectingBothCentered domination a' b' |> InMemoryNode
                | true, false  -> mergeIntersectingFirstCentered domination a' b' |> InMemoryNode
                | false, true  -> mergeIntersectingFirstCentered (flipDomination domination) b' a' |> InMemoryNode
                | false, false -> mergeSameRoot domination a b

            elif a'.Cell.Exponent < b'.Cell.Exponent then

                mergeIntersecting (flipDomination domination) b a

            else

                let dCellExp = a'.Cell.Exponent - b'.Cell.Exponent
                invariant (a'.SampleExponent = b'.SampleExponent + dCellExp)            "c66ce7cd-5e8f-43d9-ad8e-53aaf0b44910"

                let domination =
                    if domination = MoreDetailedDominates then
                        if a'.SampleExponent <> b'.SampleExponent then
                            if a'.SampleExponent < b'.SampleExponent then
                                FirstDominates
                            else
                                SecondDominates
                        else
                            failwith "Cannot merge data with same resolution. Invariant f74cbaf3-04f6-49d1-bf0f-c6395b887619."
                    else
                        domination

                match a'.Cell.IsCenteredAtOrigin, b'.Cell.IsCenteredAtOrigin with
                | true, true -> mergeIntersectingBothCentered domination a' b' |> InMemoryNode
                | true, false -> mergeIntersectingFirstCentered domination a' b' |> InMemoryNode
                | false, true -> mergeIntersectingFirstCentered (flipDomination domination) b' a' |> InMemoryNode
                | false, false ->

                    invariant (a'.Cell.Exponent > b'.Cell.Exponent)                     "4b40bc08-b19d-4f49-b6e5-f321bf1e7dd0"
                    invariant (a'.Cell.Contains(b'.Cell))                               "9a44a9ea-2996-46ff-9cc6-c9de1992465d"
                    invariant (not(b'.Cell.Contains(a'.Cell)))                          "7d3465b9-90c7-4e7d-99aa-67e5383fb124"

                    let qi = a'.Cell.GetQuadrant(b'.Cell).Value
                    let qcell = a'.Cell.GetQuadrant(qi)

                    let a'' = if a'.IsLeafNode then 
                                QNode(Guid.NewGuid(), a'.Cell, a'.SplitLimitExponent, a'.OriginalSampleExponent, a'.Layers, Some(Array.create 4 NoNode))
                              else 
                                a'

                    let extendedB = match extendUpTo qcell b with
                                    | InMemoryNode n -> n
                                    | _ -> failwith "Invariant 8cdd673e-4ec9-439b-9c23-995dae494a09."

                    invariant (qcell = extendedB.Cell)                            "2c475ceb-a6c8-4106-ba09-fa34469dfb59"
                    let de = qcell.Exponent - b'.Cell.Exponent
                    invariant (b'.SampleExponent + de = extendedB.SampleExponent) "950eabd4-e7e8-4498-a514-9a71f76a6369"


                    let result = InMemoryNode extendedB |> setOrMergeIthSubnode domination qi a''
                    InMemoryNode result

        | Some _, None   -> a
        | None,   Some _ -> b
        | None,   None   -> NoNode

    let private mergeNonIntersecting (domination : Dominance) (a : QNodeRef) (b : QNodeRef) : QNodeRef =
        match a.TryGetInMemory(), b.TryGetInMemory() with
        | Some a1, Some b1 ->
            let commonRootCell = Cell2d(Box2d(a1.Cell.BoundingBox, b1.Cell.BoundingBox))
            let withCommonRoot n = match extendUpTo commonRootCell n with
                                   | InMemoryNode n -> n
                                   | _              -> failwith "Invariant cefba0df-3631-4da9-a19a-742e844ca8d4."
            let a2 = (a |> withCommonRoot)
            let b2 = (b |> withCommonRoot)
            invariant (a2.Cell = commonRootCell)                          "7730eae1-0212-40e5-b114-ff81c0c40762"
            invariant (b2.Cell = commonRootCell)                          "00205e51-46c2-451a-bd7c-5dc45f18acc1"
            invariant (a2.SampleExponent = b2.SampleExponent)             "178eedaa-89e2-473b-afbb-1beee112225d"
            mergeSameRoot domination (InMemoryNode a2) (InMemoryNode b2)
        | Some _,  None    -> a
        | None,    Some _  -> b
        | None,    None    -> NoNode

    let Merge (domination : Dominance) (a : QNodeRef) ( b : QNodeRef) : QNodeRef =
        match a.TryGetInMemory(), b.TryGetInMemory() with
        | Some a', Some b' -> (if intersecting a' b' then mergeIntersecting else mergeNonIntersecting) domination a b
        | Some _,  None    -> a
        | None,    Some _  -> b
        | None,    None    -> NoNode
