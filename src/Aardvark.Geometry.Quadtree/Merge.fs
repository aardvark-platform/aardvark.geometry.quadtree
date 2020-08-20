namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open System

(*
    Merge.
*)

module Merge =

    let inline private intersecting (a : INode) (b : INode) = a.Cell.Intersects(b.Cell)

    let rec private extendUpTo (root : Cell2d) (node : INode option) : INode option =
        match node with
        | None -> None
        | Some node ->
            invariant (root.Contains(node.Cell))                "a48ca4ab-3f20-45ff-bd3c-c08f2a8fcc15."
            invariant (root.Exponent >= node.Cell.Exponent)     "cda4b28d-4449-4db2-80b8-40c0617ecf22."
            invariant (root.BoundingBox.Contains(node.SampleWindowBoundingBox)) "3eb5c9c4-a78e-4788-b1b2-2727564524ee."
            
            if root = node.Cell then
                Some node
            else
                invariant (root.Exponent > node.Cell.Exponent)  "56251fd0-5344-4d0a-b76b-815cdd5a7607."

                let isChildOfCenteredRoot = root.IsCenteredAtOrigin && root.Exponent = node.Cell.Exponent + 1

                let parentCell = if isChildOfCenteredRoot then root else node.Cell.Parent
                let qi = parentCell.GetQuadrant(node.Cell)
                invariant qi.HasValue                           "09575aa7-38b3-4afa-bb63-389af3301fc0."
                let subnodes = Array.create 4 None
                subnodes.[qi.Value] <- Some node
                let parentLodLayers = Node.GenerateLodLayers subnodes root
                invariant (node.SampleExponent + 1 = parentLodLayers.[0].SampleExponent) "7b0fa058-4812-4332-9547-0c33ee7ea7d5."
                let parentNode = Node(Guid.NewGuid(), parentCell, node.OriginalSampleExponent, parentLodLayers, Some subnodes) :> INode |> Some
                let result = extendUpTo root parentNode
                invariant (root.Exponent = result.Value.Cell.Exponent) "a0d249da-ed0d-4abe-8751-191ad0ffb9f9."
                result

    let private mergeLayers (domination : Dominance) (a : ILayer[]) (b : ILayer[]) : ILayer[] =

        invariant (a.Length = b.Length) "Mismatch number of layers. d034fed2-aaa8-4e95-851d-48cc364943e9."

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

    let rec private mergeSameRoot (domination : Dominance) (a : INode option) (b : INode option) : INode option =
        match a, b with
        | Some a0, Some b0 ->
            invariant (a0.Cell = b0.Cell) "641da2e5-a7ea-4692-a96b-94440453ff1e."
            invariant (a0.Layers.Length = b0.Layers.Length) "ef0512a4-a18c-46da-8983-9ed57903854c."

            let rootCell = a0.Cell
            let windowA = a0.SampleWindow
            let windowB = b0.SampleWindow
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
                invariant (a0.Layers.Length = layers.Length) "384e9a83-4c5d-4dfc-92eb-2ebc8084dd60."
                let rootCellChildren = rootCell.Children
                for i = 0 to 3 do
                    match xs.[i], ys.[i] with
                    | Some x, Some y ->
                        invariant (rootCellChildren.[i] = x.Cell) "9d0333e3-5839-4354-a7d5-e9ed446b4e6c."
                        invariant (rootCellChildren.[i] = y.Cell) "431e8feb-7810-479a-bb68-22c0f33a77c4."
                        invariant (x.Cell = y.Cell)               "dfd994a6-f34a-4110-8b2e-752beeb1e031."
                    | Some x, None ->
                        invariant (rootCellChildren.[i] = x.Cell) "1f453ad2-9fe3-4dcb-ba97-4811d9c0c67e."
                    | None, Some y ->
                        invariant (rootCellChildren.[i] = y.Cell) "71957893-06df-41e7-b2f7-09d3274e85c7."
                    | None, None ->
                        ()
                        
                let zs = Array.map2 (mergeSameRoot domination) xs ys
                
                invariant (xs.Length = 4) "84392aaf-cf49-4afc-9d6e-923d13ecd8d8."
                invariant (ys.Length = 4) "087924cb-b42e-4f03-9385-26744c702b04."
                invariant (zs.Length = 4) "d972f5f1-5c2d-460c-9d98-5e6fc1fade99."

                Node(Guid.NewGuid(), rootCell, ose, layers, Some zs) :> INode |> Some
            | Some xs, None    -> // inner/leaf
                let lodLayers = Node.GenerateLodLayers xs rootCell
                Node(Guid.NewGuid(), rootCell, ose, lodLayers, Some xs) :> INode |> Some
            | None,    Some ys -> // leaf/inner
                let lodLayers = Node.GenerateLodLayers ys rootCell
                Node(Guid.NewGuid(), rootCell, ose, lodLayers, Some ys) :> INode |> Some
            | None,    None    -> // leaf/leaf
                let layers = mergeLayers domination a0.Layers b0.Layers
                invariant (a0.Layers.Length = layers.Length) "10d73a3d-ce2b-4bf8-a8a5-9123011477c4."
                Node(Guid.NewGuid(), rootCell, ose, layers, None) :> INode |> Some

        | Some a, None   -> Some a
        | None,   Some b -> Some b
        | None,   None   -> None
    
    let private setOrMergeIthSubnode (domination : Dominance) (i : int) (node : INode) (newSubnode : INode option) : INode =
        invariant node.SubNodes.IsSome "f74ba958-cf53-4336-944f-46ef2c2b8893"
        invariant (node.Cell.Exponent = newSubnode.Value.Cell.Exponent + 1) "7bb19442-9c94-42ed-b7c7-5c2929c349f5"
        if newSubnode.IsSome then invariant (node.Cell.GetQuadrant(i) = newSubnode.Value.Cell) "f5b92710-39de-4054-a67d-e2fbb1c9212c"
        let nss = node.SubNodes.Value |> Array.copy
        nss.[i] <- mergeSameRoot domination nss.[i] newSubnode
        let ose = match newSubnode with
                  | Some x -> min node.OriginalSampleExponent x.OriginalSampleExponent
                  | None   -> node.OriginalSampleExponent
        Node(Guid.NewGuid(), node.Cell, ose, node.Layers, Some nss) :> INode

    let rec private mergeIntersectingBothCentered (domination : Dominance) (a : INode) (b : INode) : INode =
        invariant a.Cell.IsCenteredAtOrigin "41380857-b8c9-4f68-88d1-e279af0667b1"
        invariant b.Cell.IsCenteredAtOrigin "670db503-29bd-495e-b565-d1a0e45b3c08"
        failwith "Merging intersecting centered cells is not implemented (both)."

    let rec private mergeIntersectingFirstCentered (domination : Dominance) (a : INode) (b : INode) : INode =
        invariant a.Cell.IsCenteredAtOrigin "d8bf0eb6-7368-4c92-99b4-b7eafa6567f8"
        invariant (not b.Cell.IsCenteredAtOrigin) "39c66587-2f0f-48b4-9823-24d77df925c5"

        if a.Cell.Contains(b.Cell) then
            let qi = a.Cell.GetQuadrant(b.Cell).Value
            match a.SubNodes with
            | Some xs ->
                let subRootCell = a.Cell.GetQuadrant(qi)
                let bExtended = extendUpTo subRootCell (Some b)
                invariant bExtended.IsSome "87e552e4-0a5d-495c-8a43-90e0f8aba60c"
                match xs.[qi] with
                | Some qa ->
                    invariant (qa.Cell.Exponent = bExtended.Value.Cell.Exponent) "5efd17cd-0d45-47cd-be5e-255c4a4576e7"
                    let m = mergeSameRoot domination (Some qa) bExtended
                    invariant m.IsSome "886fc65b-4b88-4ac9-80dd-467a83031535"
                    let newA = setOrMergeIthSubnode domination qi a m
                    newA
                | None ->
                    let newA = setOrMergeIthSubnode domination qi a bExtended
                    newA
            | None ->
                failwith "not implemented"
        else
            match a.SubNodes with
            | None ->
                failwith "not implemented"
            | Some xs ->
                let subnodes = xs |> Array.map (fun sn -> 
                    match sn with 
                    | None -> None 
                    | Some sn -> extendUpTo sn.Cell.Parent (Some sn)
                    )
                let parentCell = a.Cell.Parent
                let parentLodLayers = Node.GenerateLodLayers subnodes parentCell
                let aNew = Node(Guid.NewGuid(), parentCell, a.OriginalSampleExponent, parentLodLayers, Some subnodes) :> INode |> Some
                invariant aNew.IsSome "ee364985-7daa-4837-b6cb-7bcbc21a314f"
                mergeIntersectingFirstCentered domination aNew.Value b

    let rec private mergeIntersecting (domination : Dominance) (a : INode option) (b : INode option) : INode option =
        match a, b with
        | Some a', Some b' ->

            if   a'.Cell.Exponent = b'.Cell.Exponent then
                mergeSameRoot     domination a b

            elif a'.Cell.Exponent < b'.Cell.Exponent then

                mergeIntersecting (flipDomination domination) b a

            else

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
                | true, true -> mergeIntersectingBothCentered domination a' b' |> Some
                | true, false -> mergeIntersectingFirstCentered domination a' b' |> Some
                | false, true -> mergeIntersectingFirstCentered (flipDomination domination) b' a' |> Some
                | false, false ->
                    invariant (a'.Cell.Exponent > b'.Cell.Exponent) "4b40bc08-b19d-4f49-b6e5-f321bf1e7dd0."
                    invariant (a'.Cell.Contains(b'.Cell))           "9a44a9ea-2996-46ff-9cc6-c9de1992465d."
                    invariant (not(b'.Cell.Contains(a'.Cell)))      "7d3465b9-90c7-4e7d-99aa-67e5383fb124."

                    let qi = a'.Cell.GetQuadrant(b'.Cell).Value
                    let qcell = a'.Cell.GetQuadrant(qi)

                    let a'' = if a'.IsLeafNode then 
                                Node(Guid.NewGuid(), a'.Cell, a'.OriginalSampleExponent, a'.Layers, Some <| Array.create 4 None) :> INode
                              else 
                                a'
                    b |> extendUpTo qcell |> setOrMergeIthSubnode domination qi a'' |> Some

        | Some _, None   -> a
        | None,   Some _ -> b
        | None,   None   -> None

    let private mergeNonIntersecting (domination : Dominance) (a : INode option) (b : INode option) : INode option =
        match a, b with
        | Some a1, Some b1 ->
            let commonRootCell = Cell2d(Box2d(a1.Cell.BoundingBox, b1.Cell.BoundingBox))
            let withCommonRoot = extendUpTo commonRootCell
            let a2 = (a |> withCommonRoot)
            let b2 = (b |> withCommonRoot)
            invariant (a2.IsSome && b2.IsSome) "cc431d22-9ad4-42bd-9317-015d2f6c520c."
            invariant (a2.Value.Cell = commonRootCell) "7730eae1-0212-40e5-b114-ff81c0c40762."
            invariant (b2.Value.Cell = commonRootCell) "00205e51-46c2-451a-bd7c-5dc45f18acc1."
            invariant (a2.Value.SampleExponent = b2.Value.SampleExponent) "178eedaa-89e2-473b-afbb-1beee112225d."
            mergeSameRoot domination a2 b2
        | Some _,  None    -> a
        | None,    Some _  -> b
        | None,    None    -> None

    let TryMerge (domination : Dominance) (a : INode option) ( b : INode option) : INode option =
        match a, b with
        | Some a', Some b' -> (if intersecting a' b' then mergeIntersecting else mergeNonIntersecting) domination a b
        | Some _,  None    -> a
        | None,    Some _  -> b
        | None,    None    -> None
