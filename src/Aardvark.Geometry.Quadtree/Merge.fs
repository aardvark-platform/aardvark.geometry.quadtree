namespace Aardvark.Geometry.Quadtree

open Aardvark.Base

(*
    Merge.
*)

module Merge =

    let inline private invariant condition id =
        if not condition then failwith <| sprintf "Invariant %s" id

    let inline private intersecting (a : INode) (b : INode) = a.Cell.Intersects(b.Cell)

    let private extendUpTo (root : Cell2d) (node : INode option) : INode option =
        match node with
        | None -> None
        | Some node ->
            invariant (not(root.Contains(node.Cell)))          "a48ca4ab-3f20-45ff-bd3c-c08f2a8fcc15."
            invariant (root.Exponent < node.Cell.Exponent)     "cda4b28d-4449-4db2-80b8-40c0617ecf22."

            if root.Exponent = node.Cell.Exponent then
                Some node
            else
                invariant (root.Exponent > node.Cell.Exponent) "56251fd0-5344-4d0a-b76b-815cdd5a7607."
                let parentCell = node.Cell.Parent
                let qi = root.GetQuadrant(parentCell)
                invariant qi.HasValue                          "09575aa7-38b3-4afa-bb63-389af3301fc0."
                let subnodes = Array.create 4 None
                subnodes.[qi.Value] <- Some node
                let lodLayers = Node.GenerateLodLayers subnodes
                Node(parentCell, lodLayers, Some subnodes) :> INode |> Some

    let private mergeLayers (a : ILayer[]) (b : ILayer[]) : ILayer[] =
        let mutable merged = Map.empty
        let merge (x : ILayer) : unit =
            match Map.tryFind x.Def.Id merged with

            | Some (y : ILayer) ->

                let handleCollision () =
                    match seq {x;y} |> Layer.Merge with
                    | Some z -> merged <- merged |> Map.add z.Def.Id z
                    | None -> ()

                if x.Mapping = y.Mapping then
                    if   x.SampleExponent < y.SampleExponent then merged <- merged |> Map.add x.Def.Id x
                    elif y.SampleExponent < x.SampleExponent then merged <- merged |> Map.add y.Def.Id y
                    else handleCollision()
                else
                    handleCollision()

            | None   -> merged <- merged |> Map.add x.Def.Id x

        for x in a do merge x
        for y in b do merge y
        merged |> Map.toArray |> Array.map (fun (_, v) -> v)

    let rec private mergeSameRoot (a : INode option) (b : INode option) : INode option =
        match a, b with
        | Some a, Some b ->
            invariant (a.Cell = b.Cell) "641da2e5-a7ea-4692-a96b-94440453ff1e."
            let cell = a.Cell
            match a.SubNodes, b.SubNodes with
            | Some xs, Some ys -> // inner/inner
                let zs = Array.map2 mergeSameRoot xs ys
                let layers = mergeLayers a.Layers b.Layers
                Node(cell, layers, Some zs) :> INode |> Some
            | Some xs, None    -> // inner/leaf
                let lodLayers = Node.GenerateLodLayers xs
                Node(cell, lodLayers, Some xs) :> INode |> Some
            | None,    Some ys -> // leaf/inner
                let lodLayers = Node.GenerateLodLayers ys
                Node(cell, lodLayers, Some ys) :> INode |> Some
            | None,    None    -> // leaf/leaf
                let layers = mergeLayers a.Layers b.Layers
                Node(cell, layers, None) :> INode |> Some
        | Some a, None   -> Some a
        | None,   Some b -> Some b
        | None,   None   -> None
    
    let private setOrMergeIthSubnode (i : int) (node : INode) (newSubnode : INode option) : INode =
        invariant node.SubNodes.IsSome "f74ba958-cf53-4336-944f-46ef2c2b8893"
        if newSubnode.IsSome then invariant (node.Cell.GetQuadrant(i) = newSubnode.Value.Cell) "f5b92710-39de-4054-a67d-e2fbb1c9212c"
        let nss = node.SubNodes.Value |> Array.copy
        nss.[i] <- mergeSameRoot nss.[i] newSubnode
        Node(node.Cell, node.Layers, Some nss) :> INode

    let rec private mergeIntersecting (a : INode option) (b : INode option) : INode option =
        match a, b with
        | Some a', Some b' ->
            if   a'.Cell.Exponent = b'.Cell.Exponent then mergeSameRoot     a b
            elif a'.Cell.Exponent < b'.Cell.Exponent then mergeIntersecting a b
            else
                invariant (a'.Cell.Exponent > b'.Cell.Exponent) "4b40bc08-b19d-4f49-b6e5-f321bf1e7dd0."
                invariant (not(a'.Cell.Contains(b'.Cell)))      "9a44a9ea-2996-46ff-9cc6-c9de1992465d."
                invariant (b'.Cell.Contains(a'.Cell))           "7d3465b9-90c7-4e7d-99aa-67e5383fb124."

                let qi = a'.Cell.GetQuadrant(b'.Cell).Value
                let qcell = a'.Cell.GetQuadrant(qi)

                let a'' = if a'.IsLeafNode then Node(a'.Cell, a'.Layers, Some <| Array.create 4 None) :> INode else a'
                b |> extendUpTo qcell |> setOrMergeIthSubnode qi a'' |> Some

        | Some _, None   -> a
        | None,   Some _ -> b
        | None,   None   -> None

    let private mergeNonIntersecting (a : INode option) (b : INode option) : INode option =
        match a, b with
        | Some a', Some b' ->
            let withCommonRoot = extendUpTo <| Cell2d(Box2d(a'.Cell.BoundingBox, b'.Cell.BoundingBox)) 
            mergeSameRoot (a |> withCommonRoot) (b |> withCommonRoot)
        | Some _,  None    -> a
        | None,    Some _  -> b
        | None,    None    -> None

    let TryMerge (a : INode option) ( b : INode option) : INode option =
        match a, b with
        | Some a', Some b' -> (if intersecting a' b' then mergeIntersecting else mergeNonIntersecting) a b
        | Some _,  None    -> a
        | None,    Some _  -> b
        | None,    None    -> None
