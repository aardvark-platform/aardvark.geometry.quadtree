namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open System

(*
    Quadtree.
*)

type BuildConfig = {
    SplitLimit : int
}
with
    static member Default = { SplitLimit = 256 }

[<AutoOpen>]
module Quadtree =

    let rec private tryCount a b (root : INode option) =
        match root with 
        | None -> 0 
        | Some r -> match r.SubNodes with 
                    | None -> a 
                    | Some ns -> b + (ns |> Array.sumBy (tryCount a b))

    let rec TryCountNodes root = root |> tryCount 1 1
    let rec TryCountLeafs root = root |> tryCount 1 0
    let rec TryCountInner root = root |> tryCount 0 1

    let private count a b root = tryCount a b (Some root)
    
    let rec CountNodes root = root |> count 1 1
    let rec CountLeafs root = root |> count 1 0
    let rec CountInner root = root |> count 0 1

    let rec private build (config : BuildConfig) (cell : Cell2d) (layers : ILayer[]) : INode =
    
        //printfn "[build] %A" cell
        let minExp = layers |> Array.map (fun l -> l.SampleExponent) |> Array.min
        
        //printfn "min exp ......... %A" minExp
        
        let needToSplit = layers |> Array.map Layer.Window |> Array.exists (fun box -> 
            int box.SizeX > config.SplitLimit || int box.SizeY > config.SplitLimit
            )
        
        if needToSplit then
                    
            //printfn "cell box ........ %A" (cell.GetBoundsForExponent(minExp))
            //for layer in layers do
            //    printfn "layer %-20s %A" layer.Def.Name layer.Box
            //if cell.Exponent = 8 then 
            //    printfn "what?????????????????????????????????????????????????????????????????????????????????????????????????"

            let subLayers = cell.Children |> Array.map (fun subCell ->
                let subBox = subCell.GetBoundsForExponent(minExp)
                let subLayers = layers |> Array.map (fun l -> l.WithWindow subBox) |> Array.choose id
                (subCell, subLayers) 
                )

            let subNodes = subLayers |> Array.map (fun (subCell, subLayers) ->
                match subLayers.Length with
                | 0 -> None
                | _ -> Some <| build config subCell subLayers
                    //printfn "  sub cell %A"  subCell
                    //for layer in subLayers do
                    //    printfn "    layer %-20s" layer.Def.Name
                    //    printfn "          %A with area %A" layer.Box layer.Box.Area
                    
                )

            let children = cell.Children
            for i = 0 to 3 do
                match subNodes.[i] with
                | Some x -> invariant (x.Cell = children.[i]) "15f2c6c3-6f5b-4ac0-9ec0-8ab968ac9c2e."
                | None -> ()

            let lodLayers = Node.GenerateLodLayers subNodes

            Node(Guid.NewGuid(), cell, lodLayers, Some subNodes) :> INode
        
        else
        
            Node(cell, layers) :> INode

    /// At least 1 layer is required, and
    /// all layers must have the same sample exponent and sample window.
    let Build (config : BuildConfig) ([<ParamArray>] layers : ILayer[]) : INode =
        
        if layers.Length = 0 then
            failwith "Can't build quadtree with 0 layers. Invariant 6216df3f-279c-415f-a435-bdb35d274e39."

        let layerExponents = layers |> Array.groupBy (fun x -> x.SampleExponent)
        if layerExponents.Length <> 1 then 
            failwith "All layers must have same resolution. Invariant a25e5f58-c22a-436e-81be-69afb2b37492."

        let layerWindows = layers |> Array.groupBy (fun x -> x.SampleWindow)
        if layerWindows.Length <> 1 then 
            failwith "All layers must have same samples window. Invariant 36488503-b5b8-4d80-8992-b713e7552480."

        let globalBounds = layers |> Array.map Layer.BoundingBox |> Box2d
        let rootCell = Cell2d(globalBounds)
        build config rootCell layers

    let TryMerge a b = Merge.TryMerge a b

    let Merge a b = (TryMerge (Some a) (Some b)).Value

    ()