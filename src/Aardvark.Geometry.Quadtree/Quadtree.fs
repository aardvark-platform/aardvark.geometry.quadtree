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

            let lodLayers = Node.GenerateLodLayers subNodes

            Node(cell, lodLayers, Some subNodes) :> INode
        
        else
        
            Node(cell, layers) :> INode

    let Build (config : BuildConfig) ([<ParamArray>] layers : ILayer[]) : INode =
        let globalBounds = layers |> Array.map Layer.BoundingBox |> Box2d
        let rootCell = Cell2d(globalBounds)
        //printfn "global bounds ... %A" globalBounds
        //printfn "root cell ....... %A" rootCell
        build config rootCell layers

    let TryMerge a b = Merge.TryMerge a b

    let Merge a b = (TryMerge (Some a) (Some b)).Value

    ()