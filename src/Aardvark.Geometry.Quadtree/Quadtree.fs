﻿namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open System

(*
    Quadtree.
*)

type BuildConfig = {
    /// Node is split if width and/or height is greater than 2^SplitLimitPowerOfTwo.
    SplitLimitPowerOfTwo : int
}
with
    static member Default = { SplitLimitPowerOfTwo = 8 }

[<AutoOpen>]
module Quadtree =

    let rec private tryCount a b (root : QNodeRef) =
        match root.TryGetInMemory() with 
        | None -> 0 
        | Some n -> 
            match n.SubNodes with 
            | None -> a 
            | Some ns -> b + (ns |> Array.sumBy (tryCount a b))

    let rec CountNodes root = root |> tryCount 1 1
    let rec CountLeafs root = root |> tryCount 1 0
    let rec CountInner root = root |> tryCount 0 1

    let rec private build (config : BuildConfig) (rootCell : Cell2d) (originalSampleExponent : int) (layers : ILayer[]) : QNode =
    
        invariant (layers.Length > 0)                                                       "dccf4ce3-b163-41b7-9bd9-0a3e7b76e3a5"
        invariant (layers |> Array.groupBy (fun l -> l.SampleExponent) |> Array.length = 1) "4071f97e-1809-422a-90df-29c774cedc7b"

        //printfn "[build] %A" cell
        let layerExponent = layers.[0].SampleExponent
        
        //printfn "min exp ......... %A" minExp
        
        //let needToSplit = layers |> Array.map Layer.Window |> Array.exists (fun box -> 
        //    int box.SizeX > config.SplitLimit || int box.SizeY > config.SplitLimit
        //    )

        let localResolution = rootCell.Exponent - layerExponent
        let needToSplit = localResolution > config.SplitLimitPowerOfTwo
        
        if needToSplit then
                    
            //printfn "cell box ........ %A" (cell.GetBoundsForExponent(minExp))
            //for layer in layers do
            //    printfn "layer %-20s %A" layer.Def.Name layer.Box
            //if cell.Exponent = 8 then 
            //    printfn "what?????????????????????????????????????????????????????????????????????????????????????????????????"

            let subLayers = rootCell.Children |> Array.map (fun subCell ->
                let subBox = subCell.GetBoundsForExponent(layerExponent)
                let subLayers = layers |> Array.choose (fun l -> l.WithWindow subBox)
                (subCell, subLayers) 
                )

            let subNodes = subLayers |> Array.map (fun (subCell, subLayers) ->
                match subLayers.Length with
                | 0 -> NoNode
                | _ -> let n = build config subCell originalSampleExponent subLayers
                       InMemoryNode n
                    //printfn "  sub cell %A"  subCell
                    //for layer in subLayers do
                    //    printfn "    layer %-20s" layer.Def.Name
                    //    printfn "          %A with area %A" layer.Box layer.Box.Area
                    
                )

            let children = rootCell.Children
            for i = 0 to 3 do
                match subNodes.[i].TryGetInMemory() with
                | Some x -> invariant (x.Cell = children.[i])                               "15f2c6c3-6f5b-4ac0-9ec0-8ab968ac9c2e"
                | None -> ()

            let lodLayers = QNode.GenerateLodLayers subNodes rootCell

            QNode(Guid.NewGuid(), rootCell, config.SplitLimitPowerOfTwo, originalSampleExponent, lodLayers, Some subNodes)
        
        else
        
            QNode(rootCell, config.SplitLimitPowerOfTwo, originalSampleExponent, layers)

    /// At least 1 layer is required, and
    /// all layers must have the same sample exponent and sample window.
    let Build (config : BuildConfig) ([<ParamArray>] layers : ILayer[]) : QNodeRef =
        
        invariantm (layers.Length > 0) "Can't build quadtree with 0 layers."                "6216df3f-279c-415f-a435-bdb35d274e39"

        let layerExponents = layers |> Array.groupBy (fun x -> x.SampleExponent)
        invariantm (layerExponents.Length = 1) "All layers must have same resolution."      "a25e5f58-c22a-436e-81be-69afb2b37492"

        let layerWindows = layers |> Array.groupBy (fun x -> x.SampleWindow)
        invariantm (layerWindows.Length = 1) "All layers must have same samples window."    "36488503-b5b8-4d80-8992-b713e7552480"
            
        let sampleExponent = layers.[0].SampleExponent
        let minRootExponent = sampleExponent + config.SplitLimitPowerOfTwo

        let globalBounds = layers |> Array.map Layer.BoundingBox |> Box2d
        let mutable rootCell = Cell2d(globalBounds)
        while rootCell.Exponent < minRootExponent do
            rootCell <- rootCell.Parent

        build config rootCell sampleExponent layers |> InMemoryNode

    let Merge (domination : Dominance) (a : QNodeRef) (b : QNodeRef) = Merge.Merge domination a b

    /// Save quadtree. Returns id of root node, or Guid.Empty if empty quadtree.
    let Save (options : SerializationOptions) (qtree : QNodeRef) : Guid =
        match qtree with
        | InMemoryNode n -> n.Save options
        | OutOfCoreNode (id, _) -> id
        | NoNode -> Guid.Empty

    let Load (options : SerializationOptions) (id : Guid) : QNodeRef =
        QNode.Load options id