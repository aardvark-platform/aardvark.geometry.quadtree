namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open System
open System.Collections.Generic

module Builder =

    let private debugOutput = false

    let rec private build' (sampleExponent : int) (rootCell : Cell2d) (patches : LayerSet[]) =

        if debugOutput then
            printfn "[DEBUG] build' rootCell = %A, %d patches" rootCell patches.Length

        match patches.Length with

        | 0 -> NoNode

        | 1 ->
            
            let theSinglePatch = patches[0]

            if debugOutput then
                printfn "[DEBUG] SINGLE patch (%A)" theSinglePatch.SampleWindow

            Quadtree.Build BuildConfig.Default theSinglePatch.Layers

        | n -> // n patches
        
            let ebb = patches |> Seq.map (fun patch -> patch.BoundingBox) |> Box2d
            
            let rootBounds = getBoundsForExponent sampleExponent rootCell
            if rootBounds.Size.X <= 256 && rootBounds.Size.Y <= 256 then
            
                let bbWindow = patches |> Seq.map (fun patch -> patch.SampleWindow) |> Box2l
                
                if debugOutput then
                    printfn "[DEBUG] MERGE %d patches; %A" n (bbWindow - rootBounds.Min)

                
                //if debugOutput then
                //    for patch in patches do
                //        printfn "[DEBUG]    %A (exact %A)" (patch.SampleWindow - rootBounds.Min) (bbWindow - rootBounds.Min)

                let merged = LayerSet.Merge patches
                let qnode = QNode(Guid.NewGuid(), ebb, rootCell, BuildConfig.Default.SplitLimitPowerOfTwo, merged)
                let xs = qnode |> InMemoryNode |> Query.All Query.Config.Default |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.HeightsBilinear4f)) |> Array.ofSeq |> Array.collect id
                //for x in xs do printfn "%A" x
                qnode |> InMemoryNode

            else

                let bbQuadrants = rootCell.Children |> Array.map (fun subCell -> subCell.GetBoundsForExponent(sampleExponent))
 
                let patchesPerQuadrant = 
                    rootCell.Children
                    |> Array.map (fun subCell -> 
                        let bbQuadrant = subCell.GetBoundsForExponent(sampleExponent)
                        let subPatches = patches |> Array.choose (fun b -> b.WithWindow bbQuadrant)
                        (subCell, subPatches)
                        )

                
                if debugOutput then
                    printfn "[DEBUG] SPLIT %d patches" n

                //for i in 0..3 do
                //    let (subCell, subPatches) = patchesPerQuadrant[i]
                //    printfn "%A%A[%d] size = %A, %d patches" rootCell ebb i bbWindow.Size subPatches.Length

                let subNodes = patchesPerQuadrant |> Array.map (fun (subCell, subPatches) ->
                    match subPatches.Length with
                    | 0 -> NoNode
                    | _ -> build' sampleExponent subCell subPatches
                    )

                let hasMask = subNodes |> Array.exists (fun n -> n.HasMask)
                let result = { Id = Guid.NewGuid(); ExactBoundingBox = ebb; Cell = rootCell; SplitLimitExponent = BuildConfig.Default.SplitLimitPowerOfTwo; HasMask = hasMask; SubNodes = subNodes }
                result |> InMemoryInner

    /// Creates a quadtree from many small patches.
    let Build (patches : LayerSet seq) : QNodeRef =
        let patches = patches |> Array.ofSeq
        let rootCell = patches |> Array.map (fun patch -> patch.BoundingBox) |> Box2d |> Cell2d
        let sampleExponent = (patches |> Array.distinctBy (fun x -> x.SampleExponent) |> Array.exactlyOne).SampleExponent
        build' sampleExponent rootCell patches


/// Creates a quadtree from many small patches.
type Builder () =

    let l = obj()
    let patches = Dictionary<int, List<LayerSet>>()
    let mutable isEmpty = true
    let mutable layerCount = 0

    /// Add patch.
    member this.Add (patch : LayerSet) : unit = lock l (fun () ->

        if isEmpty then
            layerCount <- patch.Layers.Length
            isEmpty <- true

        invariantm 
            (patch.Layers.Length = layerCount)
            (fun()->sprintf "Expected %d layers, but patch has %d. layers" layerCount patch.Layers.Length)
            "e76e1577-4963-441c-884c-dc32da0b3b15"
            
        // get or create patch list for patch's resolution (a.k.a. sample exponent)
        let list =
            match patches.TryGetValue(patch.SampleExponent) with
            | (true, xs) -> xs
            | (false, _) -> let xs = List<LayerSet>()
                            patches[patch.SampleExponent] <- xs
                            xs
        // add patch to list
        list.Add(patch)
        )

    /// Add patch.
    member this.Add (patch : ILayer) : unit =
        this.Add(LayerSet([| patch |]))

    /// Add patch.
    member this.Add (patch : QNode) : unit =
        this.Add(patch.LayerSet)

    /// Build a quadtree from all the patches that have been added to this builder.
    member this.Build () : QNodeRef option =

        patches 
        // (1) sort per-resolution patch lists from coarse to fine resolution ...
        |> Seq.sortByDescending (fun kv -> kv.Key) |> Seq.map (fun kv -> kv.Value)
        // (2) create a quadtree for each resolution ...
        |> Seq.map Builder.Build
        // (3) merge quadtrees with finer resolution always dominating ...
        |> Seq.fold (fun state item -> 
            match state with
            | None -> Some item
            | Some state -> Quadtree.Merge Dominance.SecondDominates state item |> Some
            ) 
            None // initial state
        
        