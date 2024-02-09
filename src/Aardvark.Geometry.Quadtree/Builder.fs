namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open System
open System.Collections.Generic
open Serialization
open System.Diagnostics

#nowarn "1337"

module Builder =

    let rec private build' (config : BuildConfig) (sampleExponent : int) (rootCell : Cell2d) (patches : LayerSet[]) =

        if config.Verbose then
            printfn "[build'] rootCell = %A, %d patches" rootCell patches.Length

        for p in patches do
            invariantm (p.SampleExponent = sampleExponent) 
                (fun () -> sprintf "Sample exponent %d does not match patch sample exponent %d." sampleExponent p.SampleExponent)
                "28d1fdd1-2da6-4329-a065-c134c1351ffc"

        match patches.Length with

        | 0 -> NoNode

        | 1 ->
            
            let theSinglePatch = patches[0]

            if config.Verbose then
                printfn "[build'] SINGLE patch (%A)" theSinglePatch.SampleWindow

            Quadtree.Build BuildConfig.Default theSinglePatch.Layers

        | n -> // n patches
        
            let ebb = patches |> Seq.map (fun patch -> patch.BoundingBox) |> Box2d
            
            let rootBounds = getBoundsForExponent sampleExponent rootCell
            if rootBounds.Size.X <= 256 && rootBounds.Size.Y <= 256 then
            
                let bbWindow = patches |> Seq.map (fun patch -> patch.SampleWindow) |> Box2l
                
                if config.Verbose then
                    printfn "[build'] MERGE %d patches; %A" n (bbWindow - rootBounds.Min)

                //if debugOutput then
                //    for patch in patches do
                //        printfn "[build']    %A (exact %A)" (patch.SampleWindow - rootBounds.Min) (bbWindow - rootBounds.Min)

                // adjust root cell for split limit
                let requiredRootCellSplitLimit = sampleExponent + BuildConfig.Default.SplitLimitPowerOfTwo

                let mutable rootCell = rootCell

                if requiredRootCellSplitLimit <> rootCell.Exponent then

                    invariantm (rootCell.Exponent < requiredRootCellSplitLimit) 
                        (fun () -> sprintf "Expected root cell exponent %d to be smaller than requiredRootCellSplitLimit %d." rootCell.Exponent requiredRootCellSplitLimit)
                        "4911adf3-7b87-4234-9bcc-bc3076df846e"

                    if config.Verbose then
                        printfn "[build'] must adjust root cell %A exponent to %d" rootCell requiredRootCellSplitLimit

                    while rootCell.Exponent < requiredRootCellSplitLimit do rootCell <- rootCell.Parent

                    if config.Verbose then
                        printfn "[build'] adjusted root cell is %A" rootCell

                let merged = LayerSet.Merge patches
                let qnode = QNode(Guid.NewGuid(), ebb, rootCell, BuildConfig.Default.SplitLimitPowerOfTwo, merged)
                
                if config.Verbose then
                    printfn "[build'] CREATED QNode with split limit = %d" qnode.SplitLimitExponent

                //let xs = qnode |> InMemoryNode |> Query.All Query.Config.Default |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.HeightsBilinear4f)) |> Array.ofSeq |> Array.collect id
                //for x in xs do printfn "%A" x
                qnode |> InMemoryNode

            else

                //let bbQuadrants = rootCell.Children |> Array.map (fun subCell -> subCell.GetBoundsForExponent(sampleExponent))
 
                let patchesPerQuadrant = 
                    rootCell.Children
                    |> Array.map (fun subCell -> 
                        let bbQuadrant = subCell.GetBoundsForExponent(sampleExponent)
                        let subPatches = patches |> Array.choose (fun b -> b.WithWindow bbQuadrant)
                        (subCell, subPatches)
                        )

                
                if config.Verbose then
                    printfn "[build'] SPLIT %d patches" n

                //for i in 0..3 do
                //    let (subCell, subPatches) = patchesPerQuadrant[i]
                //    printfn "%A%A[%d] size = %A, %d patches" rootCell ebb i bbWindow.Size subPatches.Length

                let subNodes = patchesPerQuadrant |> Array.map (fun (subCell, subPatches) ->
                    match subPatches.Length with
                    | 0 -> NoNode
                    | _ -> build' config sampleExponent subCell subPatches
                    )

                let hasMask = subNodes |> Array.exists (fun n -> n.HasMask)
                if config.Verbose && hasMask then
                    printfn "[build'] has mask %A" rootCell
                let result = { Id = Guid.NewGuid(); ExactBoundingBox = ebb; Cell = rootCell; SplitLimitExponent = BuildConfig.Default.SplitLimitPowerOfTwo; HasMask = hasMask; SubNodes = subNodes }
                result |> InMemoryInner

    let rec private build'' (config : BuildConfig) (rootCell : Cell2d) (patches : LayerSet[]) =
    
        let minSampleExponent = patches |> Seq.map (fun p -> p.SampleExponent) |> Seq.min

        if config.Verbose then
            printfn "[build''] ******** BEGIN ******** %d patches; rootCell = %A; minSampleExponent = %d " patches.Length rootCell minSampleExponent
            let mutable i = 0
            for patch in patches do
                printfn "[build''] | patches[%d] : e=%d origin=%A size=%A" i patch.SampleExponent patch.SampleWindow.Min patch.SampleWindow.Size
                i <- i + 1

        match patches.Length with

        | 0 -> 

            if config.Verbose then
                printfn "[build''] ZERO patches"

            NoNode

        | 1 ->
            
            if config.Verbose then
                printfn "[build''] SINGLE patch (%A)" patches[0].SampleWindow

            let theSinglePatch = patches[0]

            Quadtree.Build config theSinglePatch.Layers

        | 2 ->
            
            if config.Verbose then
                printfn "[build''] TWO patches (%A, %A)" patches[0].SampleWindow patches[1].SampleWindow

            let p0 = Quadtree.Build' config patches[0]
            let p1 = Quadtree.Build' config patches[1]

            Quadtree.Merge Dominance.MoreDetailedOrSecond p0 p1

        | n -> // n patches
        
            if config.Verbose then
                printfn "[build''] MULTIPLE patches (n=%d)" patches.Length

            let ebb = patches |> Seq.map (fun patch -> patch.BoundingBox) |> Box2d
            
            invariantm (config.SplitLimitPowerOfTwo >= 0) 
                (fun () -> sprintf "Expected config.SplitLimitPowerOfTwo to be non-negative, but found %d." config.SplitLimitPowerOfTwo)
                "95c43529-9649-42d6-9b47-c6f8fb6da301"

            let tileSize = 1 <<< config.SplitLimitPowerOfTwo
            let rootBounds = getBoundsForExponent minSampleExponent rootCell

            
            let patchesPerQuadrant = 
                rootCell.Children
                |> Array.map (fun subCell -> 
                    let subPatches =
                        patches
                        |> Array.choose (fun patch ->
                            let bbQuadrant = subCell.GetBoundsForExponent(patch.SampleExponent)
                            let r = patch.WithWindow bbQuadrant
                            //if config.Verbose && r.IsSome then
                            //    printfn "[build''] | subCell=%A patch.Window=%A bbQuadrant=%A" subCell patch.SampleWindow r.Value.SampleWindow
                            r
                            )
                    (subCell, subPatches)
                    )

            let patchesPerQuadrantCounts = patchesPerQuadrant |> Array.map (fun (_, ps) -> ps.Length)
            let canMakeProgress = patchesPerQuadrantCounts |> Array.forall (fun count -> count < n)

            //if rootBounds.Size.X <= tileSize && rootBounds.Size.Y <= tileSize then
            if rootCell.Exponent = minSampleExponent (*&& rootBounds.Size.X <= tileSize && rootBounds.Size.Y <= tileSize*) then
            
                // current tile size has reached the split limit:
                // 1. sort all patches from fine to coarse (from small to large sample exponents)
                // 2. flatten all patches/layers into a single patch/layer
                //    -> resulting layer has finest resolution of all patches
                //    -> coarser layers will be subsampled
                //    -> a mask indicates non-defined samples
                
                if config.Verbose then
                    printfn "[build''] MERGE %d patches; because reached split limit" n

                //if debugOutput then
                //    let bbWindow = patches |> Seq.map (fun patch -> patch.SampleWindow) |> Box2l
                //    for patch in patches do
                //        printfn "[DEBUG]    %A (exact %A)" (patch.SampleWindow - rootBounds.Min) (bbWindow - rootBounds.Min)

                // adjust root cell for split limit
                let requiredRootCellExponent = minSampleExponent + config.SplitLimitPowerOfTwo
                
                if config.Verbose then
                    printfn "[build''] | requiredRootCellExponent %d = minSampleExponent %d + config.SplitLimitPowerOfTwo %d" requiredRootCellExponent minSampleExponent config.SplitLimitPowerOfTwo
                    
                let mutable rootCell = rootCell
                
                if requiredRootCellExponent <> rootCell.Exponent then

                    invariantm (rootCell.Exponent < requiredRootCellExponent) 
                        (fun () -> sprintf "Expected root cell exponent %d to be smaller than requiredRootCellExponent %d." rootCell.Exponent requiredRootCellExponent)
                        "4911adf3-7b87-4234-9bcc-bc3076df846e"

                    if config.Verbose then
                        printfn "[build''] | must adjust root cell %A exponent to %d ..." rootCell requiredRootCellExponent

                    while rootCell.Exponent < requiredRootCellExponent do rootCell <- rootCell.Parent

                    if config.Verbose then
                        printfn "[build''] | adjusted root cell is %A" rootCell

                else

                    if config.Verbose then
                        printfn "[build''] | root cell %A already has requiredRootCellExponent %d" rootCell requiredRootCellExponent

                let flattended = LayerSet.Flatten config.Verbose patches

                let qnode = QNode(Guid.NewGuid(), ebb, rootCell, config.SplitLimitPowerOfTwo, flattended)
                
                if config.Verbose then
                    printfn "[build''] CREATED QNode with split limit = %d" qnode.SplitLimitExponent

                //let xs = qnode |> InMemoryNode |> Query.All Query.Config.Default |> Seq.map (fun x -> x.GetSamples<V4f>(Defs.HeightsBilinear4f)) |> Array.ofSeq |> Array.collect id
                //for x in xs do printfn "%A" x
                qnode |> InMemoryNode

            else
            
                // current tile size is still above split limit:
                // we sort all patches into the current cell's quadrants, and
                // if a patch overlaps multiple quadrants, then we split the patch (according to the quadrants)
 
                if config.Verbose then
                    printfn "[build''] SPLIT %d patches into quadrants of %A ..." n rootCell

                let patchesPerQuadrant = 
                    rootCell.Children
                    |> Array.map (fun subCell -> 
                        let subPatches =
                            patches
                            |> Array.choose (fun patch ->
                                let bbQuadrant = subCell.GetBoundsForExponent(patch.SampleExponent)
                                let r = patch.WithWindow bbQuadrant
                                //if config.Verbose && r.IsSome then
                                //    printfn "[build''] | subCell=%A patch.Window=%A bbQuadrant=%A" subCell patch.SampleWindow r.Value.SampleWindow
                                r
                                )
                        (subCell, subPatches)
                        )

                if config.Verbose then
                    printfn "[build''] SPLIT %d patches into quadrants of %A --> %A" n rootCell (patchesPerQuadrant |> Array.map(fun (_,xs) -> xs.Length))

                //for i in 0..3 do
                //    let (subCell, subPatches) = patchesPerQuadrant[i]
                //    printfn "%A%A[%d] size = %A, %d patches" rootCell ebb i bbWindow.Size subPatches.Length

                let subNodes = patchesPerQuadrant |> Array.map (fun (subCell, subPatches) ->
                    match subPatches.Length with
                    | 0 -> NoNode
                    | _ ->

                        let subnode = build'' config subCell subPatches
                        match subnode with
                        | NoNode -> failwith "AAAARGH"
                        | _ -> subnode
                    )

                let hasMask = subNodes |> Array.exists (fun n -> n.HasMask)
                if config.Verbose && hasMask then
                    printfn "[build''] has mask %A" rootCell
                let result = { Id = Guid.NewGuid(); ExactBoundingBox = ebb; Cell = rootCell; SplitLimitExponent = config.SplitLimitPowerOfTwo; HasMask = hasMask; SubNodes = subNodes }
                result |> InMemoryInner

    /// Creates a quadtree from many small patches.
    let Build (config : BuildConfig) (patches : LayerSet seq) : QNodeRef =
        let patches = patches |> Array.ofSeq
        let rootCell = patches |> Seq.map (fun patch -> patch.BoundingBox) |> Box2d |> Cell2d
        build'' config rootCell patches

/// Creates a quadtree from many small patches.
type Builder () =

    let l = obj()
    let patches = Dictionary<int, List<LayerSet>>()
    let mutable isEmpty = true
    let mutable layerCount = 0

    static let ensureDir path =
        let path = System.IO.Path.GetFullPath(path)
        let dir = System.IO.DirectoryInfo(path)
        if not dir.Exists then dir.Create()
        path

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
    member this.Add (patch : LayerSet option) : unit =
        match patch with
        | None -> ()
        | Some patch -> this.Add(patch)

    /// Add patch.
    member this.Add (patch : ILayer) : unit =
        this.Add(LayerSet([| patch |]))

    /// Add patch.
    member this.Add (patch : QNode) : unit =
        this.Add(patch.LayerSet)

    /// Add all leaf nodes of given quadtree as patches.
    member this.Add (root : QNodeRef) : unit =
        root |> Quadtree.EnumerateLeafNodesInMemory |> this.AddRange

    /// Add multiple patches.
    member this.AddRange (patches : seq<QNode>) : unit =
        for patch in patches do this.Add(patch)

    /// Add multiple patches.
    member this.AddRange (patches : seq<LayerSet>) : unit =
        for patch in patches do this.Add(patch)

    /// Build a quadtree from all the patches that have been added to this builder.
    member this.Build (config : BuildConfig) : QNodeRef option =
    
        let mutable mergesCount = 0

        patches 
        // (1) sort per-resolution patch lists from coarse to fine resolution ...
        |> Seq.sortByDescending (fun kv -> kv.Key) |> Seq.map (fun kv -> kv.Value)
        // (2) create a quadtree for each resolution ...
        |> Seq.map (Builder.Build config)
        // (3) merge quadtrees with finer resolution always dominating ...
        |> Seq.fold (fun state item -> 
            match state with
            | None -> Some item
            | Some state -> 
                mergesCount <- mergesCount + 1
                printfn "[Builder.Build()] mergesCount -> %d" mergesCount
                Quadtree.Merge Dominance.SecondDominates state item |> Some
            ) 
            None // initial state

    /// Build a quadtree from all the patches that have been added to this builder.
    member this.Build2 (config : BuildConfig) : QNodeRef option =
        let allPatches = this.GetPatches()
        Builder.Build config allPatches |> Some

    /// Enumerate all patches.
    member this.GetPatches () : seq<LayerSet> =
        patches |> Seq.map (fun kv -> kv.Value) |> Seq.collect id

    member this.Print () : unit =

        printfn "Builder("
        let ps = patches |> Seq.sortByDescending (fun kv -> kv.Key) |> Seq.map (fun kv -> kv.Value) |> Seq.collect id
        for p in ps do
          printfn "  exp = %5d  bb = %A" p.SampleExponent p.BoundingBox

        let bbGlobal = Box2d(ps |> Seq.map (fun p -> p.BoundingBox))
        printfn "  global bb        = %A" bbGlobal
        printfn "  global root cell = %A" (Cell2d bbGlobal)

        printfn "  )"
        ()

    /// Save builder. Returns id of saved builder.
    member this.Save (options : SerializationOptions) : Guid =
        Defs.init ()
        let patches = this.GetPatches() |> Array.ofSeq
        Serialization.SavePatches options patches

    /// Load builder with given id.
    static member Load (options : SerializationOptions) (id : Guid) : Builder option =
        Defs.init ()
        match Serialization.LoadPatches options id with
        | None -> None
        | Some patches ->
            let builder = Builder()
            builder.AddRange(patches)
            Some builder

    /// Export builder to directory.
    member this.Export (dir : string) : Guid =

        let storeDir = ensureDir dir

        let now = DateTimeOffset.Now
        let fileNameKey = sprintf("builder.%04d%02d%02d%02d%02d%02d.%d.key.txt") now.Year now.Month now.Day now.Hour now.Minute now.Second now.Ticks

        use store = new Uncodium.SimpleStore.SimpleDiskStore(System.IO.Path.Combine(storeDir, "store.dur"))
        let options = SerializationOptions.SimpleStore(store)
        let key = this.Save(options)

        System.IO.File.WriteAllText(System.IO.Path.Combine(storeDir, fileNameKey), key.ToString())

        key

    /// Imports builder with given id from directory.
    static member Import (dir : string, id : Guid) : Builder option =

        Defs.init ()

        let storeFileName =
            if System.IO.File.Exists(dir) then
                dir
            else
                if System.IO.Directory.Exists(dir) then
                    System.IO.Path.Combine(dir, "store.dur")
                else
                    sprintf "Unknown path %s. Error 094f64b9-ba1e-4d9d-a5e1-bc471665f332." dir |> failwith

        use store = new Uncodium.SimpleStore.SimpleDiskStore(storeFileName)
        let options = SerializationOptions.SimpleStore(store)
        match Serialization.LoadPatches options id with
        | None -> None
        | Some patches ->
            let builder = Builder()
            builder.AddRange(patches)
            Some builder
