namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System
open System.Collections.Generic
open System.Diagnostics

module Query =

    type SampleMode =
        | Center
        | BottomLeft
        | BottomRight
        | TopLeft
        | TopRight
        | CustomRelativePosition of V2f
    with
        /// Value (0,0) corresponds to bottom left, and (1,1) to top right corner of sample.
        member this.RelativePosition with get () =
            match this with
            | Center                   -> V2f(0.5, 0.5)
            | BottomLeft               -> V2f(0.0, 0.0)
            | BottomRight              -> V2f(1.0, 0.0)
            | TopRight                 -> V2f(1.0, 1.0)
            | TopLeft                  -> V2f(0.0, 1.0)
            | CustomRelativePosition p -> p

    type Config = {
        /// Stop at this level-of-detail.
        MinExponent : int
        SampleMode : SampleMode
        Verbose : bool
    }
    with
        static member Default = { 
            MinExponent = Int32.MinValue
            SampleMode = SampleMode.Center
            Verbose = false
            }

    type NodeSelection =
        | FullySelected
        | CellsSelected of Cell2d[]
        | SubCellsSelected of Cell2d[]
        | WindowSelected of Box2l
        | SubtractionSelected of Box2l // full node except given box
        | NotSelected

    type Result = {
        Node : QNode
        Selection : NodeSelection
    }
    with
        member this.GetSampleCells () : Cell2d[] =
            match this.Selection with
            | NotSelected -> Array.empty
            | CellsSelected cells -> cells
            | SubCellsSelected cells -> cells
            | WindowSelected window -> this.Node.GetAllSamplesInsideWindow(window)
            | SubtractionSelected second -> this.Node.LayerSet.SampleWindow.GetAllSamplesFromFirstMinusSecond(second, this.Node.LayerSet.SampleExponent)
            | FullySelected -> this.Node.GetAllSamples()
        
        member this.GetSamples<'a when 'a : equality>(def : Durable.Def) : (Cell2d*'a)[] =
            let layer = this.Node.GetLayer<'a>(def)
            let cells = this.GetSampleCells ()
            let result = cells |> Array.map (fun p ->
                if p.Exponent = layer.Mapping.BufferOrigin.Exponent then
                    (p, layer.GetSample(Fail, p))
                else
                    (p, layer.GetSample(Fail, p.Parent))
                )
            result

    let mergeDominatingT0 = Stopwatch()
    let mergeDominatingT1 = Stopwatch()

    /// xs dominate always
    let private mergeDominatingPerSample (config : Config) (xs : Result list) (ys : Result list) (isSampleInside : Cell2d -> bool) : Result seq =

        if config.Verbose then 
            printfn "[mergeDominating] xs:"
            for x in xs |> Seq.collect(fun z -> z.GetSampleCells()) do
                printfn "[mergeDominating]     %A" x
            printfn "[mergeDominating] ys:"
            for y in ys |> Seq.collect(fun z -> z.GetSampleCells()) do
                printfn "[mergeDominating]     %A" y
                
        let xs = xs |> Seq.toArray

        let getInterferingXs (y : Result) =
            let yebb = y.Node.ExactBoundingBox
            xs |> Array.filter(fun x -> x.Node.ExactBoundingBox.Intersects(yebb))

        let resultSeq = seq {

            mergeDominatingT0.Start();

            // return ALL dominating samples ...
            if xs.Length > 0 then
                if config.Verbose then 
                    printfn "[mergeDominating] YIELD all dominating samples:"
                    for c in (xs |> Array.collect(fun z -> z.GetSampleCells())) do
                        printfn "[mergeDominating]   Y %A" c
                yield! xs
                
            // resolve dominated results ...
            for y in ys do
                let yebb = y.Node.ExactBoundingBox
                let zs = getInterferingXs y
                if zs.Length > 0 then

                    // dominated result IS interfered with -> resolve remaining (virtual) sample cells
                    let mutable result = List<Cell2d>(8192)

                    // all dominating sample cells intersecting dominated result
                    let zcs0 = zs |> Array.collect(fun z -> z.GetSampleCells())
                    let zcs1 = zcs0 |> Seq.filter(fun zc -> zc.BoundingBox.Intersects(yebb)) |> Seq.toList
                    let zcs = zcs1 |> Seq.map(fun c -> c.BoundingBox) |> Seq.toList |> List.toArray

                    // all sample cells of dominated result
                    let ycs = y.GetSampleCells ()

                    let mutable i = 0
                    for yc in ycs do
                        i <- i + 1
                        //printfn "[resolve dominate results] %d/%d" i ycs.Length
                        let inline isDominatedCellYcFullyCovered (bb : Box2d) = 
                            zcs |> Array.exists(fun zc -> zc.Contains(bb))
                        let inline isDominatedCellYcInterferedWith (bb : Box2d) =
                            zcs |> Array.exists(fun zc -> bb.Intersects(zc))

                        // -> resolve by recursively splitting sample into fragments
                        let rec resolve (s : Cell2d) =
                            let bb = s.BoundingBox
                            if isDominatedCellYcInterferedWith bb then
                                if isDominatedCellYcFullyCovered bb then
                                    () // discard sample, it is completely covered by dominating sample
                                else
                                    // partially covered, aaargh -> resolve recursively
                                    for q in s.Children do resolve q
                            else
                                if isSampleInside s then
                                    result.Add(s)
                                else
                                    ()

                        mergeDominatingT1.Start()
                        resolve yc
                        mergeDominatingT1.Stop()

                    if result.Count > 0 then
                        if config.Verbose then 
                            printfn "[mergeDominating] YIELD resolved samples:"
                            for c in result do
                                printfn "[mergeDominating]   Y %A" c

                        // DEBUG code
                        //let subcellExponent = y.Node.Cell.Exponent - 1
                        //for subcell in result do
                        //    if subcell.Exponent <> subcellExponent then failwith "Invariant d06646a3-0c0e-4f19-aad7-9045b06617d1."
                        //    ()

                        // subcells may not only be direct subcells due to recursive splitting (see resolve)
                        // this may happen if resolution of merged layers differs by more than 1 exponent
                        // at least this is what I think happens ;-)

                        yield { Node = y.Node; Selection = SubCellsSelected (result.ToArray()) }

                else
                    // dominated sample IS NOT interfered with -> return
                    if config.Verbose then printfn "[mergeDominating] YIELD non-dominating sample: %A" (y.GetSampleCells())

                    //match y.Node with | NoNode -> failwith "Invariant 5e786252-dbfe-4ff2-9e6c-0d59460f7ac9" | _ -> ()

                    yield y

            mergeDominatingT0.Stop()
        }

        let result = resultSeq |> Seq.toArray
        result

    let private mergePerSample (config : Config) (dom : Dominance) (xs : Result list) (ys : Result list) (isSampleInside : Cell2d -> bool) : Result list =
        match xs |> Seq.isEmpty, ys |> Seq.isEmpty with
        // both sequences contain values
        | false, false ->
            match dom with
            | FirstDominates
            | MoreDetailedOrFirst  -> mergeDominatingPerSample config xs ys isSampleInside |> Seq.toList
            | SecondDominates
            | MoreDetailedOrSecond -> mergeDominatingPerSample config ys xs isSampleInside |> Seq.toList
        // xs contains values, ys is empty -> no need to merge, just return xs
        | false, true  -> xs
        // xs is empty -> no need to merge, just return ys
        | true, _      -> ys

    let rec private merge'
            (config : Config)
            (isNodeFullyInside : QNodeRef -> bool) 
            (isNodeFullyOutside : QNodeRef -> bool)
            (isSampleInside : Cell2d -> bool)
            (dom : Dominance)
            (first : QNodeRef) (second : QNodeRef)
            (generic : QNodeRef -> Result seq)
            : Result seq = seq {

        if not (QMergeNode.isLegalMergeConstellation first second) then
            failwith "Invariant a2175093-5832-4cf4-97dc-90068b2a15b3."

        let recurse a b = merge' config isNodeFullyInside isNodeFullyOutside isSampleInside dom a b generic

        let perSampleMerge a b : Result list =
            //printf "[merge'  ] ... "
            let xs = generic a |> Seq.toList
            let ys = generic b |> Seq.toList
            //printf "merge first (%d) + second (%d)  %A ... " xs.Length ys.Length a.Cell
            let r = mergePerSample config dom xs ys isSampleInside
            //let foo = r |> List.collect (fun x -> x.GetSampleCells() |> Array.toList)
            //printfn "done %d" (r |> List.sumBy (fun x -> x.GetSampleCells().Length))
            r

        let firstIsFullyOutside = isNodeFullyOutside first
        let secondIsFullyOutside = isNodeFullyOutside second

        match isNodeFullyOutside first, isNodeFullyOutside second with
        
        | true, true ->
            if config.Verbose then printfn "[merge'] fully outside -> skip"
            ()
        
        | true, false ->
            if config.Verbose then printfn "[merge'] drop first (fully outside)"
            let r = generic second |> Seq.toList
            if r.Length > 0 then yield! r
        
        | false, true ->
            if config.Verbose then printfn "[merge'] drop second (fully outside)"
            let r = generic first |> Seq.toList
            if r.Length > 0 then yield! r
        
        | false, false ->
            match first, second with
        
            // load out-of-core nodes ...
            | OutOfCoreNode n1, OutOfCoreNode n2 ->
                if config.Verbose then printfn "[merge'] both out-of-core -> load"
                let r = recurse (n1.Load()) (n2.Load()) |> Seq.toList
                if r.Length > 0 then yield! r
            | OutOfCoreNode n1, b ->
                if config.Verbose then printfn "[merge'] first is out-of-core -> load"
                let r = recurse (n1.Load()) b |> Seq.toList
                if r.Length > 0 then yield! r
            | a, OutOfCoreNode n2 ->
                if config.Verbose then printfn "[merge'] second is out-of-core -> load"
                let r = recurse a (n2.Load()) |> Seq.toList
                if r.Length > 0 then yield! r

            // in-core
            | first, second ->

                let ebb1 = first.ExactBoundingBox
                let ebb2 = second.ExactBoundingBox
                let firstContainsSecond = ebb1.Contains(ebb2)

                match dom with

                | FirstDominates       ->
                    if firstContainsSecond then
                        if config.Verbose then printfn "[merge'] FirstDominates (firstContainsSecond)"
                        let r = generic first |> Seq.toList
                        if r.Length > 0 then yield! r
                    else
                        if config.Verbose then printfn "[merge'] FirstDominates (not firstContainsSecond)"
                        let r = perSampleMerge first second
                        if r.Length > 0 then yield! r

                | MoreDetailedOrFirst  ->
                    if config.Verbose then printfn "[merge'] MoreDetailedOrFirst"

                    match first, second with
                    | InMemoryInner a, InMemoryInner b ->
                        for i = 0 to 3 do
                            let aSub = a.SubNodes.[i]
                            let bSub = b.SubNodes.[i]

                            match aSub, bSub with
                            | NoNode, NoNode -> ()
                            | NoNode, _      -> 
                                if config.Verbose then printfn "[QUADRANTS %d] NoNode %A" i bSub.ExactBoundingBox
                                let r = generic bSub |> Seq.toList
                                if r.Length > 0 then yield! r
                            | _     , NoNode ->
                                if config.Verbose then printfn "[QUADRANTS %d] %A NoNode" i aSub.ExactBoundingBox
                                let r = generic aSub |> Seq.toList
                                if r.Length > 0 then yield! r
                            | _              ->
                                if config.Verbose then printfn "[QUADRANTS %d] %A %A" i aSub.ExactBoundingBox bSub.ExactBoundingBox
                                let r = recurse aSub bSub |> Seq.toList
                                if r.Length > 0 then yield! r
            
                    | InMemoryInner a, InMemoryNode b ->
                        if firstContainsSecond then
                            // mode is MoreDetailedOrFirst:
                            // first is definitely more detailed than second (it has subnodes, but second does not)
                            // and first completely overlaps second (so there is no rest from second visible outside bounds of first)
                            // -> therefore we can safely ignore second
                            if config.Verbose then printfn "[merge'] MoreDetailedOrFirst -> firstContainsSecond -> FIRST ONLY"
                            let r = generic first |> Seq.toList
                            if r.Length > 0 then yield! r

                        else
                            if config.Verbose then 
                                printfn "[merge'][FIRST  %A] %A" first.Cell first.ExactBoundingBox
                                Quadtree.PrintStructure true first
                                printfn "[merge'][SECOND %A] %A" second.Cell second.ExactBoundingBox
                                Quadtree.PrintStructure true second

                            let r = perSampleMerge first second
                            if r.Length > 0 then yield! r
                            ()

                    | _ ->
                        //failwith "Falling back to per-sample merge."
                        let r = perSampleMerge first second
                        if r.Length > 0 then yield! r

                | SecondDominates      ->
                    if config.Verbose then printfn "[merge'] SecondDominates -> flip"
                    let r = merge' config isNodeFullyInside isNodeFullyOutside isSampleInside FirstDominates second first generic |> Seq.toList
                    if r.Length > 0 then yield! r

                | MoreDetailedOrSecond ->
                    if config.Verbose then printfn "[merge'] MoreDetailedOrSecond -> flip"
                    let r = merge' config isNodeFullyInside isNodeFullyOutside isSampleInside MoreDetailedOrFirst second first generic |> Seq.toList
                    if r.Length > 0 then yield! r

    }


    /// The generic query function.
    let rec Generic 
            (config : Config) 
            (isNodeFullyInside : QNodeRef -> bool) 
            (isNodeFullyOutside : QNodeRef -> bool) 
            (isSampleInside : Cell2d -> bool)
            (root : QNodeRef) 
            : Result seq =

        let recurse = Generic config isNodeFullyInside isNodeFullyOutside isSampleInside

        if isNodeFullyOutside root then
            if config.Verbose then printfn "[Generic ] isFullyOutside"
            Seq.empty<Result>

        else
            seq {

                match root with
                | NoNode                    -> 
                    if config.Verbose then printfn "[Generic ] NoNode"
                    ()
            
                | OutOfCoreNode n           ->
                    if config.Verbose then printfn "[Generic ] OutOfCoreNode %A" n.Id
                    yield! n.Load() |> recurse
            
                | LinkedNode n               ->
                    if config.Verbose then printfn "[Generic ] LinkedNode %A" id
                    yield! n.Target |> recurse

                | InMemoryInner n           ->
                    if config.Verbose then printfn "[Generic ] InMemoryInner %A %A" n.Cell n.Id
                    for subnode in n.SubNodes do
                        match subnode with
                        | NoNode -> ()
                        | _ -> 
                            if config.Verbose then printfn "[Generic ]     subnode %A ->" n.Id
                            let r = subnode |> recurse |> Seq.toList
                            if r.Length > 0 then yield! r
            
                | InMemoryMerge n           ->
                    if config.Verbose then printfn "[Generic ] InMemoryMerge %A" n.Cell
                    if n.First.ExactBoundingBox.Intersects(n.Second.ExactBoundingBox) then
                        // first and second do overlap, so merge/resolve the samples of both
                        let r = merge' config isNodeFullyInside isNodeFullyOutside isSampleInside n.Dominance n.First n.Second (Generic config isNodeFullyInside isNodeFullyOutside isSampleInside) |> Seq.toList
                        if r.Length > 0 then yield! r
                    else
                        // first and second do not interfere, so simply return all samples of both
                        if config.Verbose then printfn "[Generic ]     yield first  %A" n.Cell
                        let r1 = n.First  |> recurse |> Seq.toList
                        if r1.Length > 0 then yield! r1
                        if config.Verbose then printfn "[Generic ]     yield second %A" n.Cell
                        let r2 = n.Second |> recurse |> Seq.toList
                        if r2.Length > 0 then yield! r2

                //| MultiMerge n              -> failwith "TODO 53cbaacb-02b1-4012-afff-c874d0e105e4"

                | InMemoryNode n            ->

                    if config.Verbose then printfn "[Generic ] InMemoryNode %A" n.Cell

                    invariantm (root.Cell.Exponent >= config.MinExponent)
                        (fun()->"Query cannot start at node with exponent smaller than configured minExponent.")
                        "bd20d469-970c-4c9c-b99c-6694dc90923d."

                    if isNodeFullyOutside root then
                        if config.Verbose then printfn "[Generic ] isFullyOutside"
                        ()
                    else
                        if config.Verbose then printfn "[Generic ] reached leaf or max depth"
                        if isNodeFullyInside root then
                            // fully inside
                            let result = { Node = n; Selection = FullySelected }
                            if config.Verbose then
                                printfn "[Generic ] fully inside (%A)" root.Cell
                                printfn "[Generic ] YIELD %A" result.Node.Cell
                                //for c in result.GetSampleCells() do printfn "[Generic ]   Ya %A" c
                            yield result
                        else
                            // partially inside
                            if config.Verbose then 
                                printfn "[Generic ] partially inside (%A)" root.Cell
                            let xs = n.GetAllSamples() |> Array.filter isSampleInside
                            if xs.Length > 0 then
                                let result = { Node = n; Selection = CellsSelected xs }
                                if config.Verbose then
                                    printfn "[Generic ] YIELD %A" result.Node.Cell
                                    //for c in result.GetSampleCells() do printfn "[Generic ]   Yb %A" c
                                yield result
                   
        }
    
    /// Returns all samples inside given tree.
    let All (config : Config) (root : QNodeRef) : Result seq =
        Generic config (fun _ -> true) (fun _ -> false) (fun _ -> true) root 

    /// Returns all samples inside given cell.
    let InsideCell (config : Config) (filter : Cell2d) (root : QNodeRef) : Result seq =
        let filterBb = filter.BoundingBox
        let isNodeFullyInside (n : QNodeRef) = filterBb.Contains n.ExactBoundingBox
        let isNodeFullyOutside (n : QNodeRef) = not (filterBb.Intersects n.ExactBoundingBox)
        let isSampleInside (n : Cell2d) = filter.Contains n
        Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root

    /// Returns all samples inside given box.
    let InsideBox (config : Config) (filter : Box2d) (root : QNodeRef) : Result seq =
        let isNodeFullyInside (n : QNodeRef) = filter.Contains n.ExactBoundingBox
        let isNodeFullyOutside (n : QNodeRef) = not (filter.Intersects n.ExactBoundingBox)
        let isSampleInside (n : Cell2d) = filter.Contains n.BoundingBox
        Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root

    /// Returns all samples inside given polygon.
    let InsidePolygon (config : Config) (filter : Polygon2d) (root : QNodeRef) : Result seq =
        let filter =
            let p = filter.WithoutMultiplePoints()
            if p.IsCcw() then p else p.Reversed
        let rpos = V2d(config.SampleMode.RelativePosition)
        let isNodeFullyInside (n : QNodeRef) =
            n.ExactBoundingBox.ToPolygon2dCCW().IsFullyContainedInside(filter)
        let isNodeFullyOutside (n : QNodeRef) =
            not (filter.Intersects(n.ExactBoundingBox.ToPolygon2dCCW()))
        let isSampleInside (n : Cell2d) =
            let bb = n.BoundingBox
            let p = V2d(bb.Min.X + bb.SizeX * rpos.X, bb.Min.Y + bb.SizeY * rpos.Y)
            let isInside = filter.Contains(p)
            isInside
        Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root

    /// Returns all samples within a given distance of a line.
    let NearLine (config : Config) (filter : Ray2d) (withinDistance : float) (root : QNodeRef) : Result seq =
        let filter = Ray2d(filter.Origin, filter.Direction.Normalized)
        let rpos = V2d(config.SampleMode.RelativePosition)
        let isNodeFullyInside (n : QNodeRef) =
            let wbb = n.ExactBoundingBox
            let halfDiagonal = wbb.Size.Length * 0.5
            let centerDist = filter.GetDistanceToRay(wbb.Center)
            centerDist + halfDiagonal <= withinDistance
        let isNodeFullyOutside (n : QNodeRef) =
            let wbb = n.ExactBoundingBox
            let halfDiagonal = wbb.Size.Length * 0.5
            let centerDist = filter.GetDistanceToRay(wbb.Center)
            centerDist >= withinDistance + halfDiagonal
        let isSampleInside (n : Cell2d) =
            let bb = n.BoundingBox
            let p = V2d(bb.Min.X + bb.SizeX * rpos.X, bb.Min.Y + bb.SizeY * rpos.Y)
            let dist = filter.GetDistanceToRay(p)
            dist <= withinDistance
        Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root

    (* 
        Faster implementation than "Generic". 
        Todo: replace all occurences of "Generic" with this one and then replace old "Generic" with this one.
    *)
    /// The generic query function.
    let rec Generic' 
            (config : Config) 
            (isNodeFullyInside : QNodeRef -> bool) 
            (isNodeFullyOutside : QNodeRef -> bool)
            (isSampleInside : Cell2d -> bool)
            (getSamplesInside : QNode -> Result option)
            (root : QNodeRef) 
            : Result seq =

        //printfn "[Generic'] %A" root.Cell

        let recurse = Generic' config isNodeFullyInside isNodeFullyOutside isSampleInside getSamplesInside

        if isNodeFullyOutside root then
            if config.Verbose then printfn "[Generic'] isFullyOutside"
            Seq.empty<Result>
        else
            seq {

                match root with
                | NoNode                    -> ()
                | OutOfCoreNode n           -> yield! n.Load() |> recurse
                | LinkedNode n              -> yield! n.Target |> recurse
                | InMemoryInner n           -> for subnode in n.SubNodes do yield! subnode |> recurse
                | InMemoryMerge n           ->

                    if config.Verbose then printfn "[Generic'] InMemoryMerge %A" n.Cell
                    if n.First.ExactBoundingBox.Intersects(n.Second.ExactBoundingBox) then

                        let r = merge' config isNodeFullyInside isNodeFullyOutside isSampleInside n.Dominance n.First n.Second (Generic' config isNodeFullyInside isNodeFullyOutside isSampleInside getSamplesInside) |> Seq.toList
                        if r.Length > 0 then yield! r

                        //let xs = (n.First |> recurse)
                        //let ys = (n.Second |> recurse)
                        //if config.Verbose then printfn "[Generic']     merge first + second  %A" n.Cell
                        //yield! merge config n.Dominance xs ys
                    else
                        // first and second do not interfere, so simply return all samples of both
                        if config.Verbose then printfn "[Generic']     yield first  %A" n.Cell
                        let r1 = n.First  |> recurse |> Seq.toList
                        if r1.Length > 0 then yield! r1
                        if config.Verbose then printfn "[Generic']     yield second %A" n.Cell
                        let r2 = n.Second |> recurse |> Seq.toList
                        if r2.Length > 0 then yield! r2

                //| MultiMerge n              -> failwith "TODO c475a1e4-d61a-421f-88a2-6dae48952e7d"

                | InMemoryNode n            ->

                    invariantm (n.Cell.Exponent >= config.MinExponent)
                        (fun()->"Query cannot start at node with exponent smaller than configured minExponent.")
                        "2c82bfd9-e7ff-4f8f-a990-156f0460a255."

                    if isNodeFullyOutside root then
                        if config.Verbose then printfn "[Generic'] isFullyOutside"
                        ()

                    else
                        //if root.IsLeafNode || n.Cell.Exponent = config.MinExponent then
                            // reached leaf or max depth
                            if config.Verbose then printfn "[Generic'] reached leaf or max depth"
                            if isNodeFullyInside root then
                                // fully inside
                                let result = { Node = n; Selection = FullySelected }
                                if config.Verbose then 
                                    printfn "[Generic'] fully inside"
                                    printfn "[Generic'] YIELD %A" result
                                yield result
                            else
                                // partially inside
                                if config.Verbose then printfn "[Generic'] partially inside"
                                match getSamplesInside n with
                                | None -> ()
                                | Some result -> 
                                    if config.Verbose then printfn "[Generic'] YIELD %A" result
                                    yield result
            }

    /// Returns all samples intersecting given cell.
    let IntersectsCell (config : Config) (filter : Cell2d) (root : QNodeRef) : Result seq =
        let filterBb = filter.BoundingBox
        let isNodeFullyInside  (n : QNodeRef) = filterBb.Contains n.ExactBoundingBox
        let isNodeFullyOutside (n : QNodeRef) = not (filterBb.Intersects n.ExactBoundingBox)
        let isSampleInside (n : Cell2d) =
            let bb = n.BoundingBox
            let relativeSamplePos = V2d(config.SampleMode.RelativePosition) * bb.Size
            let p = bb.Min + relativeSamplePos
            let isInside = filter.BoundingBox.Contains(p)
            isInside
        let getSamplesInside (n : QNode) =
            let overlap = filter.GetBoundsForExponent(n.LayerSet.SampleExponent).Intersection(n.LayerSet.SampleWindow)
            if overlap.SizeX > 0L && overlap.SizeY > 0L then
                Some { Node = n; Selection = WindowSelected overlap }
            else
                None

        Generic' config isNodeFullyInside isNodeFullyOutside isSampleInside getSamplesInside root

    /// Returns all samples inside given polygon.
    let InsidePolygon' (config : Config) (filter : Polygon2d) (root : QNodeRef) : Result seq =
        let filter =
            let p = filter.WithoutMultiplePoints()
            if p.IsCcw() then p else p.Reversed
        let isNodeFullyInside (n : QNodeRef) =
            n.ExactBoundingBox.ToPolygon2dCCW().IsFullyContainedInside(filter)
        let isNodeFullyOutside (n : QNodeRef) =
            not (filter.Intersects(n.ExactBoundingBox.ToPolygon2dCCW()))
        let isSampleInside (n : Cell2d) =
            let bb = n.BoundingBox
            let relativeSamplePos = V2d(config.SampleMode.RelativePosition) * bb.Size
            let p = bb.Min + relativeSamplePos
            let isInside = filter.Contains(p)
            isInside
        let getSamplesInside (n : QNode) : Result option =
            let relativeSamplePos = V2d(config.SampleMode.RelativePosition) * n.SampleSize
            let xs = n.GetAllSamples() |> Array.filter (fun s ->
                let p = s.BoundingBox.Min + relativeSamplePos
                let isInside = filter.Contains(p)
                isInside
                )
            if xs.Length > 0 then
                let result = { Node = n; Selection = CellsSelected xs }
                if config.Verbose then printfn "[InsidePolygon] n=%A, YIELD %A" n.Cell result
                Some result
            else
                if config.Verbose then printfn "[InsidePolygon] n=%A, NO SAMPLES inside polygon %A." n.Cell filter
                None

        Generic' config isNodeFullyInside isNodeFullyOutside isSampleInside getSamplesInside root



    [<Obsolete("Use IntersectsCell instead.")>]
    let IntersectsCell' = IntersectsCell

    /// Enumerates all samples in the quadtree,
    /// including samples from inner cells.
    let rec Full (root : QNodeRef) : Result seq = seq {
        match root with
        | NoNode          -> ()
        | InMemoryInner n -> for subnode in n.SubNodes do yield! Full subnode
        | InMemoryNode  n -> yield { Node = n; Selection = FullySelected }
        | InMemoryMerge n -> yield! Full n.First
                             yield! Full n.Second
        //| MultiMerge    _ -> failwith "TODO 0470fae0-1232-4167-9c1f-74002c9afd41"
        | LinkedNode    n -> yield! Full n.Target
        | OutOfCoreNode n -> yield! Full (n.Load())
    }

module Sample =
    
    type SampleResult = {
        Node : QNode
        Cells : Cell2d[]
        Positions : V2d[]
    }
    with
        member this.GetSampleCells () : Cell2d[] = this.Cells
        member this.GetSamples<'a when 'a : equality>(def : Durable.Def) : (V2d*Cell2d*'a)[] =
            let layer = this.Node.GetLayer<'a>(def)
            this.Cells |> Array.map2 (fun p c -> (p, c, layer.GetSample(Fail, c))) this.Positions

    let rec private PositionsWithBounds (config : Query.Config) (positions : V2d[]) (positionsBounds : Box2d) (n : QNodeRef) : SampleResult seq =

        seq {

            let sampleExponent = n.Cell.Exponent - n.SplitLimitExponent

            if sampleExponent >= config.MinExponent then

                let nodebb = n.ExactBoundingBox

                match n with
                | NoNode -> ()
                | InMemoryInner n ->

                    if sampleExponent > config.MinExponent then

                        let center = n.Cell.GetCenter()
                        let inline getQuadrant (p : V2d) =
                            if p.Y < center.Y then (if p.X < center.X then 0 else 1)
                            else (if p.X < center.X then 2 else 3)

                        // split positions by quadrants and recursively query subnodes
                        let gs = positions |> Array.groupBy getQuadrant
                        for (qi, ps) in gs do
                            if ps.Length > 0 then
                                yield! PositionsWithBounds config ps (Box2d(ps)) (n.SubNodes.[qi])

                | LinkedNode n -> yield! PositionsWithBounds config positions positionsBounds n.Target

                | InMemoryMerge n ->

                    if sampleExponent > config.MinExponent then
                        let xs = PositionsWithBounds config positions positionsBounds n.First
                        let ys = PositionsWithBounds config positions positionsBounds n.Second

                        let result = Dictionary<V2d, QNode * Cell2d>()
                        let set (r : SampleResult) = 
                            for i = 0 to r.Positions.Length - 1 do
                                result.[r.Positions.[i]] <- (r.Node, r.Cells.[i])

                        let add dominating other =
                            for r in other      do set r
                            for r in dominating do set r // overwrite others

                        let addMoreDetailed dominating other =
                            let replaceWhenMoreDetailed (r : SampleResult) =
                                for i = 0 to r.Positions.Length - 1 do
                                    let p = r.Positions.[i]
                                    let c = r.Cells.[i]
                                    
                                    
                                    //let (found, (n0, c0)) = result.TryGetValue(p)  // this somehow throws a nullreferenceexception when key does not exist 
                                    //if found then                                  // (probably the compiler generates some code which accesses the out param tuple which is not initialized ...)
                                    if (result.ContainsKey(p)) then
                                        let (n0, c0) = result[p]
                                        if c.Exponent < c0.Exponent then result.[p] <- (r.Node, c)
                                    else
                                        result.[p] <- (r.Node, c)
                            for r in dominating do set r
                            for r in other      do replaceWhenMoreDetailed r


                        match n.Dominance with
                        | FirstDominates        ->  add xs ys
                        | SecondDominates       ->  add ys xs
                        | MoreDetailedOrFirst   ->  addMoreDetailed xs ys
                        | MoreDetailedOrSecond  ->  addMoreDetailed ys xs

                        let gs = result |> Seq.map(fun kv ->
                                            let (n, c) = kv.Value
                                            (n, (kv.Key, c))
                                            )
                                        |> Seq.groupBy fst
                                        |> Seq.map(fun (n, xs) ->
                                            let ts = xs |> Seq.map snd |> Array.ofSeq
                                            let ps = ts |> Array.map fst
                                            let cs = ts |> Array.map snd
                                            { Node = n; Cells = cs; Positions = ps }
                                            )
                        yield! gs

                //| MultiMerge n -> failwith "TODO 7aab61ac-faf3-42a8-8f11-9f9080d8a6a8"

                | InMemoryNode n ->

                    if nodebb.ContainsMaxExclusive positionsBounds then
                        // fully inside
                        let cells = positions |> Array.map n.GetSample
                        yield { Node = n; Cells = cells; Positions = positions }
                    else
                        // partially inside (some positions have no samples)
                        let ps = positions |> Array.filter nodebb.ContainsMaxExclusive
                        let cells = ps |> Array.map n.GetSample
                        yield { Node = n; Cells = cells; Positions = ps }

                | OutOfCoreNode n ->
                    
                        let n = n.Load()
                        yield! PositionsWithBounds config positions positionsBounds n


                            
        }

    /// Returns samples at given positions.
    /// If there is no sample at a given position, it will not be included in the result sequence.
    /// This means, the result sequence may be empty.
    let Positions (config : Query.Config) (positions : V2d[]) (root : QNodeRef) : SampleResult seq=
        let bb = Box2d(positions)
        if root.ExactBoundingBox.Contains(bb) then
            PositionsWithBounds config positions bb root
        else
            Seq.empty
            
    /// Returns sample at given position, or None if there is no sample.
    let Position (config : Query.Config) (position : V2d) (root : QNodeRef) : SampleResult option =
        let result = Positions config [| position |] root |> Seq.toArray
        result |> Seq.tryHead

    let PositionTyped<'a when 'a : equality>  (config : Query.Config) (position : V2d) (def : Durable.Def) (root : QNodeRef) : 'a option =
        match Position config position root with
        | Some x -> 
            let xs = x.GetSamples<'a> def
            let (_, _, result) = xs.[0]
            Some result
        | None -> None

    /// Returns sample cell at given position, or None if there is no sample at this position.
    let TryGetCellAtPosition (config : Query.Config) (position : V2d) (root : QNodeRef) : Cell2d option =
        match Position config position root with
        | None -> None
        | Some result -> 
            if result.Cells.Length > 1 then failwith "Invariant 26b9fc1a-45ee-4e14-aede-ea50c32e3bed."
            result.Cells |> Array.tryHead