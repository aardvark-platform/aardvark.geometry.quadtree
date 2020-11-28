namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System

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
            | SubtractionSelected second -> this.Node.SampleWindow.GetAllSamplesFromFirstMinusSecond(second, this.Node.SampleExponent)
            | FullySelected -> this.Node.GetAllSamples()
        member this.GetSamples<'a>(def : Durable.Def) : (Cell2d*'a)[] =
            let layer = this.Node.GetLayer<'a>(def)
            let cells = this.GetSampleCells ()
            let result = cells |> Array.map (fun p -> (p, layer.GetSample(Fail, p)))
            result

    /// The generic query function.
    let rec Generic 
            (config : Config) 
            (isNodeFullyInside : QNode -> bool) 
            (isNodeFullyOutside : QNode -> bool) 
            (isSampleInside : Cell2d -> bool)
            (n : QNode) 
            : Result seq =

        if config.Verbose then printfn "\n[Generic ] %A %A" n.Cell n.SampleWindow

        seq {

            invariantm (n.Cell.Exponent >= config.MinExponent)
                "Query cannot start at node with exponent smaller than configured minExponent."
                "bd20d469-970c-4c9c-b99c-6694dc90923d."

            if isNodeFullyOutside n then
                if config.Verbose then printfn "[Generic ] isFullyOutside"
                ()
            else
                if n.IsLeafNode || n.SampleExponent = config.MinExponent then
                    // reached leaf or max depth
                    if config.Verbose then printfn "[Generic ] reached leaf or max depth"
                    if isNodeFullyInside n then
                        // fully inside
                        let result = { Node = n; Selection = FullySelected }
                        if config.Verbose then
                            printfn "[Generic ] fully inside"
                            printfn "[Generic ] YIELD %A" result
                        yield result
                    else
                        // partially inside
                        if config.Verbose then printfn "[Generic ] partially inside"
                        let xs = n.GetAllSamples() |> Array.filter isSampleInside
                        if xs.Length > 0 then
                            let result = { Node = n; Selection = CellsSelected xs }
                            if config.Verbose then printfn "[Generic ] YIELD %A" result
                            yield result
                else
                    // at inner node with children to recursively traverse
                    if config.Verbose then printfn "[Generic ] at inner node with children to recursively traverse"

                    invariant n.SubNodes.IsSome "0baa1ede-dde1-489b-ba3c-28a7e7a5bd3a"

                    // return samples from inner node, which are not covered by children
                    let subWindows = n.SubNodes.Value  |> Array.map QNode.tryGetInMemory |> Array.choose (Option.map (fun x -> x.SampleWindow))
                    let inline subWindowContainsSample (sample : Cell2d) (subWindow : Box2l) =
                        let sampleWindow = Box2l.FromMinAndSize(sample.XY * 2L, V2l(2, 2))
                        subWindow.Intersects sampleWindow
                    let inline notContainedBySubWindows (s : Cell2d) = subWindows |> Array.exists (subWindowContainsSample s) |> not
                    
                    // get all samples _inside_ the query
                    let allSamples = n.GetAllSamples() |> Array.filter isSampleInside

                    // partition by whether samples are
                    // - intersecting subnode-samples (-> unsafe), or 
                    // - not intersecting subnode-samples (safe)
                    let (safeSamples, unsafeSamples) = allSamples |> Array.partition notContainedBySubWindows
                    
                    // safe samples can simply be returned ...
                    if safeSamples.Length > 0 then
                        let result = { Node = n; Selection = CellsSelected safeSamples }
                        if config.Verbose then printfn "[Generic ] YIELD %A" result
                        yield result

                    // an unsafe sample may only be partially covered by subnode samples
                    // -> in this case we cannot simply return the subnode samples, because
                    //    the not-covered part of the sample would appear as a HOLE, i.e.
                    //    not be covered by any samples at all (although the discarded sample
                    //    would actually cover the
                    if unsafeSamples.Length > 0 then
                        
                        let notCoveredBySubSamples (s : Cell2d) =
                            let isNotCovered = subWindows |> Array.exists (fun sw -> s.X < sw.Min.X || s.Y < sw.Min.Y || s.X >= sw.Max.X || s.Y >= sw.Max.Y)
                            isNotCovered

                        //let foo1 = unsafeSamples |> Seq.collect (fun x -> x.Children) |> Seq.toArray
                        //let foo2 = foo1 |> Array.filter notCoveredBySubSamples

                        // fill holes ...
                        let holeFillingSamples = 
                            unsafeSamples
                        // ... by splitting each unsafe sample into 4 subsamples, ...
                            |> Seq.collect (fun x -> x.Children)
                        // ... and keeping those not already covered by subnode samples
                            |> Seq.filter notCoveredBySubSamples
                            |> Seq.toArray

                        let result = { Node = n; Selection = SubCellsSelected holeFillingSamples }
                        if config.Verbose then printfn "[Generic ] YIELD %A" result
                        yield result

                    // recursively return samples from children (a.k.a. subnode samples)
                    match n.SubNodes with
                    | None -> failwith "Invariant 4f33151d-e387-40a1-a1b7-c04e2335bd91."
                    | Some subnodes ->
                        for subnode in subnodes |> Seq.choose QNode.tryGetInMemory do
                            let r = Generic config isNodeFullyInside isNodeFullyOutside isSampleInside subnode
                            yield! r
        }
    
    /// Returns all samples inside given cell.
    let All (config : Config) (root : QNodeRef) : Result seq =
        match root.TryGetInMemory() with
        | None -> Seq.empty
        | Some root -> Generic config (fun _ -> true) (fun _ -> false) (fun _ -> true) root

    /// Returns all samples inside given cell.
    let InsideCell (config : Config) (filter : Cell2d) (root : QNodeRef) : Result seq =
        let filterBb = filter.BoundingBox
        let isNodeFullyInside (n : QNode) = filterBb.Contains n.SampleWindowBoundingBox
        let isNodeFullyOutside (n : QNode) = not (filterBb.Intersects n.SampleWindowBoundingBox)
        let isSampleInside (n : Cell2d) = filter.Contains n
        match root.TryGetInMemory() with
        | None -> Seq.empty
        | Some root -> Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root

    /// Returns all samples intersecting given cell.
    let IntersectsCell (config : Config) (filter : Cell2d) (root : QNodeRef) : Result seq =
        let filterBb = filter.BoundingBox
        let isNodeFullyInside (n : QNode) = filterBb.Contains n.SampleWindowBoundingBox
        let isNodeFullyOutside (n : QNode) = not (filterBb.Intersects n.SampleWindowBoundingBox)
        let isSampleInside (n : Cell2d) = filterBb.Intersects n.BoundingBox
        match root.TryGetInMemory() with
        | None -> Seq.empty
        | Some root -> Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root

    /// Returns all samples inside given box.
    let InsideBox (config : Config) (filter : Box2d) (root : QNodeRef) : Result seq =
        let isNodeFullyInside (n : QNode) = filter.Contains n.SampleWindowBoundingBox
        let isNodeFullyOutside (n : QNode) = not (filter.Intersects n.SampleWindowBoundingBox)
        let isSampleInside (n : Cell2d) = filter.Contains n.BoundingBox
        match root.TryGetInMemory() with
        | None -> Seq.empty
        | Some root -> Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root

    /// Returns all samples inside given polygon.
    let InsidePolygon (config : Config) (filter : Polygon2d) (root : QNodeRef) : Result seq =
        let filter = 
            let p = filter.WithoutMultiplePoints()
            if p.IsCcw() then p else p.Reversed
        let rpos = V2d(config.SampleMode.RelativePosition)
        let isNodeFullyInside (n : QNode) =
            n.SampleWindowBoundingBox.ToPolygon2dCCW().IsFullyContainedInside(filter)
        let isNodeFullyOutside (n : QNode) =
            not (filter.Intersects(n.SampleWindowBoundingBox.ToPolygon2dCCW()))
        let isSampleInside (n : Cell2d) =
            let bb = n.BoundingBox
            let p = V2d(bb.Min.X + bb.SizeX * rpos.X, bb.Min.Y + bb.SizeY * rpos.Y)
            filter.Contains(p)
        match root.TryGetInMemory() with
        | None -> Seq.empty
        | Some root -> Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root

    /// Returns all samples within a given distance of a line.
    let NearLine (config : Config) (filter : Ray2d) (withinDistance : float) (root : QNodeRef) : Result seq =
        let filter = Ray2d(filter.Origin, filter.Direction.Normalized)
        let rpos = V2d(config.SampleMode.RelativePosition)
        let isNodeFullyInside (n : QNode) =
            let wbb = n.SampleWindowBoundingBox
            let halfDiagonal = wbb.Size.Length * 0.5
            let centerDist = filter.GetDistanceToRay(wbb.Center)
            centerDist + halfDiagonal <= withinDistance
        let isNodeFullyOutside (n : QNode) =
            let wbb = n.SampleWindowBoundingBox
            let halfDiagonal = wbb.Size.Length * 0.5
            let centerDist = filter.GetDistanceToRay(wbb.Center)
            centerDist >= withinDistance + halfDiagonal
        let isSampleInside (n : Cell2d) =
            let bb = n.BoundingBox
            let p = V2d(bb.Min.X + bb.SizeX * rpos.X, bb.Min.Y + bb.SizeY * rpos.Y)
            let dist = filter.GetDistanceToRay(p)
            dist <= withinDistance
        match root.TryGetInMemory() with
        | None -> Seq.empty
        | Some root -> Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root


    /// The generic query function.
    let rec Generic' 
            (config : Config) 
            (isNodeFullyInside : QNode -> bool) 
            (isNodeFullyOutside : QNode -> bool) 
            (getSamplesInside : QNode -> Result option)
            (n : QNode) 
            : Result seq =

        if config.Verbose then printfn "\n[Generic'] %A %A" n.Cell n.SampleWindow

        seq {

            invariantm (n.Cell.Exponent >= config.MinExponent)
                "Query cannot start at node with exponent smaller than configured minExponent."
                "2c82bfd9-e7ff-4f8f-a990-156f0460a255."

            if isNodeFullyOutside n then
                if config.Verbose then printfn "[Generic'] isFullyOutside"
                ()
            else
                if n.IsLeafNode || n.Cell.Exponent = config.MinExponent then
                    // reached leaf or max depth
                    if config.Verbose then printfn "[Generic'] reached leaf or max depth"
                    if isNodeFullyInside n then
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
                else
                    // at inner node with children to recursively traverse
                    if config.Verbose then printfn "[Generic'] at inner node with children to recursively traverse"

                    invariant n.SubNodes.IsSome "d6801ee4-022f-4a05-a5c1-6ca4dbe24159"

                    // return samples from inner node, which are not covered by children
                    
                    let nodeSampleWindow = n.SampleWindow
                    for sn in n.SubNodes.Value do
                        match QNode.tryGetInMemory sn with
                        | None -> () // no subnode
                        | Some sn ->
                            let quadrantBounds = sn.Cell.GetBoundsForExponent(n.SampleExponent)
                            match nodeSampleWindow.TryIntersect(quadrantBounds) with
                            | None -> () // subnode quadrant contains no samples
                            | Some nodeSampleWindowQuadrant ->
                                let subWindow = Box2l(sn.SampleWindow.Min / 2L, sn.SampleWindow.Max / 2L)
                                if subWindow.ContainsMaxExclusive(nodeSampleWindowQuadrant) then
                                    // subnode quadrant samples are FULLY covered by child samples
                                    // -> return nothing
                                    () 
                                else
                                    // subnode quadrant samples are PARTIALLY covered by child samples
                                    // -> return subtraction
                                    let rs = Generic' config isNodeFullyInside isNodeFullyOutside getSamplesInside sn
                                    yield! rs
        }

    
    /// Returns all samples intersecting given cell.
    let IntersectsCell' (config : Config) (filter : Cell2d) (root : QNodeRef) : Result seq =
        let filterBb = filter.BoundingBox
        let isNodeFullyInside (n : QNode) = filterBb.Contains n.SampleWindowBoundingBox
        let isNodeFullyOutside (n : QNode) = not (filterBb.Intersects n.SampleWindowBoundingBox)
        let getSamplesInside (n : QNode) =
            let overlap = filter.GetBoundsForExponent(n.SampleExponent).Intersection(n.SampleWindow)
            if overlap.SizeX > 0L && overlap.SizeY > 0L then
                Some { Node = n; Selection = WindowSelected overlap }
            else
                None

        match root.TryGetInMemory() with
        | None -> Seq.empty
        | Some root -> Generic' config isNodeFullyInside isNodeFullyOutside getSamplesInside root



module Sample =
    
    type SampleResult = {
        Node : QNode
        Cells : Cell2d[]
        Positions : V2d[]
    }
    with
        member this.GetSampleCells () : Cell2d[] = this.Cells
        member this.GetSamples<'a>(def : Durable.Def) : (V2d*Cell2d*'a)[] =
            let layer = this.Node.GetLayer<'a>(def)
            this.Cells |> Array.map2 (fun p c -> (p, c, layer.GetSample(Fail, c))) this.Positions

    let rec private PositionsWithBounds (config : Query.Config) (positions : V2d[]) (positionsBounds : Box2d) (n : QNode) : SampleResult seq =

        seq {

            invariantm (n.Cell.Exponent >= config.MinExponent)
                "Query cannot start at node with exponent smaller than configured minExponent."
                "c7c3713d-2c9d-4924-a9dd-a2e15dd0a2a8."

            invariantm (n.Cell.BoundingBox.Contains(positionsBounds))
                "Positions bounds not contained in node."
                "0f367a54-cafb-405d-aad4-cf5ec36216f6"

            let swbb = n.SampleWindowBoundingBox

            if not (swbb.Contains positionsBounds) then
                ()
            else
                if n.IsLeafNode || n.Cell.Exponent = config.MinExponent then
                // reached leaf or max depth
                    if swbb.ContainsMaxExclusive positionsBounds then
                        // fully inside
                        let cells = positions |> Array.map n.GetSample
                        yield { Node = n; Cells = cells; Positions = positions }
                    else
                        // partially inside (some positions have no samples)
                        let ps = positions |> Array.filter swbb.ContainsMaxExclusive
                        let cells = ps |> Array.map n.GetSample
                        yield { Node = n; Cells = cells; Positions = ps }
                else
                // at inner node with children to recursively traverse

                    invariant n.SubNodes.IsSome "d8bf936c-7ff5-48b0-ac8a-d714f1e3af4c"

                    // split positions in two sets, by whether a position is covered by subnode samples (or not)
                    let subSwbbs = n.SubNodes.Value  |> Array.map QNode.tryGetInMemory |> Array.choose (Option.map (fun x -> x.SampleWindowBoundingBox))
                    let inline coveredByInnerNodeSamples (p : V2d) = swbb.ContainsMaxExclusive p
                    let inline coveredBySubNodeSamples (p : V2d) = subSwbbs |> Array.exists (fun bb -> bb.ContainsMaxExclusive(p))
                    let mutable positionsCoveredBySubNodeSamples = Array.empty
                    let mutable positionsNotCoveredBySubNodeSamples = Array.empty
                    for (isCovered, ps) in positions |> Array.groupBy coveredBySubNodeSamples do
                        if isCovered then positionsCoveredBySubNodeSamples <- ps else positionsNotCoveredBySubNodeSamples <- ps

                    // (optionally) return samples from inner node, which are not covered by children
                    if positionsNotCoveredBySubNodeSamples.Length > 0 then
                        let ps = positionsNotCoveredBySubNodeSamples |> Array.filter coveredByInnerNodeSamples
                        if ps.Length > 0 then
                            let cells = ps |> Array.map n.GetSample
                            yield { Node = n; Cells = cells; Positions = ps }

                    // recursively return samples from children
                    match n.SubNodes with
                    | None -> failwith "Invariant 853246b8-9754-4a99-b4ca-a3a94e4f7ebe."
                    | Some subnodes ->
                        let center = n.Cell.GetCenter()
                        let inline getQuadrant (p : V2d) = 
                            match p.X >= center.X, p.Y >= center.Y with | false, false -> 0 | true,  false -> 1 | false, true  -> 2 | true,  true  -> 3
                        let spss = positionsCoveredBySubNodeSamples |> Array.groupBy getQuadrant
                        for (quadrant, ps) in spss do
                            match subnodes.[quadrant] |> QNode.tryGetInMemory with
                            | None -> failwith "Invariant b431402f-9db7-4b74-b3bd-ca8655d96f58."
                            | Some subnode ->
                                let bb = Box2d(ps)
                                let r = PositionsWithBounds config ps bb subnode
                                yield! r
                            
        }

    /// Returns samples at given positions.
    /// If there is no sample at a given position, it will not be included in the result sequence.
    /// This means, the result sequence may be empty.
    let Positions (config : Query.Config) (positions : V2d[]) (root : QNodeRef) : SampleResult seq=
        let bb = Box2d(positions)
        match root.TryGetInMemory() with
        | None -> Seq.empty
        | Some root ->
            if root.Cell.BoundingBox.Contains(bb) then
                PositionsWithBounds config positions bb root
            else
                Seq.empty

    /// Returns sample at given position, or None if there is no sample.
    let Position (config : Query.Config) (position : V2d) (root : QNodeRef) : SampleResult option =
        Positions config [| position |] root |> Seq.tryHead

    let PositionTyped<'a>  (config : Query.Config) (position : V2d) (def : Durable.Def) (root : QNodeRef) : 'a option =
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