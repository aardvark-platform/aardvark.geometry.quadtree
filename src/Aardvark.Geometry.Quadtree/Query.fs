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
        | PartiallySelected of Cell2d[]
        | NotSelected

    type Result = {
        Node : QNode
        Selection : NodeSelection
    }
    with
        member this.GetSampleCells () : Cell2d[] =
            match this.Selection with
            | NotSelected -> Array.empty
            | PartiallySelected cells -> cells
            | FullySelected -> this.Node.AllSamples
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
        seq {

            invariantm (n.Cell.Exponent >= config.MinExponent)
                "Query cannot start at node with exponent smaller than configured minExponent."
                "bd20d469-970c-4c9c-b99c-6694dc90923d."

            if isNodeFullyOutside n then
                ()
            else
                if n.IsLeafNode || n.Cell.Exponent = config.MinExponent then
                // reached leaf or max depth
                    if isNodeFullyInside n then
                        // fully inside
                        yield { Node = n; Selection = FullySelected }
                    else
                        // partially inside
                        let xs = n.AllSamples |> Array.filter isSampleInside
                        if xs.Length > 0 then
                            yield { Node = n; Selection = PartiallySelected xs }
                else
                // at inner node with children to recursively traverse

                    invariant n.SubNodes.IsSome "0baa1ede-dde1-489b-ba3c-28a7e7a5bd3a"

                    // return samples from inner node, which are not covered by children
                    let subWindows = n.SubNodes.Value  |> Array.map QNode.TryGetInMemory |> Array.choose (Option.map (fun x -> x.SampleWindow))
                    let inline subWindowContainsSample (sample : Cell2d) (subWindow : Box2l) =
                        let sampleWindow = Box2l.FromMinAndSize(sample.XY * 2L, V2l(2, 2))
                        subWindow.Intersects sampleWindow
                    let inline notContainedBySubWindows (s : Cell2d) = subWindows |> Array.exists (subWindowContainsSample s) |> not
                    let xs = n.AllSamples |> Array.filter (fun s -> isSampleInside s && notContainedBySubWindows s)
                    if xs.Length > 0 then
                        yield { Node = n; Selection = PartiallySelected xs }

                    // recursively return samples from children
                    match n.SubNodes with
                    | None -> failwith "Invariant 4f33151d-e387-40a1-a1b7-c04e2335bd91."
                    | Some subnodes ->
                        for subnode in subnodes |> Seq.choose QNode.TryGetInMemory do
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

    /// Returns sample at given position.
    let Position (config : Config) (position : V2d) (root : QNodeRef) : Result seq =
        let isNodeFullyInside _ =  false
        let isNodeFullyOutside (n : QNode) =
            let bb = n.SampleWindowBoundingBox
            position.X <  bb.Min.X || position.Y <  bb.Min.Y || position.X >= bb.Max.X || position.Y >= bb.Max.Y
        let isSampleInside (n : Cell2d) = 
            let bb = n.BoundingBox
            position.X >= bb.Min.X && position.Y >= bb.Min.Y && position.X <  bb.Max.X && position.Y <  bb.Max.Y
        match root.TryGetInMemory() with
        | None -> Seq.empty
        | Some root -> Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root

    /// Returns sample cell at given position, or None if there is no sample at this position.
    let TryGetSampleCellAtPosition (config : Config) (position : V2d) (root : QNodeRef) : Cell2d option =
        Position config position root
        |> Seq.collect (fun r -> r.GetSampleCells ())
        |> Seq.tryHead



    
    
    type SampleResult = {
        Node : QNode
        Cells : Cell2d[]
        Positions : V2d[]
    }
    with
        member this.GetSampleCells () : Cell2d[] = this.Cells
        member this.GetSamples<'a>(def : Durable.Def) : (Cell2d*'a)[] =
            let layer = this.Node.GetLayer<'a>(def)
            this.Cells |> Array.map (fun c -> (c, layer.GetSample(Fail, c)))

    let rec private SamplePositionsWithBounds (config : Config) (positions : V2d[]) (positionsBounds : Box2d) (n : QNode) : seq<SampleResult> =

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
                    let subSwbbs = n.SubNodes.Value  |> Array.map QNode.TryGetInMemory |> Array.choose (Option.map (fun x -> x.SampleWindowBoundingBox))
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
                            match subnodes.[quadrant] |> QNode.TryGetInMemory with
                            | None -> failwith "Invariant b431402f-9db7-4b74-b3bd-ca8655d96f58."
                            | Some subnode ->
                                let bb = Box2d(ps)
                                let r = SamplePositionsWithBounds config ps bb subnode
                                yield! r
                            
        }

    /// Returns samples at given positions.
    let rec SamplePositions (config : Config) (positions : V2d[]) (root : QNodeRef) =
        let bb = Box2d(positions)
        match root.TryGetInMemory() with
        | None -> Seq.empty
        | Some root -> SamplePositionsWithBounds config positions bb root


