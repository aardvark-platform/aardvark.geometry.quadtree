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
        
        member this.GetSamples<'a>(def : Durable.Def) : (Cell2d*'a)[] =
            let layer = this.Node.GetLayer<'a>(def)
            let ps = 
                match this.Selection with
                | NotSelected -> Array.empty
                | PartiallySelected ps -> ps
                | FullySelected -> this.Node.AllSamples
            let result = ps |> Array.map (fun p -> (p, layer.GetSample Fail p))
            result

        member this.GetSampleCells () : Cell2d[] =
            match this.Selection with
            | NotSelected -> Array.empty
            | PartiallySelected ps -> ps
            | FullySelected -> this.Node.AllSamples

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
                    let w = n.SampleWindow
                    let v = Box2l(w.Min * 2L, w.Max * 2L)

                    let subWindows = n.SubNodes.Value  |> Array.map QNode.TryGetInMemory |> Array.choose (Option.map (fun x -> x.SampleWindow))
                    let inline subWindowContainsSample (sample : Cell2d) (subWindow : Box2l) =
                        let sampleWindow = Box2l.FromMinAndSize(sample.XY * 2L, V2l(2, 2))
                        subWindow.Intersects sampleWindow

                    let xs = 
                        n.AllSamples
                        |> Array.filter (fun sample -> not (Array.exists (subWindowContainsSample sample) subWindows) )
                        |> Array.filter isSampleInside
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

    ()

