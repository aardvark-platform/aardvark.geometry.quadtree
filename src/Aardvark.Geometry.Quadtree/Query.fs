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
        Node : INode
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
            ps |> Array.map (fun p -> (p, layer.GetSample Fail p))

    let rec Generic 
            (config : Config) 
            (isNodeFullyInside : INode -> bool) 
            (isNodeFullyOutside : INode -> bool) 
            (isSampleInside : Cell2d -> bool)
            (n : INode) 
            : Result seq =
        seq {
            if n.Cell.Exponent < config.MinExponent then
                failwith ("Query cannot start at node with exponent smaller than configured minExponent. " + 
                         "Invariant bd20d469-970c-4c9c-b99c-6694dc90923d.")

            if isNodeFullyOutside n then
                ()
            else
                if n.IsLeafNode || n.Cell.Exponent = config.MinExponent then
                    if isNodeFullyInside n then
                        // fully inside
                        yield { Node = n; Selection = FullySelected }
                    else
                        // partially inside
                        let xs = n.AllSamples |> Array.filter isSampleInside
                        if xs.Length > 0 then
                            yield { Node = n; Selection = PartiallySelected xs }
                else
                    match n.SubNodes with
                    | None -> failwith "Invariant 4f33151d-e387-40a1-a1b7-c04e2335bd91."
                    | Some sns ->
                        for sn in sns |> Seq.choose id do
                            let r = Generic config isNodeFullyInside isNodeFullyOutside isSampleInside sn
                            yield! r
        }
    
    /// Returns all samples inside given cell.
    let InsideCell (config : Config) (filter : Cell2d) (root : INode) : Result seq =
        let filterBb = filter.BoundingBox
        let isNodeFullyInside (n : INode) = filterBb.Contains n.SampleWindowBoundingBox
        let isNodeFullyOutside (n : INode) = not (filterBb.Intersects n.SampleWindowBoundingBox)
        let isSampleInside (n : Cell2d) = filter.Contains n
        Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root

    /// Returns all samples inside given box.
    let InsideBox (config : Config) (filter : Box2d) (root : INode) : Result seq =
        let isNodeFullyInside (n : INode) = filter.Contains n.SampleWindowBoundingBox
        let isNodeFullyOutside (n : INode) = not (filter.Intersects n.SampleWindowBoundingBox)
        let isSampleInside (n : Cell2d) = filter.Contains n.BoundingBox
        Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root

    /// Returns all samples inside given polygon.
    let InsidePolygon (config : Config) (filter : Polygon2d) (root : INode) : Result seq =
        let rpos = V2d(config.SampleMode.RelativePosition)
        let isNodeFullyInside (n : INode) =
            filter.IsFullyContainedInside(n.SampleWindowBoundingBox.ToPolygon2dCCW())
        let isNodeFullyOutside (n : INode) =
            not (filter.Intersects(n.SampleWindowBoundingBox.ToPolygon2dCCW()))
        let isSampleInside (n : Cell2d) =
            let bb = n.BoundingBox
            let p = V2d(bb.Min.X + bb.SizeX * rpos.X, bb.Min.Y + bb.SizeY * rpos.Y)
            filter.Contains(p)
        Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root

    /// Returns all samples within a given distance of a line.
    let NearLine (config : Config) (filter : Ray2d) (withinDistance : float) (root : INode) : Result seq =
        let filter = Ray2d(filter.Origin, filter.Direction.Normalized)
        let rpos = V2d(config.SampleMode.RelativePosition)
        let isNodeFullyInside (n : INode) =
            let wbb = n.SampleWindowBoundingBox
            let halfDiagonal = wbb.Size.Length * 0.5
            let centerDist = filter.GetDistanceToRay(wbb.Center)
            centerDist + halfDiagonal <= withinDistance
        let isNodeFullyOutside (n : INode) =
            let wbb = n.SampleWindowBoundingBox
            let halfDiagonal = wbb.Size.Length * 0.5
            let centerDist = filter.GetDistanceToRay(wbb.Center)
            centerDist >= withinDistance + halfDiagonal
        let isSampleInside (n : Cell2d) =
            let bb = n.BoundingBox
            let p = V2d(bb.Min.X + bb.SizeX * rpos.X, bb.Min.Y + bb.SizeY * rpos.Y)
            let dist = filter.GetDistanceToRay(p)
            dist <= withinDistance
        Generic config isNodeFullyInside isNodeFullyOutside isSampleInside root


    ()

