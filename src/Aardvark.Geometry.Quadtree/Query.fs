namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System
open System.Collections.Generic

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
        member this.GetSamples<'a>(def : Durable.Def) : (Cell2d*'a)[] =
            let layer = this.Node.GetLayer<'a>(def)
            let cells = this.GetSampleCells ()
            let result = cells |> Array.map (fun p ->
                if p.Exponent = layer.Mapping.BufferOrigin.Exponent then
                    (p, layer.GetSample(Fail, p))
                else
                    (p, layer.GetSample(Fail, p.Parent))
                )
            result

    /// xs dominate always
    let private mergeDominating (config : Config) (xs : Result seq) (ys : Result seq) : Result seq =

        let xs = xs |> Seq.toArray
        let getInterferingXs (y : Result) = 
            xs |> Array.filter(fun x -> x.Node.ExactBoundingBox.Intersects(y.Node.ExactBoundingBox))

        seq {
            // return ALL dominating samples ...
            if config.Verbose then printfn "[mergeDominating] YIELD all dominating samples: %A" (xs |> Array.collect(fun z -> z.GetSampleCells()))
            yield! xs

            // resolve dominated results ...
            for y in ys do
                let yebb = y.Node.ExactBoundingBox
                let zs = getInterferingXs y
                if zs.Length > 0 then

                    // dominated result IS interfered with -> resolve remaining (virtual) sample cells
                    let mutable result = List.empty

                    // all dominating sample cells intersecting dominated result
                    let zcs = zs |> Array.collect(fun z -> z.GetSampleCells()) |> Array.filter(fun zc -> zc.BoundingBox.Intersects(yebb))

                    // all sample cells of dominated result
                    let ycs = y.GetSampleCells ()
                    for yc in ycs do
                        let isDominatedCellYcFullyCovered   (s : Cell2d) = zcs |> Array.exists(fun zc -> zc.BoundingBox.Contains(s.BoundingBox))
                        let isDominatedCellYcInterferedWith (s : Cell2d) = zcs |> Array.exists(fun zc -> s.BoundingBox.Intersects(zc.BoundingBox))

                        // -> resolve by recursively splitting sample into fragments
                        let rec resolve (s : Cell2d) =
                            if isDominatedCellYcInterferedWith s then
                                if isDominatedCellYcFullyCovered s then
                                    () // discard sample, it is completely covered by dominating sample
                                else
                                    // partially covered, aaargh
                                    for q in s.Children do resolve q
                            else
                                result <- s :: result

                        resolve yc

                    if result.Length > 0 then
                        if config.Verbose then printfn "[mergeDominating] YIELD resolved samples: %A" result
                        yield { Node = y.Node; Selection = result |> Array.ofList |> SubCellsSelected }

                            
                else
                    // dominated sample IS NOT interfered with -> return
                    if config.Verbose then printfn "[mergeDominating] YIELD non-dominating sample: %A" (y.GetSampleCells())
                    yield y
        }

    let private merge (config : Config) (dom : Dominance) (xs : Result seq) (ys : Result seq) : Result seq =
        
        match dom with
        | FirstDominates
        | MoreDetailedOrFirst  -> mergeDominating config xs ys
        | SecondDominates
        | MoreDetailedOrSecond -> mergeDominating config ys xs


    /// The generic query function.
    let rec Generic 
            (config : Config) 
            (isNodeFullyInside : QNodeRef -> bool) 
            (isNodeFullyOutside : QNodeRef -> bool) 
            (isSampleInside : Cell2d -> bool)
            (root : QNodeRef) 
            : Result seq =

        let recurse = Generic config isNodeFullyInside isNodeFullyOutside isSampleInside

        seq {

            match root with
            | NoNode                    -> 
                if config.Verbose then printfn "[Generic ] NoNode"
                ()
            
            | OutOfCoreNode (id, load)   ->
                if config.Verbose then printfn "[Generic ] OutOfCoreNode %A" id
                yield! load() |> recurse
            
            | InMemoryInner n           ->
                if config.Verbose then printfn "[Generic ] InMemoryInner %A %A" n.Cell n.Id
                for subnode in n.SubNodes do
                    if config.Verbose then printfn "[Generic ]     subnode %A ->" n.Id
                    yield! subnode |> recurse
            
            | InMemoryMerge n           ->
                if config.Verbose then printfn "[Generic ] InMemoryMerge %A" n.Cell
                if n.First.ExactBoundingBox.Intersects(n.Second.ExactBoundingBox) then
                    let xs = (n.First |> recurse)
                    let ys = (n.Second |> recurse)
                    if config.Verbose then printfn "[Generic ]     merge first + second  %A" n.Cell
                    yield! merge config n.Dominance xs ys
                else
                    // first and second do not interfere, so simply return all samples of both
                    if config.Verbose then printfn "[Generic ]     yield first  %A" n.Cell
                    yield! n.First  |> recurse
                    if config.Verbose then printfn "[Generic ]     yield second %A" n.Cell
                    yield! n.Second |> recurse

            | InMemoryNode n            ->

                if config.Verbose then printfn "[Generic ] InMemoryNode %A" n.Cell

                invariantm (root.Cell.Exponent >= config.MinExponent)
                    "Query cannot start at node with exponent smaller than configured minExponent."
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

    /// Returns all samples intersecting given cell.
    let IntersectsCell (config : Config) (filter : Cell2d) (root : QNodeRef) : Result seq =
        let filterBb = filter.BoundingBox
        let isNodeFullyInside (n : QNodeRef) = filterBb.Contains n.ExactBoundingBox
        let isNodeFullyOutside (n : QNodeRef) = not (filterBb.Intersects n.ExactBoundingBox)
        let isSampleInside (n : Cell2d) = filterBb.Intersects n.BoundingBox
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
            filter.Contains(p)
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
            (getSamplesInside : QNodeRef -> Result option)
            (root : QNodeRef) 
            : Result seq =

        let recurse = Generic' config isNodeFullyInside isNodeFullyOutside getSamplesInside

        seq {

            match root with
            | NoNode                    -> ()
            | OutOfCoreNode (_, load)   -> yield! load() |> recurse
            | InMemoryInner n           -> for subnode in n.SubNodes do yield! subnode |> recurse
            | InMemoryMerge n           ->

                if config.Verbose then printfn "[Generic'] InMemoryMerge %A" n.Cell
                if n.First.ExactBoundingBox.Intersects(n.Second.ExactBoundingBox) then
                    let xs = (n.First |> recurse)
                    let ys = (n.Second |> recurse)
                    if config.Verbose then printfn "[Generic']     merge first + second  %A" n.Cell
                    yield! merge config n.Dominance xs ys
                else
                    // first and second do not interfere, so simply return all samples of both
                    if config.Verbose then printfn "[Generic']     yield first  %A" n.Cell
                    yield! n.First  |> recurse
                    if config.Verbose then printfn "[Generic']     yield second %A" n.Cell
                    yield! n.Second |> recurse

            | InMemoryNode n            ->

                invariantm (n.Cell.Exponent >= config.MinExponent)
                    "Query cannot start at node with exponent smaller than configured minExponent."
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
                            match getSamplesInside root with
                            | None -> ()
                            | Some result -> 
                                if config.Verbose then printfn "[Generic'] YIELD %A" result
                                yield result
        }

    
    /// Returns all samples intersecting given cell.
    let IntersectsCell' (config : Config) (filter : Cell2d) (root : QNodeRef) : Result seq =
        let filterBb = filter.BoundingBox
        let isNodeFullyInside  (n : QNodeRef) = filterBb.Contains n.ExactBoundingBox
        let isNodeFullyOutside (n : QNodeRef) = not (filterBb.Intersects n.ExactBoundingBox)
        let getSamplesInside   (n : QNodeRef) =
            match n with
            | InMemoryNode n ->
                let overlap = filter.GetBoundsForExponent(n.LayerSet.SampleExponent).Intersection(n.LayerSet.SampleWindow)
                if overlap.SizeX > 0L && overlap.SizeY > 0L then
                    Some { Node = n; Selection = WindowSelected overlap }
                else
                    None
            | _ -> failwith "todo"

        Generic' config isNodeFullyInside isNodeFullyOutside getSamplesInside root

    /// Enumerates all samples in quadtree, including samples from inner cells.
    let rec Full (root : QNodeRef) : Result seq = seq {
        match root with
        | NoNode -> ()
        | InMemoryInner n -> for subnode in n.SubNodes do yield! Full subnode
        | InMemoryNode  n -> yield { Node = n; Selection = FullySelected }
        | InMemoryMerge n -> yield! Full n.First
                             yield! Full n.Second
        | _ -> failwith "todo"
    }

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
                                    let (found, (n0, c0)) = result.TryGetValue(p)
                                    if found then
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

                | OutOfCoreNode (_,load) ->
                    
                        let n = load()
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