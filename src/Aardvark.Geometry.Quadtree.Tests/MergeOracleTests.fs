namespace Aardvark.Geometry.Quadtree.Tests

open Aardvark.Base
open Aardvark.Geometry.Quadtree
open System.Collections.Generic
open Xunit

/// Merge results are checked against values computed directly from the inputs,
/// independent of tree structure.
module MergeOracleTests =

    /// Samples inside Hole [min, max) are masked out (undefined).
    type private Input = { Id : int; Ox : int; Oy : int; W : int; H : int; E : int; Split : int; Hole : Box2l }

    let private input id ox oy w h e split = { Id = id; Ox = ox; Oy = oy; W = w; H = h; E = e; Split = split; Hole = Box2l.Invalid }

    // aligned = window max lies on the boundary of the smallest enclosing cell
    let private inputs =
        dict [
            "aligned00",     input  0  0  0  4  4  0 2
            "aligned00_8",   input  1  0  0  8  8  0 2
            "aligned80",     input  2  8  0  8  8  0 2
            "aligned88",     input  3  8  8  8  8  0 2
            "alignedFine",   input  4  8  8  8  8 -1 2
            "alignedCoarse", input  5  0  0  4  4  1 2
            "centered4",     input  6 -2 -2  4  4  0 2
            "centered2",     input  7 -1 -1  2  2  0 0
            "centeredFine",  input  8 -8 -8 16 16 -1 3
            "unaligned",     input  9  0  0 10  7  0 2
            "unalignedNeg",  input 10 -5 -3 10  7  0 2
            "coarseSplit0",  input 11  0  0  4  4  1 0
            "fineLeaf",      input 12  2  2  4  4  0 8
            "holed00_8",     { input 13  0  0  8  8  0 2 with Hole = Box2l(2L, 2L, 6L, 6L) }     // hole in all 4 leaves
            "holedFine",     { input 14 -8 -8 16 16 -1 3 with Hole = Box2l(-4L, -4L, 4L, 4L) }   // hole around origin
        ]

    let private value (i : Input) (x : int64) (y : int64) = float (i.Id + 1) * 1e6 + float y * 1e3 + float x

    let private isDefined (i : Input) (x : int64) (y : int64) =
        x >= int64 i.Ox && x < int64 (i.Ox + i.W) && y >= int64 i.Oy && y < int64 (i.Oy + i.H) &&
        not (x >= i.Hole.Min.X && x < i.Hole.Max.X && y >= i.Hole.Min.Y && y < i.Hole.Max.Y)

    let private build (i : Input) =
        let xy k = (int64 (i.Ox + k % i.W), int64 (i.Oy + k / i.W))
        let xs = Array.init (i.W * i.H) (fun k -> xy k ||> value i)
        let mask = if i.Hole.IsValid then Some (Array.init (i.W * i.H) (fun k -> if xy k ||> isDefined i then 1uy else 0uy)) else None
        let layer = Layer(Defs.Heights1d, xs, DataMapping(V2l(i.Ox, i.Oy), V2i(i.W, i.H), exponent = i.E), mask)
        Quadtree.Build { BuildConfig.Default with SplitLimitPowerOfTwo = i.Split } [| layer |]

    /// Exponent and value of the sample of i covering p.
    let private sampleAt (i : Input) (p : V2d) =
        let s = 2.0 ** float i.E
        let x = int64 (floor (p.X / s))
        let y = int64 (floor (p.Y / s))
        if isDefined i x y then Some (i.E, value i x y) else None

    let private winner (dom : Dominance) (a : (int * float) option) (b : (int * float) option) =
        match a, b with
        | None, _ -> b
        | _, None -> a
        | Some (ea, _), Some (eb, _) ->
            match dom with
            | FirstDominates       -> a
            | SecondDominates      -> b
            | MoreDetailedOrFirst  -> if eb < ea then b else a
            | MoreDetailedOrSecond -> if ea < eb then a else b

    /// Expected value at p for inputs merged from left to right.
    let private expected (dom : Dominance) (xs : Input list) (p : V2d) =
        xs |> List.map (fun i -> sampleAt i p) |> List.reduce (winner dom) |> Option.map snd

    let private dominances =
        dict [
            "FirstDominates", FirstDominates; "SecondDominates", SecondDominates
            "MoreDetailedOrFirst", MoreDetailedOrFirst; "MoreDetailedOrSecond", MoreDetailedOrSecond
        ]

    /// Inputs separated by '+' are merged from left to right.
    let cases : obj[] seq = seq {
        let merges = [
            "aligned00+aligned00_8"             // overlapping, same resolution
            "aligned00_8+aligned80"             // adjacent
            "aligned00+aligned88"               // disjoint
            "aligned88+alignedFine"             // overlapping, finer second
            "aligned00_8+alignedCoarse"         // overlapping, coarser second
            "centered4+aligned00"               // straddling origin vs touching origin
            "centeredFine+aligned00_8"          // straddling origin fine vs touching origin
            "centered2+centered4"               // both straddling origin
            "unaligned+aligned00_8"             // unaligned vs aligned
            "unalignedNeg+centered4"            // both straddling origin, unaligned
            "alignedCoarse+centeredFine"        // coarse vs fine straddling origin
            "coarseSplit0+fineLeaf"             // coarse inner tree vs fine leaf inside it
            "fineLeaf+coarseSplit0"             // fine leaf vs coarse inner tree around it
            "holed00_8+aligned00"               // hole in first, second has data there
            "aligned00+holed00_8"               // hole in second
            "holedFine+aligned00_8"             // hole around origin vs touching origin
            "alignedCoarse+holed00_8"           // coarse vs fine with hole
            "holed00_8+holedFine"               // both with holes
            "aligned00+aligned88+fineLeaf"      // third inside the gap of the first two
            "aligned00+aligned88+unaligned"     // third overlaps the gap and both
            "centered4+aligned88+holed00_8"     // chain with hole in third
        ]
        for m in merges do
            for d in dominances.Keys do
                for outOfCore in [ false; true ] do
                    yield [| box m; box d; box outOfCore |]
        }

    /// Saves and reloads the tree, which turns subnodes into lazily loaded out-of-core nodes.
    let private reload (q : QNodeRef) =
        let options = Serialization.SerializationOptions.NewInMemoryStore()
        Quadtree.Load options (Quadtree.Save options q)

    [<Theory; MemberData(nameof cases)>]
    let ``Merge_MatchesOracle`` (merge : string, dom : string, outOfCore : bool) =
        let xs = merge.Split('+') |> Array.map (fun name -> inputs.[name]) |> List.ofArray
        let dom = dominances.[dom]
        let m = xs |> List.map (fun x -> if outOfCore then build x |> reload else build x) |> List.reduce (Quadtree.Merge dom)

        // grid of points strictly inside cells of size >= 2^-2
        let step = 0.25
        let bb = m.ExactBoundingBox
        let gx = floor bb.Min.X - 1.0
        let gy = floor bb.Min.Y - 1.0
        let nx = int ((ceil bb.Max.X + 1.0 - gx) / step)
        let ny = int ((ceil bb.Max.Y + 1.0 - gy) / step)
        let point i j = V2d(gx + (float i + 0.5) * step, gy + (float j + 0.5) * step)

        let errors = List<string>()

        // point sampling
        for j = 0 to ny - 1 do
            for i = 0 to nx - 1 do
                let p = point i j
                let actual = Sample.PositionTyped<float> Query.Config.Default p Defs.Heights1d m
                let e = expected dom xs p
                if actual <> e then errors.Add(sprintf "Sample.Position %A: %A, expected %A" p actual e)

        // Query.All: every point covered by exactly one returned cell carrying the expected value
        let covering = Array2D.create nx ny []
        for r in Query.All Query.Config.Default m do
            for (c, v) in r.GetSamples<float> Defs.Heights1d do
                let cb = c.BoundingBox
                for j = max 0 (int ((cb.Min.Y - gy) / step)) to min ny (int ((cb.Max.Y - gy) / step)) - 1 do
                    for i = max 0 (int ((cb.Min.X - gx) / step)) to min nx (int ((cb.Max.X - gx) / step)) - 1 do
                        covering.[i, j] <- (c, v) :: covering.[i, j]
        for j = 0 to ny - 1 do
            for i = 0 to nx - 1 do
                let p = point i j
                match expected dom xs p, covering.[i, j] with
                | None, [] -> ()
                | Some e, [ (_, v) ] when v = e -> ()
                | e, cs -> errors.Add(sprintf "Query.All %A: %A, expected %A" p cs e)

        Assert.True(errors.Count = 0, sprintf "%d errors, first: %A" errors.Count (errors |> Seq.truncate 5 |> List.ofSeq))
