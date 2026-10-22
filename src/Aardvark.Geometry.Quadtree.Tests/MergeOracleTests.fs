namespace Aardvark.Geometry.Quadtree.Tests

open Aardvark.Base
open Aardvark.Geometry.Quadtree
open System.Collections.Generic
open Xunit

/// Merge results are checked against values computed directly from the inputs,
/// independent of tree structure.
module MergeOracleTests =

    type private Input = { Id : int; Ox : int; Oy : int; W : int; H : int; E : int; Split : int }

    // aligned = window max lies on the boundary of the smallest enclosing cell
    let private inputs =
        dict [
            "aligned00",     { Id =  0; Ox =  0; Oy =  0; W =  4; H =  4; E =  0; Split = 2 }
            "aligned00_8",   { Id =  1; Ox =  0; Oy =  0; W =  8; H =  8; E =  0; Split = 2 }
            "aligned80",     { Id =  2; Ox =  8; Oy =  0; W =  8; H =  8; E =  0; Split = 2 }
            "aligned88",     { Id =  3; Ox =  8; Oy =  8; W =  8; H =  8; E =  0; Split = 2 }
            "alignedFine",   { Id =  4; Ox =  8; Oy =  8; W =  8; H =  8; E = -1; Split = 2 }
            "alignedCoarse", { Id =  5; Ox =  0; Oy =  0; W =  4; H =  4; E =  1; Split = 2 }
            "centered4",     { Id =  6; Ox = -2; Oy = -2; W =  4; H =  4; E =  0; Split = 2 }
            "centered2",     { Id =  7; Ox = -1; Oy = -1; W =  2; H =  2; E =  0; Split = 0 }
            "centeredFine",  { Id =  8; Ox = -8; Oy = -8; W = 16; H = 16; E = -1; Split = 3 }
            "unaligned",     { Id =  9; Ox =  0; Oy =  0; W = 10; H =  7; E =  0; Split = 2 }
            "unalignedNeg",  { Id = 10; Ox = -5; Oy = -3; W = 10; H =  7; E =  0; Split = 2 }
            "coarseSplit0",  { Id = 11; Ox =  0; Oy =  0; W =  4; H =  4; E =  1; Split = 0 }
            "fineLeaf",      { Id = 12; Ox =  2; Oy =  2; W =  4; H =  4; E =  0; Split = 8 }
        ]

    let private value (i : Input) (x : int64) (y : int64) = float (i.Id + 1) * 1e6 + float y * 1e3 + float x

    let private build (i : Input) =
        let xs = Array.init (i.W * i.H) (fun k -> value i (int64 (i.Ox + k % i.W)) (int64 (i.Oy + k / i.W)))
        let layer = Layer(Defs.Heights1d, xs, DataMapping(V2l(i.Ox, i.Oy), V2i(i.W, i.H), exponent = i.E))
        Quadtree.Build { BuildConfig.Default with SplitLimitPowerOfTwo = i.Split } [| layer |]

    /// Exponent and value of the sample of i covering p.
    let private sampleAt (i : Input) (p : V2d) =
        let s = 2.0 ** float i.E
        let x = int64 (floor (p.X / s))
        let y = int64 (floor (p.Y / s))
        if x >= int64 i.Ox && x < int64 (i.Ox + i.W) && y >= int64 i.Oy && y < int64 (i.Oy + i.H) then Some (i.E, value i x y)
        else None

    let private expected (dom : Dominance) (a : Input) (b : Input) (p : V2d) =
        match sampleAt a p, sampleAt b p with
        | None, None -> None
        | Some (_, va), None -> Some va
        | None, Some (_, vb) -> Some vb
        | Some (ea, va), Some (eb, vb) ->
            match dom with
            | FirstDominates       -> Some va
            | SecondDominates      -> Some vb
            | MoreDetailedOrFirst  -> Some (if eb < ea then vb else va)
            | MoreDetailedOrSecond -> Some (if ea < eb then va else vb)

    let private dominances =
        dict [
            "FirstDominates", FirstDominates; "SecondDominates", SecondDominates
            "MoreDetailedOrFirst", MoreDetailedOrFirst; "MoreDetailedOrSecond", MoreDetailedOrSecond
        ]

    let cases : obj[] seq = seq {
        let pairs = [
            "aligned00",     "aligned00_8"    // overlapping, same resolution
            "aligned00_8",   "aligned80"      // adjacent
            "aligned00",     "aligned88"      // disjoint
            "aligned88",     "alignedFine"    // overlapping, finer second
            "aligned00_8",   "alignedCoarse"  // overlapping, coarser second
            "centered4",     "aligned00"      // straddling origin vs touching origin
            "centeredFine",  "aligned00_8"    // straddling origin fine vs touching origin
            "centered2",     "centered4"      // both straddling origin
            "unaligned",     "aligned00_8"    // unaligned vs aligned
            "unalignedNeg",  "centered4"      // both straddling origin, unaligned
            "alignedCoarse", "centeredFine"   // coarse vs fine straddling origin
            "coarseSplit0",  "fineLeaf"       // coarse inner tree vs fine leaf inside it
            "fineLeaf",      "coarseSplit0"   // fine leaf vs coarse inner tree around it
        ]
        for (a, b) in pairs do
            for d in dominances.Keys do
                yield [| box a; box b; box d |]
        }

    [<Theory; MemberData(nameof cases)>]
    let ``Merge_MatchesOracle`` (a : string, b : string, dom : string) =
        let ia = inputs.[a]
        let ib = inputs.[b]
        let dom = dominances.[dom]
        let m = Quadtree.Merge dom (build ia) (build ib)

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
                let e = expected dom ia ib p
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
                match expected dom ia ib p, covering.[i, j] with
                | None, [] -> ()
                | Some e, [ (_, v) ] when v = e -> ()
                | e, cs -> errors.Add(sprintf "Query.All %A: %A, expected %A" p cs e)

        Assert.True(errors.Count = 0, sprintf "%d errors, first: %A" errors.Count (errors |> Seq.truncate 5 |> List.ofSeq))
