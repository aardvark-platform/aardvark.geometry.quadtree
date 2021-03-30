namespace Aardvark.Geometry.Quadtree.Scratch

open Aardvark.Base
open Aardvark.Geometry.Quadtree
open System
open System.Diagnostics

module Perftests =

    let buildQuadtree (origin : V2l) (size : V2i) (e : int) =
        let data = Array.zeroCreate<float32> (size.X * size.Y)
        let mapping = DataMapping(origin = origin, size = size, exponent = e)
        let layer = Layer(Defs.Heights1f, data, mapping)
        Quadtree.Build BuildConfig.Default [| layer |]

    let createReallyLargeMergedQuadtree () =
        let a = buildQuadtree (V2l(0,   0))      (V2i(1000,1000))      0
        let b = buildQuadtree (V2l(0, 200) * 8L) (V2i( 888, 666) * 8) -3
        let m = Quadtree.Merge Dominance.MoreDetailedOrSecond a b
        //printfn "%A" a.ExactBoundingBox
        //printfn "%A" b.ExactBoundingBox
        //printfn "%A" m.ExactBoundingBox
        m

    let timeReallyLargeMergedQuadtreeQueries () =

        let q = createReallyLargeMergedQuadtree ()
        let ebb = q.ExactBoundingBox
        //printfn "m: %A" ebb
        printfn "start"

        let iterations = 10
        let n = 50

        let config = { Query.Config.Default with Verbose = false }
        let sw = Stopwatch()
        for _ = 1 to iterations do
            let r = Random(4711)
            let mutable totalSeconds = 0.0
            let mutable i = 0
            while i < n do

                if config.Verbose then printfn "[timeReallyLargeMergedQuadtreeQueries] i = %d" i

                try
                    let rv (v : V2d) = V2d(v.X * r.NextDouble(), v.Y * r.NextDouble())
                    let p0 = ebb.Min + rv(ebb.Size)
                    let p1 = p0 + rv(V2d(10.0, 10.0))
                    let p2 = p0 + rv(V2d(10.0, 10.0))
                    let poly = Polygon2d(p0, p1, p2)
                    if not (poly.IsCcw()) then poly.Reverse()

                    //printfn "poly: %A   (area = %f)" poly (poly.ComputeArea())

                    sw.Restart()

                    let samples =
                        Query.InsidePolygon config poly q 
                        |> Seq.collect (fun x -> x.GetSamples<float32> Defs.Heights1f)
                        |> Seq.map fst
                        |> Seq.toArray
                
                    //printfn " -> samples: %d" samples.Length

                    let data =
                        Query.InsidePolygon config poly q 
                        |> Seq.collect (fun x -> x.GetSamples<float32> Defs.Heights1f)
                        |> Seq.map (fun (c,_) -> Query.IntersectsCell config c q |> Seq.collect (fun x -> x.GetSamples<float32> Defs.Heights1f))
                        |> Seq.toArray

                    ()

                with
                | _ -> printfn "failed"

                sw.Stop()
                totalSeconds <- totalSeconds + sw.Elapsed.TotalSeconds
                i <- i + 1

            printfn "avg: %A" (totalSeconds / float(n))
