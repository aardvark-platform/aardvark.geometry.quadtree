namespace Aardvark.Geometry.Quadtree.Tests

open Aardvark.Base
open Aardvark.Geometry.Quadtree
open System

[<AutoOpen>]
module Prelude =

    [<Measure>] type powerOfTwo

    let createQuadtree (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int<powerOfTwo>) =
        let size = V2i(w, h)
        let xs = Array.zeroCreate<float32> (w * h)
        for y = 0 to size.Y - 1 do
            for x = 0 to size.X - 1 do
                let i = y * size.X + x
                xs.[i] <- float32 x + float32 y / 100.0f

        let a = Layer(Defs.Heights1f, xs, DataMapping(V2l(ox, oy), size, exponent = e))

        let config = { BuildConfig.Default with SplitLimitPowerOfTwo = int splitLimit }
        Quadtree.Build config [| a |]

    let createQuadtreeWithRandomValues (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int<powerOfTwo>) =
        let r = Random()
        let size = V2i(w, h)
        let xs = Array.zeroCreate<float32> (w * h)
        for y = 0 to size.Y - 1 do
            for x = 0 to size.X - 1 do
                let i = y * size.X + x
                xs.[i] <- -100.0f + float32(r.NextDouble()) * 200.0f

        let a = Layer(Defs.Heights1f, xs, DataMapping(V2l(ox, oy), size, exponent = e))

        let config = { BuildConfig.Default with SplitLimitPowerOfTwo = int splitLimit }
        Quadtree.Build config [| a |]

    let createQuadtreeWithValue (ox : int) (oy : int) (w : int) (h : int) (e : int) (splitLimit : int<powerOfTwo>) (value : float32) =
        let size = V2i(w, h)
        let xs = Array.zeroCreate<float32> (w * h)
        for y = 0 to size.Y - 1 do
            for x = 0 to size.X - 1 do
                let i = y * size.X + x
                xs.[i] <- value

        let a = Layer(Defs.Heights1f, xs, DataMapping(V2l(ox, oy), size, exponent = e))

        let config = { BuildConfig.Default with SplitLimitPowerOfTwo = int splitLimit }
        Quadtree.Build config [| a |]


