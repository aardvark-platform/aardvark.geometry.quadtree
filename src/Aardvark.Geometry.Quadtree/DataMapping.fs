namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open System

/// Window is in absolute cell space (level given by bufferOrigin.Exponent),
/// where window.Min is inclusive and window.Max is exclusive.
/// If bufferOrigin is centered cell, then window is ignored and bufferSize MUST be (1,1).
type DataMapping(bufferOrigin : Cell2d, bufferSize : V2i, window : Box2l) =

    do
        if bufferOrigin.IsCenteredAtOrigin then
            invariant (bufferSize = V2i(1,1)) "e782c751-0c45-4748-a937-c32393692659."
        else
            let max = bufferOrigin.XY + V2l(bufferSize)
            if window.Min.X < bufferOrigin.X || window.Min.Y < bufferOrigin.Y || window.Max.X > max.X || window.Max.Y > max.Y then
                failwith "Invalid window. Invariant 8e2912ee-2a02-4fda-9a1c-6a1a2dfe801a."

    let getBufferIndex (x : int64, y : int64) =
        let dx = x - bufferOrigin.X
        let dy = y - bufferOrigin.Y
        if dx < 0L || dy < 0L || dx >= int64 bufferSize.X || dy >= int64 bufferSize.Y then failwith "Sample position out of range."
        int(dy) * bufferSize.X + int(dx)

    new (origin : Cell2d, size : V2i) =
        DataMapping(origin, size, Box2l.FromMinAndSize(origin.XY, V2l(size)))

    new (origin : V2l, size : V2i, exponent : int) =
        DataMapping(Cell2d(origin, exponent), size, Box2l.FromMinAndSize(origin, V2l(size)))

    new (origin : Cell2d, maxIncl : Cell2d) =
        if origin.Exponent <> maxIncl.Exponent then
            failwith "Invalid arguments. Exponents mismatch. Invariant ea829c55-edd1-4af9-8955-6aafddb88965."
        let size = maxIncl.XY - origin.XY + V2l.II
        if size.X < 0L || size.Y < 0L || size.X > int64 Int32.MaxValue || size.Y > int64 Int32.MaxValue then
            failwith "Invalid arguments. Invariant a447d0d5-9036-4372-ba22-19e28decbfaa."
        DataMapping(origin, V2i(size), Box2l.FromMinAndSize(origin.XY, size))

    new (origin : Cell2d) =
        if not origin.IsCenteredAtOrigin then failwith "Invariant 3bd119fe-ec23-40a8-9287-9c8d7abe49ce."
        DataMapping(origin, V2i.II, Box2l.Invalid)

    member ____.BufferOrigin with get() = bufferOrigin
    member ____.BufferSize with get() = bufferSize
    member ____.Window with get() = window
    member ____.WindowSize with get() = window.Size
    member ____.WindowWidth with get() = window.SizeX
    member ____.WindowHeight with get() = window.SizeY

    member this.GetBufferIndex (x : int64, y : int64) = getBufferIndex(x, y)
    member this.GetBufferIndex (x : int, y : int) = getBufferIndex(int64 x, int64 y)
    member this.GetBufferIndex (s : V2l) = getBufferIndex(s.X, s.Y)
    member this.GetBufferIndex (s : V2i) = getBufferIndex(int64 s.X, int64 s.Y)
    member this.GetBufferIndex (s : Cell2d) =
        if s.Exponent <> bufferOrigin.Exponent then failwith "Sample exponent out of range."
        getBufferIndex(s.X, s.Y)

    member this.Contains (box : Box2l) = window.Contains(box)

    member this.BoundingBox with get() =
        let min = Cell2d(window.Min, bufferOrigin.Exponent).BoundingBox.Min
        let max = Cell2d(window.Max, bufferOrigin.Exponent).BoundingBox.Min
        Box2d(min, max)

    member this.WithWindow (newWindow : Box2l) =
        let o = window.Intersection(newWindow)
        if o.IsInvalid || o.Area = 0L then
            None
        else
            Some <| DataMapping(bufferOrigin, bufferSize, o)
