# Overview
This library provides tools for out-of-core processing of unbounded multi-layer raster data.

It is a part of [**The Aardvark Platform**](https://aardvarkians.com/).

## Build

Run `./build.sh` on Linux, or `.\build.cmd` on Windows. Done.

# Example

```fsharp
open Aardvark.Geometry.Quadtree

// raw height data (4x3 samples stored in a flat array)
let heights = [| 
    1.0; 1.0; 2.0; 2.0
    1.5; 1.6; 1.7; 1.8
    1.6; 1.7; 2.0; 2.2
    |]

// raw color data
let colors = [| 
    C3b(255,0,0); C3b(255,1,0); C3b(255,2,0); C3b(255,3,0)
    C3b(0,255,1); C3b(1,255,1); C3b(2,255,1); C3b(3,255,1)
    C3b(0,2,255); C3b(1,2,255); C3b(2,2,255); C3b(3,2,255)
    |]

// define mapping of raw data to raster space
let mapping = DataMapping(origin = Cell2d(500000L, 2000L, 0), size = V2i(4, 3))

// a layer gives meaning to raw data
let heightsLayer = Layer.Create Defs.Heights1d heights mapping
let colorLayer   = Layer.Create Defs.Colors3b  colors  mapping

// build the quadtree (incl. levels-of-detail)
let qtree = Quadtree.Build BuildConfig.Default [| heightsLayer; colorLayer |]

// query
let config = Query.Config.Default
let line = Ray2d(origin = V2d(500_000, 2_000), direction = V2d(1,1))
let withinDistance = 1.5
let chunks = qtree |> Query.NearLine config line withinDistance

let positions = // : V3d[]
    chunks 
    |> Seq.collect (fun chunk -> chunk.GetSamples<float> Defs.Heights1d)
    |> Seq.map (fun (cell, h) -> V3d(cell.GetCenter(), h))
    |> Array.ofSeq
// positions = [|[500000.5, 2000.5, 1]; [500001.5, 2001.5, 1.6]; [500002.5, 2002.5, 2]|]

let colors = // : C3b[]
    chunks
    |> Seq.collect (fun chunk -> chunk.GetSamples<C3b> Defs.Colors3b)
    |> Seq.map snd
    |> Array.ofSeq
// colors = [|[255, 0, 0]; [1, 255, 1]; [2, 2, 255]|]
```

# Reference

## DataMapping

Defines a mapping of a raw data array `buffer:'a[]` to cell space.

A `DataMapping` does not contain an actual buffer. It can be applied to multiple buffers with identical layout. A `Layer` is used to combine data and mappings.

Property     | Type        | Description
------------ | ----------- | -----------
**`Origin`** | `Cell2d`    | First element (`buffer.[0]`) corresponds to this cell.
**`Size`**   | `V2i`       | Buffer contains `Size.X * Size.Y` element.
**`Window`** | `Box2l`     | Use sub-region of data, where `Min` is inclusive and `Max` is exclusive.

The last buffer element corresponds to `Cell2d(Origin.X + Size.X - 1, Origin.Y + Size.Y - 1, Origin.Exponent)`.

By default, `Window` corresponds to `Box2l.FromMinAndSize(Origin.XY, Size)`.

## Layer

A `Layer` specifies a `DataMapping` and a **semantic** for raw data.

Semantic | Description
-------- | -----------
`Defs.`**`Heights1f`** | Height value per sample. Float32[].
`Defs.`**`Heights1d`** | Height value per sample. Float64[].
`Defs.`**`Normals3f`** | Normal vector per sample. V3f[].
`Defs.`**`HeightStdDevs1f`** | Standard deviation per height value. Float32[].
`Defs.`**`Colors3b`** | Color value per sample. C3b[].
`Defs.`**`Colors4b`** | Color value per sample. C4b[].
`Defs.`**`Intensities1i`** | Intensity value per sample. Int32[].

## Quadtrees

```fsharp
BuildConfig = {
    SplitLimit = 256 // splits tile if width or height is greater than 256
}
```

In a future version, it will be possible to configure out-of-core storage of quadtrees.
Currently quadtrees are created in-memory.

## Queries

```fsharp
let config = { 
    MinExponent = 2
    SampleMode = Query.SampleMode.Center
    Verbose = false
    }
```

All queries return a sequence of `Query.Result` objects.

Each result references a quadtree node, and whether it is **fully** or **partially** selected.

```fsharp
type NodeSelection =
     | FullySelected
     | PartiallySelected of Cell2d[]

type Result = {
    Node : INode
    Selection : NodeSelection
    }
```

Query | Description
-------- | -----------
`Query.`**`InsideCell`** | All samples inside given cell.
`Query.`**`InsideBox`** | All samples inside given box.
`Query.`**`InsidePolygon`** | All samples inside given polygon.
`Query.`**`NearLine`** | All samples within a given distance of a line.