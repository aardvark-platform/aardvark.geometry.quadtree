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
let heightsLayer = Layer(Defs.Heights1d, heights, mapping)
let colorLayer   = Layer(Defs.Colors3b , colors,  mapping)

// build the quadtree (incl. levels-of-detail)
let qtree = Quadtree.Build BuildConfig.Default [| heightsLayer; colorLayer |]

// query
let config = Query.Config.Default
let line = Ray2d(origin = V2d(500_000, 2_000), direction = V2d(1,1))
let withinDistance = 0.5
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

Semantic | Description | F# Type | C# Type
-------- | ----------- | -------------- | ------
`Defs.`**`Heights1f`** | Height value per sample. | `float32[]` | `float[]` 
`Defs.`**`Heights1d`** | Height value per sample. | `float[]` | `double[]` 
`Defs.`**`Normals3f`** | Normal vector per sample. | `V3f[]` | `V3f[]`
`Defs.`**`Normals3d`** | Normal vector per sample. | `V3d[]` | `V3d[]`
`Defs.`**`HeightStdDevs1f`** | Std dev per height value. | `float32[]` | `float[]`
`Defs.`**`HeightStdDevs1d`** | Std dev per height value. | `float[]` | `double[]`
`Defs.`**`Colors3b`** | Color value per sample. | `C3b[]` | `C3b[]`
`Defs.`**`Colors4b`** | Color value per sample. | `C4b[]` | `C4b[]`
`Defs.`**`Colors3f`** | Color value per sample. | `C3f[]` | `C3f[]`
`Defs.`**`Colors4f`** | Color value per sample. | `C4f[]` | `C4f[]`
`Defs.`**`Intensities1i`** | Intensity value per sample. | `int[]` | `int[]`
`Defs.`**`Intensities1l`** | Intensity value per sample. | `int64[]` | `long[]`
`Defs.`**`Intensities1f`** | Intensity value per sample. | `float32[]` | `float[]`
`Defs.`**`Intensities1d`** | Intensity value per sample. | `float[]` | `double[]`
`Defs.`**`BilinearParams4f`** | Bilinear params per sample. | `V4f[]` | `V4f[]`
`Defs.`**`BilinearParams4d`** | Bilinear params per sample. | `V4d[]` | `V4d[]`

## Quadtrees

Quadtrees are a persistent (immutable) data structure.
This means, that existing quadtrees will never be mutated.
All operations that mutate a quadtree will return a new (mutated) copy of the original quadtree. Of course no literal "copy" is made, but original parts will be re-used where possible.

```fsharp
BuildConfig = {
    SplitLimit = 256 // splits tile if width or height is greater than 256
}
```

In a future version, it will be possible to configure out-of-core storage of quadtrees.
Currently quadtrees are created in-memory.

## Queries

Currently available query functions:

Query | Description
-------- | -----------
`Query.`**`InsideCell`** | All samples fully inside given cell.
`Query.`**`IntersectsCell`** | All samples with sample cell intersecting given cell. If query cell is smaller than data cells this will also return a data cell surrounding the query cell even if sample position is outside query cell. Sample mode is ignored.
`Query.`**`InsideBox`** | All samples inside given box.
`Query.`**`InsidePolygon`** | All samples inside given polygon.
`Query.`**`NearLine`** | All samples within a given distance of a line.

Please see the example above on how to use query functions. 

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

You can either use the default config `Query.Config.Default`, which returns the most detailed data,

```fsharp
{ 
    MinExponent = Int32.MinValue // most detailed data available
    SampleMode = SampleMode.Center
    Verbose = false
}
```

or create your own custom config


```fsharp
let myCustomConfig = { 
    MinExponent = 2
    SampleMode = Query.SampleMode.Center
    Verbose = false
    }
```


## Merge

You can use `Quadtree.Merge` or `Quadtree.TryMerge` to create a new octree from two existing octrees. An immutable merge is performed, meaning the original octrees remain untouched.

```fsharp
Quadtree.Merge (d : Dominance) (first : INode) (second : INode) : INode

Quadtree.TryMerge (d : Dominance) (first : INode option) (second : INode option) : INode option
```

The `dominance` parameter is used to control which samples will be used in overlapping areas, where both the first and second quadtree contain samples:

Dominance               | Description
----------------------- | -----------
`MoreDetailedDominates` | Finer resolution wins. If the same, than it is undefined if  data from the first or second quadtree is used.   
`FirstDominates`        | First quadtree always wins.
`SecondDominates`       | Second quadtree always wins.

```fsharp
let firstQuadtree = ...
let secondQuadtree = ...

let mergedQuadtree = Quadtree.Merge MoreDetailedDominates firstQuadtree secondQuadtree
```