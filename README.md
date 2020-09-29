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
`Defs.`**`HeightsBilinear4f`** | Height value per sample as bilinear params.<br>height(x,y) = A + B*x + C*y+ D*x*y,<br>where A=v.X, B=v.Y, C=v.Z, D=v.W. | `V4f[]` | `V4f[]`
`Defs.`**`HeightsBilinear4d`** | Height value per sample as bilinear params.<br>height(x,y) = A + B*x + C*y+ D*x*y,<br>where A=v.X, B=v.Y, C=v.Z, D=v.W. | `V4d[]` | `V4d[]`
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
`Defs.`**`BilinearParams4d`** | Bilinear params per sample. | `V4d[]` | `V4d[]`
*planned*<br>`Defs.`**`Volumes1f`** | Volume value (height difference) per sample. | `float32[]` | `float[]` 
*planned*<br>`Defs.`**`Volumes1d`** | Volume value (height difference) per sample. | `float[]` | `double[]`
*planned*<br>`Defs.`**`VolumesBilinear4f`** | Volume value (height difference) per sample as bilinear params.<br>volume(x,y) = A + B*x + C*y+ D*x*y,<br>where A=v.X, B=v.Y, C=v.Z, D=v.W. | `V4f[]` | `V4f[]`
*planned*<br>`Defs.`**`VolumesBilinear4d`** | Volume value (height difference) per sample as bilinear params.<br>volume(x,y) = A + B*x + C*y+ D*x*y,<br>where A=v.X, B=v.Y, C=v.Z, D=v.W. | `V4d[]` | `V4d[]`

## Quadtrees

Quadtrees are a persistent (immutable) data structure.
This means, that existing quadtrees will never be mutated.
All operations that mutate a quadtree will return a new (mutated) copy of the original quadtree. Of course no literal "copy" is made, but original parts will be re-used where possible.

### Build

```fsharp
/// At least 1 layer is required, and
/// all layers must have the same sample exponent and sample window.
Quadtree.Build (config : BuildConfig) (layers : ILayer[]) : QNodeRef
```
where `BuildConfig.Default` is defined as

```fsharp
BuildConfig = {
    SplitLimitPowerOfTwo = 8 // splits tile if width or height is greater than 256
}
```
.

As the layer data is already in memory, the `Quadtree.Build` function will construct the quadtree **in memory** as well. This results in optimal performance and the memory overhead is slightly more than 1/3 of layer data size and mostly used for storing levels-of-detail.

In order to handle large data sets, that do not fit into memory, you can split up your data set into chunks, create and save a quadtree for each chunk, and finally merge all the chunks into a single quadtree.

See the following sections how to save, load and merge quadtrees.

### Save/Load

Quadtrees can be saved to disk using an arbitrary key/value store.
The store is configured using a `SerializationOptions` record.

```fsharp
/// Save quadtree. Returns id of root node, or Guid.Empty if empty quadtree.
Quadtree.Save (options : SerializationOptions) (qtree : QNodeRef) : Guid
```

You can load a quadtree using

```fsharp
/// Load quadtree with given id.
/// Returns the tree's root node, with children being loaded lazily.
/// If id does not exist, then `NoNode` is returned.
Quadtree.Load (options : SerializationOptions) (id : Guid) : QNodeRef
```

A `SerializationOptions` record specifies a few functions allowing to interact
with the underlying key/value store.

```fsharp
type SerializationOptions = {
    Save : Guid -> byte[] -> unit
    TryLoad : Guid -> byte[] option
    Exists : Guid -> bool
    }
```

`Aardvark.Geometry.Quadtree' provides the following out-of-the-box serialization bindings:

```fsharp
/// Create an in-memory store for testing purposes.
/// This does NOT solve the out-of-core problem. ;-)
let inMemoryStore = SerializationOptions.NewInMemoryStore ()
```

```fsharp
/// Create a disk store using the Uncodium.SimpleStore package.
let simpleDiskStore = SerializationOptions.SimpleDiskStore (path : string)
```

Additional bindings may be available if you use this library in conjunction with some larger project. Or, you can quite easily create your own, by providing a few simple functions to interact with your favourite key/value blob store.

### Merge

You can use `Quadtree.Merge` to create a new quadtree from two existing quadtree.
An immutable merge is performed, meaning the original quadtrees remain untouched.

```fsharp
Quadtree.Merge (d : Dominance) (first : QNodeRef) (second : QNodeRef) : QNodeRef
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



## Queries

Currently available query functions:

Query | Description
-------- | -----------
`Query.`**`All`** | All samples in quadtree.
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
    Node : QNode
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
