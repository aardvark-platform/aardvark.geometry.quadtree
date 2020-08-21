namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System
open System.Collections.Generic
open System.Runtime.CompilerServices

/// Which data dominates in merge operations.
type Dominance = 
    | FirstDominates 
    | SecondDominates 
    | MoreDetailedDominates

[<AutoOpen>]
module Prelude =

    let inline invariant condition id =
        if not condition then failwith <| sprintf "Invariant %s." id

    let inline invariantm condition msg id =
        if not condition then failwith <| sprintf "%s Invariant %s." msg id

    let kvp def x = KeyValuePair<Durable.Def, obj>(def, x :> obj)

    let flipDomination d =
           match d with
           | FirstDominates        -> SecondDominates
           | SecondDominates       -> FirstDominates
           | MoreDetailedDominates -> MoreDetailedDominates



[<AutoOpen>]
module Extensions =

    type Box2l with
        member this.IsSizePowerOfTwoSquared with get() = this.SizeX = this.SizeY && this.SizeX.IsPowerOfTwo()
        member this.SplitAtCenter () =
            let c = this.Center
            [|
                Box2l(this.Min.X, this.Min.Y,        c.X,        c.Y)
                Box2l(       c.X, this.Min.Y, this.Max.X,        c.Y)
                Box2l(this.Min.X,        c.Y,        c.X, this.Max.Y)
                Box2l(       c.X,        c.Y, this.Max.X, this.Max.Y)
            |]

    type Cell2d with
        static member Create(xy : V2l, e : int) = Cell2d(xy, e)
        member this.SideLength with get() = Math.Pow(2.0, float this.Exponent)

[<Extension>]
type Cell2dExtensions =

    [<Extension>]
    static member GetBoundsForExponent (self : Cell2d, e : int) : Box2l =
        let d = self.Exponent - e
        if d < 0 then 
            failwith "Invariant 9fa8c845-2821-4cfa-9fc2-f9b8c15030f7."
        else
            let f = 1L <<< d
            let x = self.X <<< d
            let y = self.Y <<< d
            Box2l(x, y, x + f, y + f)
                
