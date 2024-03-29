﻿namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System
open System.Collections.Generic
open System.Runtime.CompilerServices

[<AutoOpen>]
module Prelude =

    let inline invariant condition id =
        if not condition then failwith <| sprintf "Invariant %s." id

    let inline invariantm condition (msg : unit -> string) id =
        if not condition then failwith <| sprintf "%s Invariant %s." (msg()) id

    let internal kvp def x = KeyValuePair<Durable.Def, obj>(def, x :> obj)

    let inline minInt64 (a : int64) (b : int64) = if a < b then a else b
    let inline maxInt64 (a : int64) (b : int64) = if a > b then a else b

    let inline getParentCellAndIndex (cell : Cell2d) : (Cell2d * int) =
        invariant (not cell.IsCenteredAtOrigin) "dd478bd0-f646-4fc9-9874-b56b518a120e"
        let parent = cell.Parent
        let qi = parent.GetQuadrant(cell).Value
        (parent, qi)

    let rec getParentForLevel level (c : Cell2d) =
        if c.Exponent > level then failwith "Cell exponent must be less or equal than requested level. Invariant e51c067e-bfb9-4548-a685-322dfad65da2."
        if c.Exponent = level then c else getParentForLevel level c.Parent

    /// If any of the cells is centered, then this returns false.
    let inline inDifferentQuadrants (a : Cell2d) (b : Cell2d) =
        if a.IsCenteredAtOrigin || b.IsCenteredAtOrigin then false
        else ((a.X >= 0L) <> (b.X >= 0L)) || ((a.Y >= 0L) <> (b.Y >= 0L))

    /// Gets cell bounds in resolution 2^e with inclusive min and exclusive max.
    let getBoundsForExponent (e : int) (cell : Cell2d) : Box2l =
        if cell.IsCenteredAtOrigin then
            invariant (e < cell.Exponent) "ecfc0260-688b-43f2-9e10-a0bc66132ac1"
            let d = cell.Exponent - e - 1
            let x = 1L <<< d
            Box2l(-x, -x, x, x)
        else
            invariant (e <= cell.Exponent) "316764bd-c4fe-4202-bba4-bf603061b629"
            let d = cell.Exponent - e
            Box2l(cell.X <<< d, cell.Y <<< d, (cell.X + 1L) <<< d, (cell.Y + 1L) <<< d)

module Option =

    let merge2 f x y = match x, y with | None, None -> None | Some x, None | None, Some x -> Some x | Some x, Some y -> f x y |> Some

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
    static member Union (a : Cell2d, b : Cell2d) : Cell2d =
        
        let inline growUntilTouchesOrigin (c : Cell2d) =
            if c.IsCenteredAtOrigin then failwith "Cell must not be centered. Error d46dccaa-ddba-4da1-84dc-db805ef9890e."
            let mutable c = c
            while not c.TouchesOrigin do c <- c.Parent
            c

        let inline firstIsCentered (other : Cell2d) =
            let mutable c = other
            while not c.TouchesOrigin do c <- c.Parent
            Cell2d(c.Exponent + 1)
        
        let bothAreNotCentered (a : Cell2d) (b : Cell2d) =
            
            if inDifferentQuadrants a b then
                let a = growUntilTouchesOrigin a
                let b = growUntilTouchesOrigin b
                if a.Exponent >= b.Exponent then Cell2d(a.Exponent + 1) else Cell2d(b.Exponent + 1)
            else
                let mutable a = a
                let mutable b = b
                let mutable i = 0
                while a <> b do
                    i <- i + 1
                    if i = 1000 then failwith "khjlkhkjh"
                    if a.Exponent = b.Exponent then a <- a.Parent; b <- b.Parent
                    elif a.Exponent > b.Exponent then b <- b.Parent
                    else a <- a.Parent
                a
                
        match a.IsCenteredAtOrigin, b.IsCenteredAtOrigin with
        | false, false -> bothAreNotCentered a b
        | true,  false -> firstIsCentered b
        | false, true  -> firstIsCentered a
        | true,  true  -> if a.Exponent > b.Exponent then a else b

    [<Extension>]
    static member GetBoundsForExponent (self : Cell2d, e : int) : Box2l =
        let d = self.Exponent - e
        if self.IsCenteredAtOrigin then
            if d > 0 then
                let f = 1L <<< (d - 1)
                Box2l(-f, -f, +f, +f)
            else
                sprintf "Undefined result for %A.GetBoundsForExponent(%d). Error e1ae7574-6268-4674-90ae-5530eec29066." self e |> failwith
        else
            if d < 0 then 
                let mutable cell = self
                while cell.Exponent < e do cell <- cell.Parent
                Box2l.FromMinAndSize(cell.XY, 1L, 1L)
            elif d = 0 then
                Box2l.FromMinAndSize(self.XY, 1L, 1L)
            else
                let f = 1L <<< d
                let x = self.X <<< d
                let y = self.Y <<< d
                Box2l(x, y, x + f, y + f)


[<Extension>]
type Box2dExtensions =

    [<Extension>]
    static member inline ContainsMaxExclusive (self : Box2d, other : Box2d) : bool =
        self.Min.X <= other.Min.X && self.Min.Y <= other.Min.Y && self.Max.X > other.Max.X && self.Max.Y > other.Max.Y

    [<Extension>]
    static member inline ContainsMaxExclusive (self : Box2d, p : V2d) : bool =
        self.Min.X <= p.X && self.Min.Y <= p.Y && self.Max.X > p.X && self.Max.Y > p.Y
                
[<Extension>]
type Box2lExtensions =

    [<Extension>]
    static member inline ContainsMaxExclusive (self : Box2l, other : Box2l) : bool =
        self.Min.X <= other.Min.X && self.Min.Y <= other.Min.Y && self.Max.X > other.Max.X && self.Max.Y > other.Max.Y

    [<Extension>]
    static member inline ContainsMaxExclusive (self : Box2l, p : V2l) : bool =
        self.Min.X <= p.X && self.Min.Y <= p.Y && self.Max.X > p.X && self.Max.Y > p.Y

    [<Extension>]
    static member inline IntersectsMaxExclusive (self : Box2l, other : Box2l) : bool =
        other.Max.X > self.Min.X && other.Max.Y > self.Min.Y && other.Min.X < self.Max.X && other.Min.Y < self.Max.Y

    [<Extension>]
    static member inline TryIntersect (self : Box2l, other : Box2l) : Box2l option =
        let xmin = maxInt64 self.Min.X other.Min.X
        let xmax = minInt64 self.Max.X other.Max.X
        if xmin >= xmax then
            None
        else
            let ymin = maxInt64 self.Min.Y other.Min.Y
            let ymax = minInt64 self.Max.Y other.Max.Y
            if ymin >= ymax then
                None
            else
                Box2l(xmin, ymin, xmax, ymax) |> Some

    [<Extension>]
    static member inline GetAllSamples (self : Box2l, exponent : int) : Cell2d[] =
        let xMaxIncl = int self.Size.X - 1
        let yMaxIncl = int self.Size.Y - 1

        let samples = Array.zeroCreate (int self.Size.X * int self.Size.Y)
        let mutable i = 0
        for y = 0 to yMaxIncl do
            for x = 0 to xMaxIncl do
                samples.[i] <- Cell2d(self.Min.X + int64 x, self.Min.Y + int64 y, exponent)
                i <- i + 1
        samples

    [<Extension>]
    static member GetAllSamplesFromFirstMinusSecond (first : Box2l, second : Box2l, exponent : int) : Cell2d[] =
        
        match first.TryIntersect(second) with
        | Some b ->
            let a = first

            let box00 = Box2l(a.Min.X, a.Min.Y, b.Min.X, b.Min.Y)
            let box10 = Box2l(b.Min.X, a.Min.Y, b.Max.X, b.Min.Y)
            let box20 = Box2l(b.Max.X, a.Min.Y, a.Max.X, b.Min.Y)

            let box01 = Box2l(a.Min.X, b.Min.Y, b.Min.X, b.Max.Y)
            /// box11 = Box2l(b.Min.X, b.Min.Y, b.Max.X, b.Max.Y)
            let box21 = Box2l(b.Max.X, b.Min.Y, a.Max.X, b.Max.Y)
            
            let box02 = Box2l(a.Min.X, b.Max.Y, b.Min.X, a.Max.Y)
            let box12 = Box2l(b.Min.X, b.Max.Y, b.Max.X, a.Max.Y)
            let box22 = Box2l(b.Max.X, b.Max.Y, a.Max.X, a.Max.Y)

            let count = box00.Area + box10.Area + box20.Area +
                        box01.Area +              box21.Area +
                        box02.Area + box12.Area + box22.Area

            let result = Array.zeroCreate (int count)
            let mutable ri = 0
            let add (box : Box2l) =
                let mutable x = box.Min.X
                let mutable y = box.Min.Y
                let xmaxincl = int box.SizeX-1
                let ymaxincl = int box.SizeY-1
                for j = 0 to ymaxincl do
                    x <- box.Min.X
                    for i = 0 to xmaxincl do
                        result.[ri] <- Cell2d(x, y, exponent)
                        x <- x + 1L
                        ri <- ri + 1
                    y <- y + 1L

            add box00; add box10; add box20
            add box01;            add box21
            add box02; add box12; add box22

            result

        | None ->
            // nothing substracted -> simply return first
            first.GetAllSamples(exponent)