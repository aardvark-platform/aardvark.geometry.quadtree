namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System.Collections.Generic
open System.Collections.Immutable

(*
    Merge.
*)

/// Which data dominates in merge operations.
type Dominance = 
    | FirstDominates 
    | SecondDominates 
    | MoreDetailedOrFirst
    | MoreDetailedOrSecond
with
    member this.Flipped with get() =
        match this with 
        | FirstDominates       -> SecondDominates
        | SecondDominates      -> FirstDominates
        | MoreDetailedOrFirst  -> MoreDetailedOrSecond
        | MoreDetailedOrSecond -> MoreDetailedOrFirst
        
module Merge =

    /// Immutable merge.
    let merge (dominance : Dominance) (first : QNodeRef) (second : QNodeRef) : QNodeRef =

        match first.TryGetInMemory(), second.TryGetInMemory() with
        | None,   None   -> NoNode
        | Some _, None   -> first
        | None,   Some _ -> second
        | Some a, Some b -> 
            
            invariantm (a.SplitLimitExponent = b.SplitLimitExponent)
                "Cannot merge quadtrees with different split limits."   "6222eb6b-a7aa-43c1-9323-e28d6275696b"

            failwith "TODO: implement"
