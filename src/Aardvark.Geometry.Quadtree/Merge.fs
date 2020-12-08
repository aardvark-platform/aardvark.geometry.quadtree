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

    let mergeInnerInner (dom : Dominance) (a : QInnerNode) (b : QInnerNode) : QNodeRef =
        failwith "todo: mergeInnerInner"

    let mergeInnerLeaf  (dom : Dominance) (a : QInnerNode) (b : QNode)      : QNodeRef =
        failwith "todo: mergeInnerLeaf"

    let mergeLeafLeaf   (dom : Dominance) (a : QNode)      (b : QNode)      : QNodeRef =
        failwith "todo: mergeInnerLeaf"

    /// Immutable merge.
    let rec merge (dom : Dominance) (first : QNodeRef) (second : QNodeRef) : QNodeRef =

        match first, second with
        | NoNode, _ | _, NoNode -> ()
        | _ ->
            invariantm (first.SplitLimitExponent = second.SplitLimitExponent)
                "Cannot merge quadtrees with different split limits."   "6222eb6b-a7aa-43c1-9323-e28d6275696b"

        match first, second with
        | NoNode,               b                       -> b
        | a,                    NoNode                  -> a

        | InMemoryInner a,      InMemoryInner b         -> mergeInnerInner dom a b
        | InMemoryInner a,      InMemoryNode  b         -> mergeInnerLeaf  dom a b
        | InMemoryNode a ,      InMemoryInner b         -> mergeInnerLeaf  dom.Flipped b a
        | InMemoryNode a ,      InMemoryNode  b         -> mergeLeafLeaf   dom a b

        | OutOfCoreNode (_,a),  OutOfCoreNode (_,b)     -> merge dom (a() |> InMemoryNode) (b() |> InMemoryNode)
        | OutOfCoreNode (_,a),  b                       -> merge dom (a() |> InMemoryNode) (b                  )
        | a,                    OutOfCoreNode (_,b)     -> merge dom (a                  ) (b() |> InMemoryNode)
