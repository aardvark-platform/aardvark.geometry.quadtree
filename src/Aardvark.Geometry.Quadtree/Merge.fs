namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System.Collections.Generic
open System.Collections.Immutable

(*
    Merge.
*)

     
type CellRelation =
    | DisjointCells
    | PartiallyOverlappingCells
    | IdenticalCells
    | FirstCellContainsSecond
    | SecondCellContainsFirst
with
    static member create (first : Cell2d) (second : Cell2d) =
        if first.IsCenteredAtOrigin && second.TouchesOrigin && second.Exponent >= first.Exponent then
            PartiallyOverlappingCells
        elif first.TouchesOrigin && second.IsCenteredAtOrigin && first.Exponent >= second.Exponent then
            PartiallyOverlappingCells
        else
            if first.Intersects(second) then
                match first.Contains(second), second.Contains(first) with
                | true, true   -> IdenticalCells
                | true, false  -> FirstCellContainsSecond
                | false, true  -> SecondCellContainsFirst
                | false, false -> PartiallyOverlappingCells
            else
                DisjointCells

type DirectChild     = { Child : QNodeRef; Index : int }
type IndirectChild   = { Child : QNodeRef; Index : int }
type DirectNested    = { Child : QNodeRef }
type IndirectNested  = { Child : QNodeRef }

type SubtreeRelation =
    /// non-centered child (e-1)
    | RelChildDirect    of DirectChild
    /// non-centered child (e-n where n>1)
    | RelChildIndirect  of IndirectChild
    /// centered parent Cell2d(e) -> centered child Cell2d(e-1)
    | RelNestedDirect   of DirectNested
    /// centered parent Cell2d(e) -> centered child Cell2d(e-n where n>1)
    | RelNestedIndirect of IndirectNested
with
    static member ofQNodeRef (root : Cell2d) (child : QNodeRef) : SubtreeRelation option =
        let c = child.Cell
        if root.Exponent > c.Exponent then
            let isDirect = c.Exponent + 1 = root.Exponent
            let qi = Option.ofNullable(root.GetQuadrant(c))
            match root.IsCenteredAtOrigin, qi, c.IsCenteredAtOrigin with
            | _,     Some qi,  false -> (if isDirect then RelChildDirect  { Child = child; Index = qi } else RelChildIndirect  { Child = child; Index = qi }) |> Some
            | true,  None,     true  -> (if isDirect then RelNestedDirect { Child = child }             else RelNestedIndirect { Child = child })             |> Some
            | _ -> None
        else
            None

module Merge =

    let winner dom (first : QNodeRef) (second : QNodeRef) : QNodeRef option =
        match dom, first.ExactBoundingBox, second.ExactBoundingBox with
        | FirstDominates,  bb1, bb2 when bb1.Contains(bb2) -> Some first
        | SecondDominates, bb1, bb2 when bb2.Contains(bb1) -> Some second
        | _ -> None

    let growParent (n : QNodeRef) : QNodeRef = QInnerNode.ofSubNode(n) |> InMemoryInner

    /// Immutable merge.
    let rec merge (dom : Dominance) (first : QNodeRef) (second : QNodeRef) : QNodeRef =

        let rec mergeToCommonRoot (dom : Dominance) (a : QNodeRef) (b : QNodeRef) : QNodeRef =

            match winner dom a b with
            | Some w -> w
            | None   ->
                let acell = a.Cell
                let bcell = b.Cell
                match CellRelation.create acell bcell with

                | IdenticalCells -> QMergeNode.ofNodes dom a b |> InMemoryMerge

                | DisjointCells ->
                    let rc = acell.Union(bcell)
                    invariant (acell <> rc && rc.Contains(acell) && bcell <> rc && rc.Contains(bcell)) "01d13ddf-5763-4fbf-8e34-10e1516d224f"
                    mergeToCommonRoot dom (a |> growParent) (b |> growParent)

                | FirstCellContainsSecond ->
                    
                    if a.Cell.IsCenteredAtOrigin then

                        if b.Cell.Exponent + 1 < a.Cell.Exponent then
                            mergeToCommonRoot dom a (b |> growParent)
                        else
                            // b is a quadrant of centered cell a
                            invariant (b.Cell.Exponent + 1 = a.Cell.Exponent && b.Cell.TouchesOrigin) "8ac53980-f7c9-4cae-8002-6f98560deb97"
                            QMergeNode.ofNodes dom a b |> InMemoryMerge
                            //failwith "todo: mergeToCommonRoot (b is a quadrant of centered cell a)"
                    else
                        mergeToCommonRoot dom a (b |> growParent)

                | SecondCellContainsFirst -> mergeToCommonRoot dom.Flipped b a

                | PartiallyOverlappingCells ->
                    invariant (a.Cell.IsCenteredAtOrigin <> b.Cell.IsCenteredAtOrigin) "933e7c94-c6e3-4149-a1aa-b89546ea38a9"
                    QMergeNode.ofNodes dom a b |> InMemoryMerge

        match winner dom first second with
        | Some w -> w
        | None   -> match first, second with
                    | NoNode,               b                   -> b
                    | a,                    NoNode              -> a
                    
                    | OutOfCoreNode (_,a),  OutOfCoreNode (_,b) -> merge dom (a() |> InMemoryNode) (b() |> InMemoryNode)
                    | OutOfCoreNode (_,a),  b                   -> merge dom (a() |> InMemoryNode) (b                  )
                    | a,                    OutOfCoreNode (_,b) -> merge dom (a                  ) (b() |> InMemoryNode)

                    | _ -> mergeToCommonRoot dom first second