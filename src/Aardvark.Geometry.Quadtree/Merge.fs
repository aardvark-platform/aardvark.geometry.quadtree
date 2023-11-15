namespace Aardvark.Geometry.Quadtree

open Aardvark.Base

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

            let safeIntersect (a : Cell2d) (b : Cell2d) =
                match a.IsCenteredAtOrigin, b.IsCenteredAtOrigin with
                | false, false -> if a.Exponent >= b.Exponent then (getParentForLevel a.Exponent b) = a
                                  else (getParentForLevel b.Exponent a) = b
                | true,  false -> if b.Exponent < a.Exponent then (getParentForLevel (a.Exponent-1) b).TouchesOrigin else b.TouchesOrigin
                | false, true  -> if a.Exponent < b.Exponent then (getParentForLevel (b.Exponent-1) a).TouchesOrigin else a.TouchesOrigin
                | true,  true  -> true

            let safeContains (a : Cell2d) (b : Cell2d) =
                match a.IsCenteredAtOrigin, b.IsCenteredAtOrigin with
                | false, false -> if b.Exponent > a.Exponent then false else getParentForLevel a.Exponent b = a
                | true,  false -> if b.Exponent > a.Exponent - 1 then false else (getParentForLevel (a.Exponent-1) b).TouchesOrigin
                | false, true  -> false
                | true,  true  -> b.Exponent <= a.Exponent

            if safeIntersect first second then
                match safeContains first second, safeContains second first with
                | true, true   ->
                    invariant (first = second) "06d30a66-8ee3-43d3-9922-f4ac440039c4"
                    IdenticalCells
                | true, false  -> FirstCellContainsSecond
                | false, true  -> SecondCellContainsFirst
                | false, false -> 
                    invariant (first.IsCenteredAtOrigin <> second.IsCenteredAtOrigin) "1c2034af-0b3e-4672-9e0b-61e414858c10"
                    PartiallyOverlappingCells
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
        
        if first.HasMask || second.HasMask then
            None
        else
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

                    if rc.IsCenteredAtOrigin then
                        if inDifferentQuadrants acell bcell then
                            match acell.TouchesOrigin, bcell.TouchesOrigin with
                            | false, false -> mergeToCommonRoot dom (a |> growParent) (b |> growParent)
                            | true,  false -> mergeToCommonRoot dom (a              ) (b |> growParent)
                            | false, true  -> mergeToCommonRoot dom (a |> growParent) (b              )
                            | true,  true  -> QMergeNode.ofNodes dom a b |> InMemoryMerge
                        else
                            match acell.IsCenteredAtOrigin, bcell.IsCenteredAtOrigin with
                            | false, false -> // in same quadrant, but rc is centered -> impossible
                                              failwith "Invariant 18e8cd18-53f3-47d1-b3c6-79628d49a5f8."
                            | false, true  -> if acell.TouchesOrigin then QMergeNode.ofNodes dom a b |> InMemoryMerge
                                              else mergeToCommonRoot dom (a |> growParent) b // grow a to touch origin
                            | true,  false -> if bcell.TouchesOrigin then QMergeNode.ofNodes dom a b |> InMemoryMerge
                                              else mergeToCommonRoot dom a (b |> growParent) // grow b to touch origin
                            | true,  true  -> QMergeNode.ofNodes dom a b |> InMemoryMerge
                            
                    else
                        mergeToCommonRoot dom (a |> growParent) (b |> growParent)

                | FirstCellContainsSecond ->
                    
                    if acell.IsCenteredAtOrigin then

                        if bcell.IsCenteredAtOrigin then
                            QMergeNode.ofNodes dom a b |> InMemoryMerge
                        else
                            if bcell.Exponent + 1 < acell.Exponent then
                                mergeToCommonRoot dom a (b |> growParent)
                            else
                                // b is a quadrant of centered cell a
                                invariant (bcell.Exponent + 1 = acell.Exponent && bcell.TouchesOrigin) "8ac53980-f7c9-4cae-8002-6f98560deb97"
                                QMergeNode.ofNodes dom a b |> InMemoryMerge
                    else
                        mergeToCommonRoot dom a (b |> growParent)

                | SecondCellContainsFirst -> mergeToCommonRoot dom.Flipped b a

                | PartiallyOverlappingCells ->
                    invariant (a.Cell.IsCenteredAtOrigin <> b.Cell.IsCenteredAtOrigin) "933e7c94-c6e3-4149-a1aa-b89546ea38a9"
                    QMergeNode.ofNodes dom a b |> InMemoryMerge

        match winner dom first second with
        | Some w -> w
        | None   -> match first, second with
                    | NoNode,           b               -> b
                    | a,                NoNode          -> a
                    
                    | OutOfCoreNode a,  OutOfCoreNode b -> merge dom (a.Load()) (b.Load())
                    | OutOfCoreNode a,  b               -> merge dom (a.Load()) (b  )
                    | a,                OutOfCoreNode b -> merge dom (a  ) (b.Load())

                    | _ -> mergeToCommonRoot dom first second