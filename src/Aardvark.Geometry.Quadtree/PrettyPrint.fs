namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data

module PrettyPrint =

    type Pos = { X : int; Y : int }

    type Box = { Min : Pos; Max : Pos }

    module Box =

        let inline private minmap f xs = xs |> Seq.map f |> Seq.min
        let inline private maxmap f xs = xs |> Seq.map f |> Seq.max

        let union (boxes : Box seq) = {
            Min = { X = minmap (fun b -> b.Min.X) boxes; Y = minmap (fun b -> b.Min.Y) boxes }
            Max = { X = maxmap (fun b -> b.Max.X) boxes; Y = maxmap (fun b -> b.Max.Y) boxes }
        }

        let ofPos p = { Min = p; Max = p }

        let ofPositions ps = {
            Min = { X = minmap (fun b -> b.X) ps; Y = minmap (fun b -> b.Y) ps }
            Max = { X = maxmap (fun b -> b.X) ps; Y = maxmap (fun b -> b.Y) ps }
        }

    type HAlign = Left | Center | Right
    type VAlign = Top | Middle | Bottom

    type Format = {
        HAlign : HAlign
        VAlign : VAlign
        Bgcolor : C3b
    }
    
    let private colorOfC3b (c : C3b) = sprintf "rgb(%d,%d,%d)" c.R c.G c.B

    type Cells =
        | Text  of Position : Pos * Format : Format * Content : string
        | Group of Position : Pos * Format : Format * Label : string * Content : Cells list
    with
        member this.Format with get() = match this with
                                        | Text (_,f,_)
                                        | Group (_,f,_,_) -> f

    module Cells =
        
        let pos cells = match cells with 
                        | Text (p,_,_) 
                        | Group (p,_,_,_)  -> p

        let box cells = match cells with 
                        | Text  (p,_,_)    -> Box.ofPos p
                        | Group (_,_,_,xs) -> xs |> Seq.map (fun x -> pos x |> Box.ofPos) |> Box.union

        let private toMap cells =
            seq {
                match cells with
                | Text (pos,_,_)   -> yield (pos, cells)
                | Group (_,_,_,xs) -> yield! xs |> Seq.map (fun x -> (pos x, x))
            }
            |> Map.ofSeq

        let rec private toHtmlTable cells = seq {
            
            let m = cells |> toMap

            match cells with
            | Group (_,_,label,_) -> yield label
            | _ -> ()

            yield sprintf "<table style=\"background-color:%s\">" (colorOfC3b cells.Format.Bgcolor)
            let bb = box cells
            for y = bb.Min.Y to bb.Max.Y do
                yield "<tr>"
                for x = bb.Min.X to bb.Max.X do
                    yield "<td>"
                    match m |> Map.tryFind { X = x; Y = y } with
                    | None -> ()
                    | Some x -> match x with
                                | Text (_,_,s) -> yield s
                                | Group _ -> yield! toHtmlTable x
                    yield "</td>"
                yield "</tr>"
            yield "</table>"
            }

        let toHtml cells = seq {
            yield "<!DOCTYPE html>"
            yield "<html>"
            yield "<head>"
            yield "<style>"
            yield "table {"
            yield "  border: 1px solid black;"
            yield "  border-collapse: collapse;"
            yield "}"
            yield "th, td {"
            yield "  border: 1px dotted black;"
            yield "  padding: 8px;"
            yield "  text-align:left;"
            yield "  vertical-align: top;"
            yield "}"
            yield "</style>"
            yield "</head>"
            yield "<body>"
            yield! toHtmlTable cells
            yield "</body"
            yield "</html>"
        }

        let private colorOfLevel level : C3b =
            match level with
            |  2 -> C3b(255,128,255)
            |  1 -> C3b(255,255,128)
            |  0 -> C3b(255,128,128)
            | -1 -> C3b(128,255,128)
            | -2 -> C3b(128,128,255)
            | -3 -> C3b(128,255,255)
            | _ -> C3b.Gray80

        let ofQNodeRef<'a> (name : string) (pos : Pos) (f : Format) (def : Durable.Def) (qref : QNodeRef) : Cells =

            let rec foo (name : string) (pos : Pos) (f : Format) (xs : (Cell2d * 'a) list) =

                let cs = xs |> List.map fst
                let maxSampleExp = xs |> Seq.map (fun (c,_) -> c.Exponent) |> Seq.max
                
                let f = { f with Bgcolor = colorOfLevel maxSampleExp }

                let o = Cell2d(
                            cs |> Seq.map(fun c -> c.X) |> Seq.min,
                            cs |> Seq.map(fun c -> c.Y) |> Seq.min,
                            maxSampleExp
                            )

                let rootSamples = xs |> List.filter (fun (c,_) -> c.Exponent = maxSampleExp)
                let groups =
                    xs
                    |> List.filter (fun (c,_) -> c.Exponent <> maxSampleExp)
                    |> List.groupBy (fun (c,s) ->
                        if c.Exponent >= maxSampleExp then failwith "Invariant a4257653-d135-444c-9b97-7b87fa168734."
                        let k = getParentForLevel maxSampleExp c
                        { X = int(k.X - o.X); Y = int(k.Y - o.Y) }
                        )
                    |> Map.ofList

                let content = rootSamples |> List.map (fun (c,s) ->
                        let pos = { X = int(c.X-o.X); Y = int(c.Y-o.Y)}
                        let text = sprintf "(%d, %d, %d)<br/>%A" c.X c.Y c.Exponent s
                        match groups |> Map.tryFind pos with
                        | None   -> 
                            Text(pos, f, text)
                        | Some g ->
                            foo text pos f g
                    )
                
                Group(pos, f, name, content)


            
            let allSamples = qref |> Query.Full |> Seq.toList |> List.collect (fun x -> x.GetSamples<'a> def |> Array.toList)
            let result = foo name pos f allSamples
            result
        

    //let rec private p<'a> (def : Durable.Def) (indent : string) (root : QNodeRef) =

    //    match root.TryGetInMemory() with
    //    | None -> printfn "%s{}" indent
    //    | Some root ->
    //        let ind = indent + "  "
    //        printfn "%s{" indent

    //        let layer = (root.TryGetLayer def).Value :?> Layer<'a>

    //        let o = root.Mapping.BufferOrigin
    //        let size = root.Mapping.WindowSize
    //        for y = size.Y-1 downto 0 do
    //            printf "%s" ind
    //            for x = 0 to size.X-1 do
    //                let c = Cell2d(o.X + int64 x, o.Y + int64 y, o.Exponent)
    //                let s = layer.GetSample(Fail, c)
    //                printf "((%d, %d, %d), %A)" c.X c.Y c.Exponent s
    //            printfn ""

    //        match root.SubNodes with
    //        | None -> ()
    //        | Some ns -> for n in ns do 
    //                        match n with | NoNode -> () | _ -> p<'a> def ind n

    //        printfn "%s}" indent

    //        ()

    //let print<'a> name (def : Durable.Def) (rootRef : QNodeRef) =
    //    printfn ""
    //    printfn "%s:" name
    //    p<'a> def "" rootRef
        

