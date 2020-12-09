namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data
open System
open System.IO

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
    with
        static member Default = { HAlign=Left; VAlign=Top; Bgcolor=C3b.White }
    
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
            for y = bb.Max.Y downto bb.Min.Y do
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
            
            yield "html {"
            yield "  font-family: \"Consolas\";"
            yield "}"

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
            |  2 -> C3b(229,178,255)
            |  1 -> C3b(255,255,  0)
            |  0 -> C3b(255,127,127)
            | -1 -> C3b( 90,206, 90)
            | -2 -> C3b( 45,206,255)
            | -3 -> C3b(128,255,255)
            | _ -> C3b.Gray80

        let ofQNodeRef<'a> (name : string) (pos : Pos) (def : Durable.Def) (qref : QNodeRef) : Cells =

            let f = { HAlign=Left; VAlign=Top; Bgcolor=C3b.White }

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
                let topLevelCells = xs |> List.map (fun (c,_) -> getParentForLevel maxSampleExp c) |> List.distinct

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

                let content = topLevelCells |> List.map (fun topLevelCell ->
                        let rootSample = rootSamples |> List.tryFind(fun (c,_) -> c = topLevelCell)
                        match rootSample with
                        | Some (c,s) ->
                            let pos = { X = int(c.X-o.X); Y = int(c.Y-o.Y)}
                            let text = sprintf "(%d, %d, %d)<br/>%A" c.X c.Y c.Exponent s
                            match groups |> Map.tryFind pos with
                            | None   -> 
                                Text(pos, f, text)
                            | Some g ->
                                foo text pos f g
                        
                        | None ->
                            let pos = { X = int(topLevelCell.X-o.X); Y = int(topLevelCell.Y-o.Y)}
                            let text = ""// sprintf "<small>(%d, %d, %d)</small>" topLevelCell.X topLevelCell.Y topLevelCell.Exponent
                            match groups |> Map.tryFind pos with
                            | None   -> 
                                Text(pos, f, text)
                            | Some g ->
                                foo text pos f g
                    )
                            
                Group(pos, f, name, content)

                

            
            let allResults = qref |> Query.Full |> Seq.toList
            let allSamples = allResults |> List.collect (fun x -> x.GetSamples<'a> def |> Array.toList)
            let result = foo name pos f allSamples
            result



    let generateHtmlDebugView<'a> title def quadtrees =
        let content = quadtrees |> List.mapi (fun i x -> Cells.ofQNodeRef<'a> (sprintf "<h2>%s</h2>" (fst x)) {X=0;Y=i*2} def (snd x))
        Cells.Group({X=0;Y=0}, Format.Default, sprintf "<h1>%s</h1>" title, content) |> Cells.toHtml

    let showHtmlDebugView<'a> title def quadtrees =
        let html = generateHtmlDebugView<'a> title def quadtrees
        let now = DateTime.Now
        let filename = sprintf "quadtree_%04d-%02d-%02d-%02d-%02d-%02d.html" now.Year now.Month now.Day now.Hour now.Minute now.Second
        let filename = Path.Combine(Path.GetTempPath(), filename)
        File.WriteAllLines(filename, html)
        System.Diagnostics.Process.Start(@"cmd.exe ", @"/c " + filename) |> ignore
