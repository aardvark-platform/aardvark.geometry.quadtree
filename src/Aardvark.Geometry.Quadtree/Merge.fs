namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data

(*
    Merge.
*)

/// Which data dominates in merge operations.
type Dominance = 
    | FirstDominates 
    | SecondDominates 
    | MoreDetailedDominates
with
    member this.Flipped with get() =
        match this with 
        | FirstDominates        -> SecondDominates
        | SecondDominates       -> FirstDominates
        | MoreDetailedDominates -> MoreDetailedDominates
        

module Merge =

    type Buffer<'a> = {
        Semantic : Durable.Def
        Mapping : DataMapping
        Data : 'a[]
        }

    module private Buffer =

        let create (semantic : Durable.Def) (w : Box2l) (e : int) = {
            Semantic = semantic
            Mapping = DataMapping(Cell2d(w.Min, e), V2i w.Size, w)
            Data = Array.zeroCreate<'a> (int w.Size.X * int w.Size.Y)
        }

        let add (layer : Layer<'a>) (buffer : Buffer<'a>)  =
            let layerSampleExponent = layer.SampleExponent
            let layerWindow = layer.Mapping.Window
            let bufferSampleExponent = buffer.Mapping.BufferOrigin.Exponent
            let bufferData = buffer.Data
            let xMaxIncl = int layerWindow.SizeX - 1
            let yMaxIncl = int layerWindow.SizeY - 1

            // layer has SAME RESOLUTION as buffer -> direct copy
            if bufferSampleExponent = layerSampleExponent then
                for y = 0 to yMaxIncl do
                    for x = 0 to xMaxIncl do
                        let c = Cell2d(layerWindow.Min.X + int64 x, layerWindow.Min.Y + int64 y, layerSampleExponent)
                        let i = buffer.Mapping.GetBufferIndex c
                        let v = layer.GetSample(Fail, c)
                        bufferData.[i] <- v
                buffer

            // layer has PARENT RESOLUTION of buffer -> copy each sample 2x2 times
            elif bufferSampleExponent = layerSampleExponent - 1 then
                for y = 0 to yMaxIncl do
                    for x = 0 to xMaxIncl do
                        let cLayer = Cell2d(layerWindow.Min.X + int64 x, layerWindow.Min.Y + int64 y, layerSampleExponent)
                        let v = layer.GetSample(Fail, cLayer)
                        for qi = 0 to 3 do
                            let c = cLayer.GetQuadrant(qi)
                            let i = buffer.Mapping.GetBufferIndex c
                            bufferData.[i] <- v
                buffer

            else
                failwith "Invariant 7634631b-cd33-43e9-844b-52c2faba8ddf."

        let addMany (layers : Layer<'a> seq) (buffer : Buffer<'a>) =
            let mutable b = buffer
            for l in layers do b <- add l b
            b

        let toLayer (buffer : Buffer<'a>) =
            Layer(buffer.Semantic, buffer.Data, buffer.Mapping)

    type ComposedLayerResult = {
        Layer : ILayer
        ChildLayer : ILayer
    }

    open Buffer
    let private composeLayersInOrderTyped<'a> 
        (semantic : Durable.Def) (bounds : Cell2d) (sampleExponentResult : int) (targetWindowAtChildLevel : Box2l) 
        (layersInOrder : Layer<'a>[])
        : ComposedLayerResult =
        
        invariant (layersInOrder.Length > 0) "0ea261d0-e3e7-4edb-bc95-0fa1eb1d3a86"

        let e = sampleExponentResult
        let resampler = Resamplers.getTypedResamplerFor<'a> semantic 

        let layerChildLevel = 
            create semantic targetWindowAtChildLevel (e - 1)
            |> addMany layersInOrder
            |> toLayer

        let layerResultLevel = layerChildLevel.Resample ClampToEdge resampler bounds

        { Layer = layerResultLevel; ChildLayer = layerChildLevel }




    let private composeLayersInOrder (def : Durable.Def) (bounds : Cell2d) (sampleExponentResult : int) (targetWindowAtChildLevel : Box2l) 
                             (layersInOrder : ILayer[])
                             : ComposedLayerResult =

        let e = sampleExponentResult

        // ensure that all sample exponents are consistent ...
        invariant (layersInOrder |> Array.forall (fun x -> x.SampleExponent = e || x.SampleExponent = e - 1)) "5ec641c3-da8f-4757-bed0-fc5a527572fb"

        let compose convert =
            let layersInOrder' = layersInOrder |> Array.map convert
            composeLayersInOrderTyped def bounds sampleExponentResult targetWindowAtChildLevel layersInOrder'

        let someLayer = layersInOrder |> Seq.head
        match someLayer with

        | :? Layer<bool>    -> (compose (fun x -> x :?> Layer<bool>   ))
        | :? Layer<int8>    -> (compose (fun x -> x :?> Layer<int8>   ))
        | :? Layer<uint8>   -> (compose (fun x -> x :?> Layer<uint8>  ))
        | :? Layer<int16>   -> (compose (fun x -> x :?> Layer<int16>  ))
        | :? Layer<uint16>  -> (compose (fun x -> x :?> Layer<uint16> ))
        | :? Layer<int32>   -> (compose (fun x -> x :?> Layer<int32>  ))
        | :? Layer<uint32>  -> (compose (fun x -> x :?> Layer<uint32> ))
        | :? Layer<int64>   -> (compose (fun x -> x :?> Layer<int64>  ))
        | :? Layer<uint64>  -> (compose (fun x -> x :?> Layer<uint64> ))
        | :? Layer<float>   -> (compose (fun x -> x :?> Layer<float>  ))
        | :? Layer<float32> -> (compose (fun x -> x :?> Layer<float32>))
        | :? Layer<decimal> -> (compose (fun x -> x :?> Layer<decimal>))
        
        | :? Layer<V2d>     -> (compose (fun x -> x :?> Layer<V2d>    ))
        | :? Layer<V2f>     -> (compose (fun x -> x :?> Layer<V2f>    ))
        | :? Layer<V2i>     -> (compose (fun x -> x :?> Layer<V2i>    ))
        | :? Layer<V2l>     -> (compose (fun x -> x :?> Layer<V2l>    ))

        | :? Layer<V3d>     -> (compose (fun x -> x :?> Layer<V3d>    ))
        | :? Layer<V3f>     -> (compose (fun x -> x :?> Layer<V3f>    ))
        | :? Layer<V3i>     -> (compose (fun x -> x :?> Layer<V3i>    ))
        | :? Layer<V3l>     -> (compose (fun x -> x :?> Layer<V3l>    ))

        | :? Layer<V4d>     -> (compose (fun x -> x :?> Layer<V4d>    ))
        | :? Layer<V4f>     -> (compose (fun x -> x :?> Layer<V4f>    ))
        | :? Layer<V4i>     -> (compose (fun x -> x :?> Layer<V4i>    ))
        | :? Layer<V4l>     -> (compose (fun x -> x :?> Layer<V4l>    ))

        | :? Layer<C3b>     -> (compose (fun x -> x :?> Layer<C3b>    ))
        | :? Layer<C3f>     -> (compose (fun x -> x :?> Layer<C3f>    ))
        | :? Layer<C4b>     -> (compose (fun x -> x :?> Layer<C4b>    ))
        | :? Layer<C4f>     -> (compose (fun x -> x :?> Layer<C4f>    ))

        | _ -> failwith <| sprintf "Unsupported layer type %A. Invariant d31887e6-6c87-4b5a-87d7-0cab1fe9ec55." someLayer




    /// all parts must have same semantic ...
    let private createLayer (bounds : Cell2d) (def : Durable.Def) (domination : Dominance)
                            (l1o : ILayer option) (slo1 : array<ILayer option>)
                            (l2o : ILayer option) (slo2 : array<ILayer option>) 
                            : ComposedLayerResult =

        invariant (domination <> MoreDetailedDominates) "58d9ee5a-ba81-4e45-b03f-4e2dd780175a"
        invariant (l1o.IsNone || l1o.Value.Def = def) "c8931d12-5472-4f11-ab64-b5c49e1ebbb5"
        invariant (l2o.IsNone || l2o.Value.Def = def) "c9e6f48e-763c-482a-8cb2-909490b978f2"
        invariant (slo1 |> Seq.choose id |> Seq.forall (fun x -> x.Def = def)) "5739daef-3c13-4409-9ba0-78a5a311634c"
        invariant (slo2 |> Seq.choose id |> Seq.forall (fun x -> x.Def = def)) "c2aaadbd-86aa-4115-9d04-42e50ab0f414"

        // ensure that all parts have consistent sample exponents ...
        let f0 = Option.map (fun (x : ILayer) -> x.SampleExponent)
        let f1 = Option.map (fun (x : ILayer) -> x.SampleExponent + 1)
        let sampleExponents = [l1o |> f0; l2o |> f0]|> Seq.append (slo1 |> Seq.map f1) |> Seq.append (slo2 |> Seq.map f1) |> Seq.choose id |> Seq.distinct |> Seq.toArray // order does not matter
        //let sampleExponents = [l1o |> f0; l2o |> f0] |> Seq.choose id |> Seq.distinct |> Seq.toArray // order does not matter
        invariantm (sampleExponents.Length = 1) (sprintf "Inconsistent sample exponents. %A" sampleExponents) "b8269ff8-55f6-4ddc-aacc-7a584d46f598"

        // get sample exponent (for result layer)
        let sampleExponentAtResultLevel = sampleExponents |> Array.exactlyOne

        // compute result 
        let getWinAtChildLevel (lo : ILayer option) = match lo with | Some l -> l.SampleWindowAtChildLevel | None -> Box2l.Invalid
        let w1 = l1o |> getWinAtChildLevel
        let w2 = l2o |> getWinAtChildLevel
        let ws1 = slo1 |> Seq.choose id |> Seq.map (fun x -> x.SampleWindow) |> Box2l
        let ws2 = slo2 |> Seq.choose id |> Seq.map (fun x -> x.SampleWindow) |> Box2l
        
        let finalWindowAtChildLevel = Box2l(w1, w2, ws1, ws2)
        
        let boundsAtChildLevel = bounds.GetBoundsForExponent(sampleExponentAtResultLevel - 1)
        invariantm (boundsAtChildLevel.Contains finalWindowAtChildLevel) 
                   (sprintf "Final window (%A) is out of bounds (%A)." finalWindowAtChildLevel boundsAtChildLevel) 
                   "d3a934ed-5e51-45e1-ad97-a28ee8ca575b"

        let layersInOrder = 
            match domination with
            | FirstDominates        -> seq { yield l2o; yield! slo2; yield l1o; yield! slo1 } |> Seq.choose id |> Seq.toArray
            | SecondDominates       -> seq { yield l1o; yield! slo1; yield l2o; yield! slo2 } |> Seq.choose id |> Seq.toArray
            | MoreDetailedDominates ->
                failwith "MoreDetailedDominates is not allowed here. Invariant bcb06b5b-5d28-4442-b388-05b29139743b." 

        composeLayersInOrder def bounds sampleExponentAtResultLevel finalWindowAtChildLevel layersInOrder

    /// all parts must have same layer set ...
    let private createLayers (bounds : Cell2d) (domination : Dominance)
                             (l1o : ILayer[] option) (slo1 : array<ILayer[] option>)
                             (l2o : ILayer[] option) (slo2 : array<ILayer[] option>) : ComposedLayerResult[] =

        invariant (domination <> MoreDetailedDominates) "de263f01-fd18-41d5-b061-f301aed7cf4e"

        // ensure that all parts have the same layers ...
        let getLayerSemantics (x : ILayer[]) = x |> Seq.map (fun x -> x.Def) |> Set.ofSeq
        let semsets = [l1o; l2o] |> Seq.append slo1 |> Seq.append slo2 |> Seq.choose id |> Seq.map getLayerSemantics |> Seq.distinct |> Seq.toArray // order does not matter
        invariantm (semsets.Length = 1) 
                   (sprintf "Parts have different layers. %A." semsets) "53fea9ea-521b-4eaa-85d4-07169f2d3f26"

        // group by semantic
        let toLayerMap (ls : ILayer[] option) = match ls with | Some ls -> ls |> Seq.map (fun l -> (l.Def, l)) |> Map.ofSeq | None -> Map.empty
        let l1om  = l1o  |>           toLayerMap
        let slo1m = slo1 |> Array.map toLayerMap
        let l2om  = l2o  |>           toLayerMap
        let slo2m = slo2 |> Array.map toLayerMap

        let semantics = semsets |> Seq.exactlyOne
        let result = 
            semantics 
            |> Seq.map (fun sem ->
                let f = Map.tryFind sem
                let arg0 = l1om  |>           f
                let arg1 = slo1m |> Array.map f
                let arg2 = l2om  |>           f
                let arg3 = slo2m |> Array.map f
                let r = createLayer bounds sem domination arg0 arg1 arg2 arg3
                r
                )
            |> Seq.toArray

        result

    /// Creates new node from two nodes.
    let rec private create (cell : Cell2d) (splitLimitExponent : int) (domination : Dominance) 
                       (n1o : QNode option) (sno1 : QNode option[])
                       (n2o : QNode option) (sno2 : QNode option[]) : QNodeRef =

        invariant (domination <> MoreDetailedDominates) "c134db85-643a-420d-8205-a795a7bce5ca"

        // ensure that optional root nodes coincide with specified root cell
        let check s (n : QNode option) = 
            match n with 
            | None -> ()
            | Some n -> invariantm (n.Cell = cell) 
                            (sprintf "%s node (%A) does not coincide with specified root (%A)." s n.Cell cell) 
                            "3294c70f-7a89-4f75-83d5-ead39cad9ac6"
        check "1st" n1o
        check "2nd" n2o

        // ensure that all subnodes are correctly placed ... 
        for qi = 0 to 3 do
            let q = cell.GetQuadrant(qi)
            invariant (match sno1.[qi] with | Some n -> n.Cell = q | None -> true) "764bc804-3f9f-4031-9ec0-7ffe04531328"
            invariant (match sno2.[qi] with | Some n -> n.Cell = q | None -> true) "50d449be-0a7e-47bc-a3dc-0cc5bfb4fc59"

        // ensure that all subnodes have same resolution and split limit ...
        invariantm 
            ((sno1 |> Seq.append sno2 |> Seq.choose id |> Seq.map (fun n -> (n.SampleExponent, n.SplitLimitExponent))) |> Seq.distinct |> Seq.length < 2) // order does not matter
            "Subnodes have different resolution or split limit." "018b42e9-34a5-4791-94d2-374d7e246f6c"
        
        // ..........
        let parts1 = seq { yield n1o; yield! sno1 } |> Seq.choose id |> Seq.toArray
        let parts2 = seq { yield n2o; yield! sno2 } |> Seq.choose id |> Seq.toArray

        let ebb1 = parts1 |> Seq.map(fun x -> x.ExactBoundingBox) |> Box2d
        let ebb2 = parts2 |> Seq.map(fun x -> x.ExactBoundingBox) |> Box2d

        //if (n1o.IsSome && n2o.IsSome) && ebb1.Intersects(ebb2) then
        
        //    if domination = FirstDominates  && n1o.Value.SampleExponent > n2o.Value.SampleExponent &&  ebb1.Contains(ebb2) |> not then
        //        failwith "foo 1"

        //    if domination = SecondDominates && n2o.Value.SampleExponent > n1o.Value.SampleExponent &&  ebb2.Contains(ebb1) |> not then
        //        failwith "foo 2"




        // compute node layers
        let l1o = n1o |> Option.map (fun n -> n.Layers)
        let slo1 = sno1 |> Array.map (Option.map (fun n -> n.Layers))
        let l2o = n2o |> Option.map (fun n -> n.Layers)
        let slo2 = sno2 |> Array.map (Option.map (fun n -> n.Layers))
        let layerResults = createLayers cell domination l1o slo1 l2o slo2
        let layers = layerResults |> Array.map (fun x -> x.Layer)

        // merge subnodes
        let subnodes = Array.zeroCreate 4
        let mutable hasSubNodes = false
        for i = 0 to 3 do
            let n = match sno1.[i], sno2.[i] with
                    | None,   None   -> None
                    | Some a, None
                    | None,   Some a ->
                        hasSubNodes <- true
                        Some a
                        //let sc = cell.GetQuadrant(i)
                        //let scwin = sc.GetBoundsForExponent(a.SampleExponent)
                        //let ls = layerResults |> Array.map (fun lr -> lr.ChildLayer.WithWindow(scwin).Value)
                        //let a2 = QNode(a.Cell, a.SplitLimitExponent, ls)
                        //Some a2
                    | Some a, Some b ->
                        failwith "Collision. Error fc1a216d-65e8-4bb0-9d4b-b355f548a830."
                        //invariant (a.Cell = b.Cell) "72d1f6cf-a7a3-41ab-bc50-216dcf23f770"
                        //let sn1 = match a.SubNodes with | None -> Array.zeroCreate 4 | Some xs -> xs |> Array.map (fun x -> x.TryGetInMemory())
                        //let sn2 = match b.SubNodes with | None -> Array.zeroCreate 4 | Some xs -> xs |> Array.map (fun x -> x.TryGetInMemory())
                        //(create a.Cell splitLimitExponent domination (Some a) sn1 (Some b) sn2).TryGetInMemory()

            subnodes.[i] <- n

        // result
        let ebb = [n1o; n2o] |> Seq.append sno1 |> Seq.append sno2 |> Seq.choose (Option.map(fun n -> n.ExactBoundingBox)) |> Box2d // order does not matter
        if hasSubNodes then
            QNode(ebb, cell, splitLimitExponent, layers, subnodes) |> InMemoryNode
        else
            QNode(ebb, cell, splitLimitExponent, layers) |> InMemoryNode


    /// Merge nodes, where one node is a subnode of the other, or both nodes are the same.
    let rec private mergeOverlappingNodes (domination : Dominance) (firstRef : QNodeRef) (secondRef : QNodeRef) : QNodeRef =

        match firstRef.TryGetInMemory(), secondRef.TryGetInMemory() with
        | None,    None    -> NoNode
        | Some _,  None    -> firstRef
        | None,    Some _  -> secondRef
        | Some n1, Some n2 ->

            invariant (n1.SplitLimitExponent = n2.SplitLimitExponent) "6b6b74c6-f8dc-4177-93ba-99bea3328332"

            invariantm (QNode.Overlap(n1, n2)) 
                (sprintf "Nodes must overlap. First %A. Second %A" n1.Cell n2.Cell)
                "740fb9b2-de34-4c83-85fd-f4149da25c83"


            if domination = FirstDominates && n1.ExactBoundingBox.Contains(n2.ExactBoundingBox) then
                firstRef  // first fully occludes second
            
            elif domination = SecondDominates && n2.ExactBoundingBox.Contains(n1.ExactBoundingBox) then
                secondRef // second fully occludes first

            else


                // common root cell
                let rc = Cell2d(Box2d(n1.Cell.BoundingBox, n2.Cell.BoundingBox))
                if rc.IsCenteredAtOrigin then
                    failwith "Centered cells are currently not supported. Error 20364c6b-c265-4e08-be21-fc69a0f96758."

                let (n1o, sno1) =
                    if rc = n1.Cell then
                        invariant (rc.Exponent >= n2.Cell.Exponent)  "16e4749b-2f55-40a1-90d3-4f07701ae146"
                
                        (Some n1, match n1.SubNodes with
                                  | None    -> Array.create 4 None
                                  | Some ns -> ns |> Array.map (fun n -> n.TryGetInMemory())
                                  )

                    else
                        invariant (rc = n2.Cell)                    "51a10ae0-1cdf-47de-ab56-8845a801e141"
                        invariant (rc.Exponent > n1.Cell.Exponent)  "58773c53-749a-478e-9f24-12bc701c4a10"
                
                        let sno1 = Array.create 4 None
                        let qi1 = rc.GetQuadrant(n1.Cell).Value
                        sno1.[qi1] <- (firstRef |> QNode.extendUpTo (rc.GetQuadrant(qi1))).TryGetInMemory().Value |> Some

                        (None, sno1)

                let (n2o, sno2) = 
                    if rc = n2.Cell then
                        invariant (rc.Exponent >= n1.Cell.Exponent)  "4f24c550-87aa-4772-9463-ea886f1bb81e"
                
                        (Some n2, match n2.SubNodes with
                                  | None    -> Array.create 4 None
                                  | Some ns -> ns |> Array.map (fun n -> n.TryGetInMemory())
                                  )

                    else
                        invariant (rc = n1.Cell)                    "1f9777be-9ea0-4117-beb3-0630c2fd8094"
                        invariant (rc.Exponent > n2.Cell.Exponent)  "f263af3f-d77e-4820-b03c-ef9bd65d089a"
                
                        let sno2 = Array.create 4 None
                        let qi2 = rc.GetQuadrant(n2.Cell).Value
                        sno2.[qi2] <- (secondRef |> QNode.extendUpTo (rc.GetQuadrant(qi2))).TryGetInMemory().Value |> Some

                        (None, sno2)

                // handle sno collisions
                for qi = 0 to 3 do
                    match sno1.[qi], sno2.[qi] with
                    | Some a, Some b ->
                        let m = (mergeOverlappingNodes domination (InMemoryNode a) (InMemoryNode b)).TryGetInMemory()
                        sno1.[qi] <- None
                        sno2.[qi] <- None
                        match domination with
                        | FirstDominates -> sno1.[qi] <- m
                        | SecondDominates -> sno2.[qi] <- m
                        | MoreDetailedDominates -> failwith "MoreDetailedDominates mode is currently not supported. Error e23a523f-b3e2-4981-b362-948fcbce15da."
                    | _ -> ()

                // ....
                if n1.ExactBoundingBox.Intersects(n2.ExactBoundingBox) then
                    
                        if domination = FirstDominates  && n1.SampleExponent > n2.SampleExponent &&  n1.ExactBoundingBox.Contains(n2.ExactBoundingBox) |> not then
                            failwith "foo 1"

                        if domination = SecondDominates && n2.SampleExponent > n1.SampleExponent &&  n2.ExactBoundingBox.Contains(n1.ExactBoundingBox) |> not then
                            failwith "foo 2"

                create rc n1.SplitLimitExponent domination n1o sno1 n2o sno2

    
    /// Merge nodes that do not overlap.
    let private mergeNonOverlappingNodes (domination : Dominance) (nr1 : QNodeRef) (nr2 : QNodeRef) : QNodeRef =
        match nr1.TryGetInMemory(), nr2.TryGetInMemory() with
        | None,    None    -> NoNode
        | Some _,  None    -> nr1
        | None,    Some _  -> nr2
        | Some n1, Some n2 ->

            invariant (n1.SplitLimitExponent = n2.SplitLimitExponent) "5a057fc7-fe39-4c6e-a759-1a49054c34e7"

            invariantm (QNode.Overlap(n1, n2) |> not)
                (sprintf "Nodes must not overlap. First %A. Second %A" n1.Cell n2.Cell)
                "6ada6e09-ef33-4daf-9b0c-4c6dd30f0087"

            // common root cell
            let rc = Cell2d(Box2d(n1.Cell.BoundingBox, n2.Cell.BoundingBox))
            if rc.IsCenteredAtOrigin then
                failwith "Centered cells are currently not supported. Error f312f2bd-4464-4156-973f-21cdd3b97886."
            invariant (rc.Contains n1.Cell && rc.Exponent > n1.Cell.Exponent) "e93e27b4-f9a3-484f-a3fa-6e28cb4e803b"
            invariant (rc.Contains n2.Cell && rc.Exponent > n2.Cell.Exponent) "6e4b0a9a-c059-4ba3-8fde-029482326669"

            // quadrant index for n1 and n2
            let qi1 = rc.GetQuadrant(n1.Cell).Value
            let qi2 = rc.GetQuadrant(n2.Cell).Value

            // init two sets of subnodes for root cell
            let sno1 = Array.create 4 None
            sno1.[qi1] <- (nr1 |> QNode.extendUpTo (rc.GetQuadrant(qi1))).TryGetInMemory().Value |> Some
            let sno2 = Array.create 4 None
            sno2.[qi2] <- (nr2 |> QNode.extendUpTo (rc.GetQuadrant(qi2))).TryGetInMemory().Value |> Some

            // create root node from two sets of subnodes
            create rc n1.SplitLimitExponent domination None sno1 None sno2


    /// Immutable merge.
    let merge (outOfCore : bool) (domination : Dominance) (firstRef : QNodeRef) (secondRef : QNodeRef) : QNodeRef =

        if domination = MoreDetailedDominates then failwith "MoreDetailedDominates mode is currently not supported. Error 37f67384-7bdc-44f0-b31d-6a347cb5b0c2."

        match firstRef.TryGetInMemory(), secondRef.TryGetInMemory() with
        | None,       None        -> NoNode
        | Some _,     None        -> firstRef
        | None,       Some _      -> secondRef
        | Some first, Some second ->

            if first.SplitLimitExponent <> second.SplitLimitExponent then
                failwith "Cannot merge quadtrees with different split limits. Error 6222eb6b-a7aa-43c1-9323-e28d6275696b."

            let result =
                if QNode.Overlap(first, second) then
                    // one quadtree contains the other (there is no partial overlap in quadtrees)
                    mergeOverlappingNodes    domination firstRef secondRef
                else
                    // quadtrees are completely separated
                    mergeNonOverlappingNodes domination firstRef secondRef

            result
