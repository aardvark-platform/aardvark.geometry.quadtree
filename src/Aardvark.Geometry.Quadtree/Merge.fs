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

        invariant (domination <> MoreDetailedOrFirst && domination <> MoreDetailedOrSecond) "58d9ee5a-ba81-4e45-b03f-4e2dd780175a"
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
            | FirstDominates       -> seq { yield l2o; yield! slo2; yield l1o; yield! slo1 } |> Seq.choose id |> Seq.toArray
            | SecondDominates      -> seq { yield l1o; yield! slo1; yield l2o; yield! slo2 } |> Seq.choose id |> Seq.toArray
            | MoreDetailedOrFirst  -> failwith "MoreDetailedOrFirst is not allowed here. Invariant bcb06b5b-5d28-4442-b388-05b29139743b."
            | MoreDetailedOrSecond -> failwith "MoreDetailedOrSecond is not allowed here. Invariant 04ddaac5-4daf-4a0a-a0be-7664a1ae7ac6."

        composeLayersInOrder def bounds sampleExponentAtResultLevel finalWindowAtChildLevel layersInOrder

    /// all parts must have same layer set ...
    let private createLayers (bounds : Cell2d) (domination : Dominance)
                             (l1o : ILayer[] option) (slo1 : array<ILayer[] option>)
                             (l2o : ILayer[] option) (slo2 : array<ILayer[] option>) : ComposedLayerResult[] =

        invariant (domination <> MoreDetailedOrFirst && domination <> MoreDetailedOrSecond) "de263f01-fd18-41d5-b061-f301aed7cf4e"

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
                       (n2o : QNode option) (sno2 : QNode option[])
                       (strangerThings : bool)
                       : QNodeRef =

        invariant (domination <> MoreDetailedOrFirst && domination <> MoreDetailedOrSecond) "c134db85-643a-420d-8205-a795a7bce5ca"

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
                        if strangerThings then
                            let sc = cell.GetQuadrant(i)
                            let scwin = sc.GetBoundsForExponent(a.SampleExponent)
                            let ls = layerResults |> Array.map (fun lr -> lr.ChildLayer.WithWindow(scwin).Value)
                            let a2 = QNode(a.Cell, a.SplitLimitExponent, ls, a.SubNodes)
                            Some a2
                        else
                            Some a
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

            invariantm (QNode.CellOverlap(n1, n2)) 
                (sprintf "Nodes must overlap. First %A. Second %A" n1.Cell n2.Cell)
                "740fb9b2-de34-4c83-85fd-f4149da25c83"


            if domination = FirstDominates && n1.ExactBoundingBox.Contains(n2.ExactBoundingBox) then
                firstRef  // first fully occludes second
            
            elif domination = SecondDominates && n2.ExactBoundingBox.Contains(n1.ExactBoundingBox) then
                secondRef // second fully occludes first

            else


                // common root cell
                let rc = n1.Cell.Union(n2.Cell)
                invariant (not rc.IsCenteredAtOrigin) "20364c6b-c265-4e08-be21-fc69a0f96758"

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
                        | FirstDominates       -> sno1.[qi] <- m
                        | SecondDominates      -> sno2.[qi] <- m
                        | MoreDetailedOrFirst  -> failwith "MoreDetailedOrFirst is not allowed here. Invariant d7caca1a-9e00-4f9e-b20b-f4ca8865005a."
                        | MoreDetailedOrSecond -> failwith "MoreDetailedOrSecond is not allowed here. Invariant 732e7657-9db4-49b5-8a96-d448318a7505."
                    | _ -> ()

                // .... check if dominating lower-resolution layer needs to be pushed through to only partially overlapping higher-res sublayer
                let mutable strangerThings = false
                if n1.ExactBoundingBox.Intersects(n2.ExactBoundingBox) then
                        
                        let rec getMinSampleExponentRec (foo : QNode) =
                            match foo.SubNodes with
                            | None -> foo.SampleExponent
                            | Some xs -> xs |> Seq.choose (fun x -> x.TryGetInMemory()) |> Seq.map getMinSampleExponentRec |> Seq.min

                        let se1 = getMinSampleExponentRec n1
                        let se2 = getMinSampleExponentRec n2
                        if domination = FirstDominates  && se1 > se2 &&  n1.ExactBoundingBox.Contains(n2.ExactBoundingBox) |> not then
                            strangerThings <- true

                        if domination = SecondDominates && se2 > se1 &&  n2.ExactBoundingBox.Contains(n1.ExactBoundingBox) |> not then
                            strangerThings <- true

                create rc n1.SplitLimitExponent domination n1o sno1 n2o sno2 strangerThings


    let private resolveCentered (domination : Dominance) (firstRef : QNodeRef) (secondRef : QNodeRef) =
        failwith "resolve centered"


    
    /// Merge nodes that do not overlap.
    let private mergeNonOverlappingNodes (dominance : Dominance) (nr1 : QNodeRef) (nr2 : QNodeRef) : QNodeRef =
        match nr1.TryGetInMemory(), nr2.TryGetInMemory() with
        | None,    None    -> NoNode
        | Some _,  None    -> nr1
        | None,    Some _  -> nr2
        | Some n1, Some n2 ->
        
            invariant (n1.DoesNotOverlap(n2))                                   "6ada6e09-ef33-4daf-9b0c-4c6dd30f0087"
            invariant (n1.SplitLimitExponent = n2.SplitLimitExponent)           "5a057fc7-fe39-4c6e-a759-1a49054c34e7"

            // common root cell
            let rc = n1.Cell.Union(n2.Cell)
            invariant (rc.Contains n1.Cell && rc.Exponent > n1.Cell.Exponent)   "e93e27b4-f9a3-484f-a3fa-6e28cb4e803b"
            invariant (rc.Contains n2.Cell && rc.Exponent > n2.Cell.Exponent)   "6e4b0a9a-c059-4ba3-8fde-029482326669"

            // quadrant index for n1 and n2
            let qi1 = rc.GetQuadrant(n1.Cell).Value
            let qi2 = rc.GetQuadrant(n2.Cell).Value

            // init two sets of subnodes for root cell
            let sno1 = Array.create 4 None
            sno1.[qi1] <- (nr1 |> QNode.extendUpTo (rc.GetQuadrant(qi1))).TryGetInMemory().Value |> Some
            let sno2 = Array.create 4 None
            sno2.[qi2] <- (nr2 |> QNode.extendUpTo (rc.GetQuadrant(qi2))).TryGetInMemory().Value |> Some

            // create root node from two sets of subnodes
            create rc n1.SplitLimitExponent dominance None sno1 None sno2 false






























    

    type CellRelation =
        | DisjointCells
        | PartiallyOverlappingCells
        | IdenticalCells
        | FirstCellContainsSecond
        | SecondCellContainsFirst

    let cellRelationOf (a : Cell2d) (b : Cell2d) =
        if a.Intersects(b) then
            match a.Contains(b), b.Contains(a) with
            | false, false -> PartiallyOverlappingCells
            | false, true  -> SecondCellContainsFirst
            | true, false  -> FirstCellContainsSecond
            | true, true   -> IdenticalCells
        else
            DisjointCells



    /// Non-centered tree.
    type Tree =
        | Leaf of QNode
        | Tree of QNode

    type Children = Children of root : Cell2d * subnodes : Tree option[]

    module Tree =

        let ofQNode (root : QNode) : Tree =
            invariant (not(root.Cell.IsCenteredAtOrigin)) "40e61038-1cda-4b27-8cc8-f23c13878b46"
            if root.IsLeafNode then Leaf(root) else Tree(root)

        let inline qnode n = match n with | Leaf n | Tree n -> n
        let inline cell n = match n with | Leaf n | Tree n -> n.Cell
        let inline isLeaf n = (qnode n).IsLeafNode
        let inline isCentered n = (cell n).IsCenteredAtOrigin

        let tryGetChildren n : Children option =
            match (qnode n).SubNodes with
            | Some ns -> Children(cell n, ns |> Array.map(fun n -> n.TryGetInMemory()) |> Array.map(Option.map(ofQNode))) |> Some
            | None    -> None

    type Tree with
        member this.Cell with get() = Tree.cell this



    /// Centered tree.
    type CenterTree =
        | CenterLeaf of QNode 
        | CenterTree of QNode

    type CenterChildren = CenterChildren of root : Cell2d * subnodes : Tree option[]

    module CenterTree =

        let ofQNode (root : QNode) : CenterTree =
            invariant (root.Cell.IsCenteredAtOrigin) "1553b069-5e00-429f-932e-e75a7fe64ede"
            if root.IsLeafNode then CenterLeaf(root) else CenterTree(root)

        let inline qnode n = match n with | CenterLeaf n | CenterTree n -> n
        let inline cell n = match n with | CenterLeaf n | CenterTree n -> n.Cell
        let inline isLeaf n = (qnode n).IsLeafNode
        let inline isCentered n = (cell n).IsCenteredAtOrigin

        let tryGetChildren n : CenterChildren option =
            match (qnode n).SubNodes with
            | Some ns -> CenterChildren(cell n, ns |> Array.map(fun n -> n.TryGetInMemory()) |> Array.map(Option.map(Tree.ofQNode))) |> Some
            | None    -> None

    type CenterTree with
        member this.Cell with get() = CenterTree.cell this

    /// Centered or non-centered tree.
    type AnyTree =
        | NonCentered of Tree
        | Centered of CenterTree

    module AnyTree =

        let ofQNode (root : QNode) : AnyTree =
            if root.Cell.IsCenteredAtOrigin then
                root |> CenterTree.ofQNode |> Centered
            else 
                root |> Tree.ofQNode |> NonCentered

        let inline qnode n = match n with | NonCentered n -> Tree.qnode n | Centered n    -> CenterTree.qnode n
        let inline cell n = (qnode n).Cell
        let inline isLeaf n = (qnode n).IsLeafNode
        let inline isCentered n = (cell n).IsCenteredAtOrigin

        //let inline children n : Children option =
        //    match (qnode n).SubNodes with
        //    | Some ns -> ns |> Array.map(fun n -> n.TryGetInMemory()) |> Array.map(Option.map(Tree.ofQNode)) |> Some
        //    | None    -> None

        let inline ebbox n = (qnode n).ExactBoundingBox

        let inline ebboxContains first second = (ebbox first).Contains(ebbox second)


    type SubtreeRelation =
        /// non-centered child (e-1)
        | DirectChild    of child : Tree * qi : int
        /// non-centered child (e-n where n>1)
        | IndirectChild  of child : Tree * qi : int
        /// centered parent Cell2d(e) -> centered child Cell2d(e-1)
        | NestedDirect   of child : CenterTree
        /// centered parent Cell2d(e) -> centered child Cell2d(e-n where n>1)
        | NestedIndirect of child : CenterTree
        /// child is not in a subtree relation with respect to root
        | InvalidSubtree of child : AnyTree * root : Cell2d

    module SubtreeRelation =

        let ofTree (root : Cell2d) (child : Tree) : SubtreeRelation =
            let c = child.Cell 
            if root.Exponent > c.Exponent then
                let qi = root.GetQuadrant(c)
                if qi.HasValue then
                    let isDirect = c.Exponent + 1 = root.Exponent
                    if isDirect then DirectChild(child, qi.Value) else IndirectChild(child, qi.Value)
                else
                    InvalidSubtree(NonCentered child, root)
            else
                InvalidSubtree(NonCentered child, root)

        let ofAnyTree (root : Cell2d) (child : AnyTree) : SubtreeRelation =
            let c = AnyTree.cell child
            if root.Exponent > c.Exponent then
                let isDirect = c.Exponent + 1 = root.Exponent
                let qi = root.GetQuadrant(c)
                match root.IsCenteredAtOrigin, qi.HasValue, child with
                | _,     true,  NonCentered x -> if isDirect then DirectChild(x, qi.Value) else IndirectChild(x, qi.Value)
                | true,  false, Centered    x -> if isDirect then NestedDirect(x)          else NestedIndirect(x)
                | _ -> InvalidSubtree(child, root)
            else
                InvalidSubtree(child, root)
            

    type MergeRelation =
        | MergeWinner       of winner : AnyTree * loser : AnyTree
        | MergeDisjoint     of commonRootCell : Cell2d * first : SubtreeRelation * second : SubtreeRelation
        | MergeCollision    of first : AnyTree * second : AnyTree * dominance : Dominance
        | MergeSubtree      of parent : AnyTree * child : SubtreeRelation * dominance : Dominance
        | MergeNested       of centeredLarge : CenterTree * centeredSmall : CenterTree * dominance : Dominance

    module MergeRelation =

        let rec create (dominance : Dominance) (first : AnyTree) (second : AnyTree) : MergeRelation =

            let c1 = AnyTree.cell first
            let c2 = AnyTree.cell second

            let failWithConfiguration error =
                sprintf "Configuration (first=%A, second=%A, dominance=%A). Error %s." 
                        c1 c2 dominance error |> failwith

            match cellRelationOf c1 c2 with
        
            | DisjointCells ->
                let rc = c1.Union(c2)
                let rel = SubtreeRelation.ofTree rc
                match first, second with
                | NonCentered a, NonCentered b -> MergeDisjoint (rc, rel a, rel b)
                | _ -> failwith "todo"
        
            | PartiallyOverlappingCells ->
                // can happen with one centered cell and another non-centered cell
                failWithConfiguration "b0cd642b-c407-45ce-a871-bec728161354"
        
            | SecondCellContainsFirst ->
                create dominance.Flipped second first
                
            | FirstCellContainsSecond ->
                if dominance = FirstDominates && (AnyTree.ebbox first).Contains((AnyTree.ebbox second)) then
                    MergeWinner (first, second)
                else
                    match first, second with
                    | Centered    _, NonCentered _
                    | NonCentered _, NonCentered _ -> MergeSubtree (first, second |> SubtreeRelation.ofAnyTree c1, dominance)
                    | Centered    a, Centered    b -> MergeNested  (a, b, dominance)
                    | NonCentered a, Centered    b -> failwith "todo: create NonCentered a, Centered b"

            | IdenticalCells ->
                invariant (c1 = c2) "8c6566b8-66b8-492d-8e30-05fefd210cb7"
                if dominance = FirstDominates && (AnyTree.ebboxContains first second) then
                    MergeWinner (first, second)
                elif dominance = SecondDominates && (AnyTree.ebboxContains second first) then
                    MergeWinner (second, first)
                else
                    MergeCollision (first, second, dominance)
       
    
    let private ofIndexedSubnodes (root : Cell2d) (children : (int * Tree) list) : (Cell2d * Tree option[]) =
        invariant (children.Length > 0 && children.Length <= 4)                 "0cf7c961-6e82-4117-935c-b83043fc0465"
        let ns = Array.create 4 None
        for (i, n) in children do
            invariant (i >= 0 && i < 4)                                         "bfb84957-3024-443e-b026-3e7db0f45c74"
            invariant (ns.[i].IsNone)                                           "1ccd65be-6c4e-4135-af5d-d25af3aed1da"
            if root.IsCenteredAtOrigin then
                let c = n.Cell
                invariant (c.Exponent + 1 = root.Exponent && c.TouchesOrigin)   "bb213b45-6f28-4ca4-9254-f2cebbf12b00"
            else
                invariant ((Tree.cell n).Parent = root)                         "cd9bd921-74aa-4719-89f5-ca28a7bd40f5"
            ns.[i] <- Some n
        (root, ns)

    
    module Children =

        let ofIndexedSubnodes (root : Cell2d) (children : (int * Tree) list) : Children =
            invariant (not root.IsCenteredAtOrigin) "a89439d5-1490-4120-b451-50be51a0e262"
            ofIndexedSubnodes root children |> Children

    type LeafNode       = LeafNode of node : AnyTree

    module LeafNode =

        let ofTree (n : Tree) : LeafNode =
            invariant (Tree.isLeaf n) "b4b2cda5-5ea8-4e0a-883d-c422a6afd760"
            n |> NonCentered |> LeafNode

        let ofCenterTree (n : CenterTree) : LeafNode =
            invariant (CenterTree.isLeaf n) "8ca00fe1-a5df-4fbd-99d5-a109d43cf6bc"
            n |> Centered |> LeafNode

        let toAnyTree (n : LeafNode) : AnyTree = match n with | LeafNode n -> n

    type InnerNode      = InnerNode of node : AnyTree

    let private createNodeFromLeafs (dominance : Dominance) (a : LeafNode) (b : LeafNode) : LeafNode =
        failwith "todo: create node from two leafs"
      
    let private createNodeFromChildren (children : Children) : Tree =
        failwith "todo: create tree from children"

    let private createNodeFromCenterChildren (children : CenterChildren) : CenterTree =
        failwith "todo: create center tree from children"
            
    
    let private split (node : Tree) : Children =
        failwith "todo: split"

    let private splitCentered (node : CenterTree) : CenterChildren =
        failwith "todo: split a centered tree"

    /// Attaches a parent node to n and creates LoD layers for parent.
    let private growParent (n : Tree) : Tree =
        let cell = Tree.cell n
        let parentCell = cell.Parent
        let qi = parentCell.GetQuadrant(cell).Value
        let ns = Array.create 4 None
        ns.[qi] <- Some n
        let children = Children(root = cell.Parent, subnodes = ns)
        createNodeFromChildren children

    /// Attaches a parent node to n and creates LoD layers for parent.
    let private growCenterParent (n : CenterTree) : CenterTree =
        splitCentered n |> createNodeFromCenterChildren


    module CenterChildren =

        let ofIndexedSubnodes (root : Cell2d) (children : (int * Tree) list) : CenterChildren =
            invariant (root.IsCenteredAtOrigin) "5beee604-5064-460f-9b81-cf98aaca0b3b"
            ofIndexedSubnodes root children |> CenterChildren

        let growParents (ns : CenterChildren) =
            match ns with | CenterChildren (root, ns) -> CenterChildren(root.Parent, ns |> Array.map(Option.map(growParent)))


    
    let rec private mergeRec (dominance : Dominance) (a : AnyTree) (b : AnyTree) : AnyTree =
        
        (* helpers *)

        let mergeChildren (dominance : Dominance) (ns1 : Children) (ns2 : Children) : Children =
            failwith "todo: merge children"

        let mergeCenterChildren (dominance : Dominance) (ns1 : CenterChildren) (ns2 : CenterChildren) : CenterChildren =
            failwith "todo: merge center children"

        let failwithInvalidMerge rel error = sprintf "Invalid merge. %A. Error %s." rel error |> failwith

        (* zoo *)
        let relation = MergeRelation.create dominance a b
        match relation with

        // winner
        | MergeWinner (winner, _)                                           -> winner
            
        // disjoint
        | MergeDisjoint (_ , IndirectChild (x, _), IndirectChild  (y, _))   -> mergeRec dominance (x |> growParent |> NonCentered) (y |> growParent |> NonCentered)
        | MergeDisjoint (_ , IndirectChild (x, _), DirectChild    (y, _))   -> mergeRec dominance (x |> growParent |> NonCentered) (y               |> NonCentered)
        | MergeDisjoint (_ , IndirectChild (x, _), NestedDirect   (y   ))
        | MergeDisjoint (_ , IndirectChild (x, _), NestedIndirect (y   ))   -> mergeRec dominance.Flipped (Centered y) (NonCentered x) 
        
        | MergeDisjoint (_ , DirectChild   (x, _), IndirectChild  (y, _))   -> mergeRec dominance (x               |> NonCentered) (y |> growParent |> NonCentered)

        | MergeDisjoint (rc, DirectChild   (x,xi), DirectChild    (y,yi))   -> if rc.IsCenteredAtOrigin then
                                                                                    CenterChildren.ofIndexedSubnodes rc [(xi, x); (yi, y)]
                                                                                    |> createNodeFromCenterChildren |> Centered
                                                                               else
                                                                                    Children.ofIndexedSubnodes rc [(xi, x); (yi, y)]
                                                                                    |> createNodeFromChildren |> NonCentered

        | MergeDisjoint (rc, NestedDirect x,       IndirectChild  (y,yi))   -> let ns1 = splitCentered x |> CenterChildren.growParents
                                                                               let ns2 = CenterChildren.ofIndexedSubnodes rc [(yi, y)]
                                                                               mergeCenterChildren dominance ns1 ns2 |> createNodeFromCenterChildren |> Centered

        | MergeDisjoint (_ , NestedIndirect x,     IndirectChild  (y, _))   -> mergeRec dominance (growCenterParent x |> Centered) (y |> NonCentered)

        // collision
        | MergeCollision (NonCentered first, NonCentered second, d)         -> invariant (first.Cell = second.Cell) "8746b419-e469-4e57-b6ba-60c3d5aae3b8"
                                                                               match Tree.tryGetChildren first, Tree.tryGetChildren second with
                                                                               | None,     None     -> createNodeFromLeafs d (first |> LeafNode.ofTree) (second |> LeafNode.ofTree) |> LeafNode.toAnyTree
                                                                               | None,     Some ns  -> mergeChildren       d (split first) ns  |> createNodeFromChildren |> NonCentered
                                                                               | Some ns,  None     -> mergeChildren       d ns (split second) |> createNodeFromChildren |> NonCentered
                                                                               | Some ns1, Some ns2 -> mergeChildren       d ns1 ns2           |> createNodeFromChildren |> NonCentered

        | MergeCollision (Centered    first, Centered    second, d)         -> invariant (first.Cell = second.Cell) "fc986971-78ae-4fca-a367-eab059e8d68e"
                                                                               match CenterTree.tryGetChildren first, CenterTree.tryGetChildren second with
                                                                               | None,     None     -> createNodeFromLeafs d (first |> LeafNode.ofCenterTree) (second |> LeafNode.ofCenterTree) |> LeafNode.toAnyTree
                                                                               | None,     Some ns  -> mergeCenterChildren d (splitCentered first) ns  |> createNodeFromCenterChildren |> Centered
                                                                               | Some ns,  None     -> mergeCenterChildren d ns (splitCentered second) |> createNodeFromCenterChildren |> Centered
                                                                               | Some ns1, Some ns2 -> mergeCenterChildren d ns1 ns2                   |> createNodeFromCenterChildren |> Centered

        // subtree
        | MergeSubtree (NonCentered parent,  DirectChild (child, qi), d)    -> Children.ofIndexedSubnodes (Tree.cell parent) [(qi, child)]
                                                                               |> match Tree.tryGetChildren parent with
                                                                                  | None     -> mergeChildren d (split parent)
                                                                                  | Some ns1 -> mergeChildren d ns1
                                                                               |> createNodeFromChildren |> NonCentered

        | MergeSubtree (Centered parent,     DirectChild (child, qi), d)    -> CenterChildren.ofIndexedSubnodes (CenterTree.cell parent) [(qi, child)]
                                                                               |> match CenterTree.tryGetChildren parent with
                                                                                  | None     -> mergeCenterChildren d (splitCentered parent)
                                                                                  | Some ns1 -> mergeCenterChildren d ns1
                                                                               |> createNodeFromCenterChildren |> Centered

        | MergeSubtree (parent, IndirectChild (child,  _), d)               -> mergeRec d parent (child |> growParent |> NonCentered)

        | MergeSubtree (parent, NestedDirect (child),      d)               -> match parent with
                                                                               | NonCentered x ->
                                                                                   sprintf "Expected centered parent, but %A. Error 841b5fa0-c130-457b-9632-9d3e637a3377." x |> failwith
                                                                               | Centered parent ->
                                                                                   let ns1 = splitCentered parent
                                                                                   let ns2 = splitCentered child |> CenterChildren.growParents
                                                                                   mergeCenterChildren d ns1 ns2 |> createNodeFromCenterChildren |> Centered

        | MergeSubtree (parent, NestedIndirect (child),    dominance)       -> invariant (AnyTree.isCentered parent) "3c46c7a4-660a-4693-b4c1-ea7d4c46d7b3"
                                                                               mergeRec dominance parent (child |> growCenterParent |> Centered)

        // nested
        | MergeNested (large, small, d)                                     -> mergeRec d (Centered large) (small |> growCenterParent |> Centered)

        // invalid
        | MergeCollision (Centered    _, NonCentered _, _)                  -> failwithInvalidMerge relation "b29d2af5-376f-467a-99eb-834135d68d4a"
        | MergeCollision (NonCentered _, Centered    _, _)                  -> failwithInvalidMerge relation "d14f075a-7dd7-4b3b-a05b-ea4379a49a99"
        | MergeDisjoint (_ , DirectChild _,    NestedDirect  _)             -> failwithInvalidMerge relation "4cf244b3-28b0-4f67-a723-f090ad9ba9eb"
        | MergeDisjoint (_ , DirectChild _,    NestedIndirect  _)           -> failwithInvalidMerge relation "18852a24-7ea2-4676-a85d-e5d53abc6e5e"
        | MergeDisjoint (_ , NestedDirect _,   DirectChild _)               -> failwithInvalidMerge relation "cbad6def-c0fb-4c08-9c81-2f20e24b62db"
        | MergeDisjoint (_ , NestedDirect _,   NestedDirect _)              -> failwithInvalidMerge relation "b77eb555-1014-42a2-9610-2a3834a1b0f7"
        | MergeDisjoint (_ , NestedDirect _,   NestedIndirect _)            -> failwithInvalidMerge relation "576b5b2f-53f0-4657-9473-71b520685df2"
        | MergeDisjoint (_ , NestedIndirect _, NestedDirect _)              -> failwithInvalidMerge relation "77fa2ed2-c160-4e6e-9b4f-c846c43951c4"
        | MergeDisjoint (_ , NestedIndirect _, NestedIndirect _)            -> failwithInvalidMerge relation "3d988409-2238-4a5e-887e-7fae6394db12"
        | MergeDisjoint (_ , NestedIndirect _, DirectChild _)               -> failwithInvalidMerge relation "3f0cf471-74a1-49cd-bf6a-89175d2bac73"
        | MergeDisjoint (_, InvalidSubtree _, _)                            -> failwithInvalidMerge relation "4cc83f47-ffea-4f89-9aa2-b08dbc9bd5a2"
        | MergeDisjoint (_, _, InvalidSubtree _)                            -> failwithInvalidMerge relation "7f06b094-6493-4216-bb4c-69ae01f67ece"
        | MergeSubtree  (_, InvalidSubtree _, _)                            -> failwithInvalidMerge relation "429dcc39-1dae-48f8-8670-d5e6ed2c24f1"

    /// Immutable merge.
    let merge (dominance : Dominance) (first : QNodeRef) (second : QNodeRef) : QNodeRef =

        match first.TryGetInMemory(), second.TryGetInMemory() with
        | None,   None   -> NoNode
        | Some _, None   -> first
        | None,   Some _ -> second
        | Some a, Some b -> 
            
            invariantm (a.SplitLimitExponent = b.SplitLimitExponent)
                "Cannot merge quadtrees with different split limits."   "6222eb6b-a7aa-43c1-9323-e28d6275696b"

            let result = mergeRec dominance (AnyTree.ofQNode a) (AnyTree.ofQNode b)
            result |> AnyTree.qnode |> InMemoryNode
