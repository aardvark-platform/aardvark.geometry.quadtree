namespace Aardvark.Geometry.Quadtree

open Aardvark.Data
open Aardvark.Base
open System
open System.Runtime.CompilerServices

#nowarn "44"

module Defs =

    let def id name description (typ : Durable.Def) = Durable.Def(Guid.Parse(id), name, description, typ.Id, false)

    module Layer =
        let DefId              = def "88b91a68-861b-4b04-84b4-a9db0ab024f3" "Aardvark.Geometry.Quadtree.Def.Id" "Guid." Durable.Primitives.GuidDef
        let DataMapping        = def "6ad967a5-5464-422a-b3f3-873050a42586" "Aardvark.Geometry.Quadtree.DataMapping" "DurableMap." Durable.Primitives.DurableMap
        let BufferOrigin       = def "78e9a99b-5255-42ed-9272-732074710c29" "Aardvark.Geometry.Quadtree.DataMapping.BufferOrigin" "Cell2d." Durable.Aardvark.Cell2d
        let BufferSize         = def "6aa5d96c-4f89-4b01-bd16-f39239d8be4e" "Aardvark.Geometry.Quadtree.DataMapping.BufferSize" "V2i." Durable.Aardvark.V2i
        let Window             = def "381a6f5d-30cc-489e-8bf7-2ebb13d47bb7" "Aardvark.Geometry.Quadtree.DataMapping.Window" "Box2l." Durable.Aardvark.Box2l
                               
    let Node                   = def "e497f9c1-c903-41c4-91de-32bf76e009da" "Quadtree.Node" "Obsolete. A quadtree node. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
    let NodeLeaf               = def "c74fad23-1211-4073-94e5-54b778e0d295" "Quadtree.NodeLeaf" "A quadtree leaf node. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
    let NodeInner              = def "1f7baa27-5bcc-420f-89a3-714b65d93a2d" "Quadtree.NodeInner" "A quadtree inner node. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
    let NodeMerge              = def "2d80c73e-ed3e-442a-a631-4f570ff838fd" "Quadtree.NodeMerge" "A quadtree merge node. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
    let NodeLinked             = def "8628aab6-a416-42ab-9192-bae0d5590f4f" "Quadtree.LinkedNode" "A reference (link) to another quadtree node. DurableMapAligned16" Durable.Primitives.DurableMapAligned16

    let NodeId                 = def "e46c4163-dd28-43a4-8254-bc21dc3f766b" "Quadtree.NodeId" "Quadtree. Unique id of a node. Guid." Durable.Primitives.GuidDef
    let TargetId               = def "4a5396fa-9bc6-4616-a450-1516946514f0" "Quadtree.TargetId" "Quadtree. Reference (link) to another node. Guid." Durable.Primitives.GuidDef
    let CellBounds             = def "59258849-5765-4d11-b760-538282063a55" "Quadtree.CellBounds" "Quadtree. Node bounds in cell space. Cell2d." Durable.Aardvark.Cell2d
    let SplitLimitExponent     = def "472cf20a-e917-4aed-8379-7661cd880511" "Quadtree.SplitLimitExponent" "Quadtree. Power-of-two split limit exponent. Int32." Durable.Primitives.Int32
    let OriginalSampleExponent = def "16c41f1d-0c31-4ef8-b22b-36276d2f2e45" "Quadtree.OriginalSampleExponent" "Quadtree. Power-of-two original sample exponent. Int32." Durable.Primitives.Int32
    let Layers                 = def "c39a978b-00f5-485f-b0b3-d2cf9599016b" "Quadtree.Layers" "A quadtree node's layers. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
    let ExactBoundingBox       = def "abd1f1e6-7165-44d2-8019-fde4f59c9c9f" "Quadtree.ExactBoundingBox" "Quadtree. Exact bounds of original (most detailed) data. Box2d." Durable.Aardvark.Box2d
    let SubnodeIds             = def "a2841629-e4e2-4b90-bdd1-7a1a5a41bded" "Quadtree.SubnodeIds" "Quadtree. Subnodes as array of guids. Array length is 4 for inner nodes (where Guid.Empty means no subnode) and no array for leaf nodes. Guid[]." Durable.Primitives.GuidArray
          
    let Dominance              = def "e782ace4-f2dd-4bc1-b8ad-5abad87f4ff1" "Quadtree.Dominance" "Quadtree. Dominance mode for quadtree merges. Guid." Durable.Primitives.GuidDef
    let DominanceFirst                = def "0d245911-3c3e-4bd4-b5c2-8de51267936e" "Quadtree.Dominance.First" "Quadtree. Quadtree.Dominance mode. Guid." Durable.Primitives.GuidDef  
    let DominanceSecond               = def "20f0b738-3f67-46d2-bc75-9f802bc714f3" "Quadtree.Dominance.Second" "Quadtree. Quadtree.Dominance mode. Guid." Durable.Primitives.GuidDef  
    let DominanceMoreDetailedOrFirst  = def "7aade0f1-9a10-48f1-8a07-e1e30fb2816d" "Quadtree.Dominance.MoreDetailedOrFirst" "Quadtree. Quadtree.Dominance mode. Guid." Durable.Primitives.GuidDef  
    let DominanceMoreDetailedOrSecond = def "046bc427-bde6-4270-b39c-a25290eede5f" "Quadtree.Dominance.MoreDetailedOrSecond" "Quadtree. Quadtree.Dominance mode. Guid." Durable.Primitives.GuidDef  

    let Heights1f              = def "4cb689c5-b627-4bcd-9db7-5dbd24d7545a" "Quadtree.Heights1f" "Quadtree. Height value per sample. Float32[]." Durable.Primitives.Float32Array
    let Heights1fLayer         = def "fcf042b4-fe33-4e28-9aea-f5526600f8a4" "Quadtree.Heights1f.Layer" "Quadtree. Heights1f layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
    
    let HeightsBilinear4f      = def "4d343b08-b82d-4667-830b-72a5180509de" "Quadtree.HeightsBilinear4f" "Quadtree. Height value per sample as bilinear params. height(x,y) = A + B*x + C*y+ D*x*y, where x,y in range [-sample.Size/2, +sample.Size/2], (x=0,y=0) corresponds to center of sample (A), and A=v.X, B=v.Y, C=v.Z, D=v.W. V4f[]." Durable.Aardvark.V4fArray
    let HeightsBilinear4fLayer = def "2e03222d-4f72-49e6-b56f-79c8839127f8" "Quadtree.HeightsBilinear4f.Layer" "Quadtree. HeightsBilinearParams4f layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16

    let Heights1d              = def "c66a4240-00ef-44f9-b377-0667f279b97e" "Quadtree.Heights1d" "Quadtree. Height value per sample. Float64[]." Durable.Primitives.Float64Array
    let Heights1dLayer         = def "baa8ed40-57e3-4f88-8d11-0b547494c8cb" "Quadtree.Heights1d.Layer" "Quadtree. Heights1d layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
                               
    let HeightsBilinear4d      = def "ea09c9f0-ac1a-4905-9a4e-5a7ebdcd873f" "Quadtree.HeightsBilinear4d" "Quadtree. Height value per sample as bilinear params. height(x,y) = A + B*x + C*y+ D*x*y, where x,y in range [-sample.Size/2, +sample.Size/2], (x=0,y=0) corresponds to center of sample (A), and A=v.X, B=v.Y, C=v.Z, D=v.W. V4d[]." Durable.Aardvark.V4dArray
    let HeightsBilinear4dLayer = def "c93e5b85-7455-425f-949b-517f38f40bac" "Quadtree.HeightsBilinear4d.Layer" "Quadtree. HeightsBilinearParams4d layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16

    let Normals3f              = def "d5166ae4-7bea-4ebe-a3bf-cae8072f8951" "Quadtree.Normals3f" "Quadtree. Normal vector per sample. V3f[]." Durable.Aardvark.V3fArray
    let Normals3fLayer         = def "817ecbb6-1e86-41a2-b1ee-53884a27ea97" "Quadtree.Normals3f.Layer" "Quadtree. Normals3f layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
                               
    let Normals3d              = def "4d466f01-e252-4234-a1e1-0a6cdcb7925b" "Quadtree.Normals3d" "Quadtree. Normal vector per sample. V3d[]." Durable.Aardvark.V3dArray
    let Normals3dLayer         = def "50d650c7-9adf-4675-a918-e1b8b0382f94" "Quadtree.Normals3d.Layer" "Quadtree. Normals3d layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
                               
    let HeightStdDevs1f        = def "74bfe324-98ad-4f57-8163-120361e1e68e" "Quadtree.HeightStdDevs1f" "Quadtree. Standard deviation per height value. Float32[]." Durable.Primitives.Float32Array
    let HeightStdDevs1fLayer   = def "f93e8c5f-7e9e-4e1f-b57a-4475ebf023af" "Quadtree.HeightStdDevs1f.Layer" "Quadtree. HeightStdDevs1f layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
                               
    let HeightStdDevs1d        = def "49930cad-cf52-4e09-a797-4045d5feb421" "Quadtree.HeightStdDevs1d" "Quadtree. Standard deviation per height value. Float64[]." Durable.Primitives.Float64Array
    let HeightStdDevs1dLayer   = def "be62b889-1078-45fe-9d00-f3accb9ce243" "Quadtree.HeightStdDevs1d.Layer" "Quadtree. HeightStdDevs1d layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
                               
    let Colors3b               = def "378d93ae-45e2-4e6a-9018-f09c88e7d10f" "Quadtree.Colors3b" "Quadtree. Color value per sample. C3b[]." Durable.Aardvark.C3bArray
    let Colors3bLayer          = def "d6f8750c-6e94-4c9f-9f83-099268e95cc5" "Quadtree.Colors3b.Layer" "Quadtree. Colors3b layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
                               
    let Colors4b               = def "97b8282c-964a-40e8-a7be-d55ee587b5d4" "Quadtree.Colors4b" "Quadtree. Color value per sample. C4b[]." Durable.Aardvark.C4bArray
    let Colors4bLayer          = def "8fe18316-7fa3-4704-869d-c3995d19d03e" "Quadtree.Colors4b.Layer" "Quadtree. Colors4b layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
                               
    let Colors3f               = def "a2ff1af5-bf99-4861-bea9-0f6f731d26f7" "Quadtree.Colors3f" "Quadtree. Color value per sample. C3f[]." Durable.Aardvark.C3fArray
    let Colors3fLayer          = def "285cf58f-3817-4b09-8ae0-2a9911b87396" "Quadtree.Colors3f.Layer" "Quadtree. Colors3f layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
                               
    let Colors4f               = def "4b05702f-0772-4db1-8368-7dc6a7e1a2e3" "Quadtree.Colors4f" "Quadtree. Color value per sample. C4f[]." Durable.Aardvark.C4fArray
    let Colors4fLayer          = def "252666e7-9803-41b1-8fac-71fcf02ef2d5" "Quadtree.Colors4f.Layer" "Quadtree. Colors4f layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
                               
    let Intensities1i          = def "da564b5d-c5a4-4274-806a-acd04fa206b2" "Quadtree.Intensities1i" "Quadtree. Intensity value per sample. Int32[]." Durable.Primitives.Int32Array
    let Intensities1iLayer     = def "b44484ba-e9a6-4e0a-a26a-3641a91ee9cf" "Quadtree.Intensities1i.Layer" "Quadtree. Intensities1i layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
                               
    let Intensities1l          = def "edff2410-8e94-4e42-b985-30d119c46727" "Quadtree.Intensities1l" "Quadtree. Intensity value per sample. Int64[]." Durable.Primitives.Int64Array
    let Intensities1lLayer     = def "3086a77c-34dd-4bce-8b14-d5efe44c4e59" "Quadtree.Intensities1l.Layer" "Quadtree. Intensities1l layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
                               
    let Intensities1f          = def "c8d6abce-063f-48d0-91b9-4469e7cb9404" "Quadtree.Intensities1f" "Quadtree. Intensity value per sample. Float32[]." Durable.Primitives.Float32Array
    let Intensities1fLayer     = def "28f6fc0b-ca05-40a5-bc40-e8e92d79a82c" "Quadtree.Intensities1f.Layer" "Quadtree. Intensities1f layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
                               
    let Intensities1d          = def "acdea865-c4ef-441d-804e-18187548a418" "Quadtree.Intensities1d" "Quadtree. Intensity value per sample. Float64[]." Durable.Primitives.Int64Array
    let Intensities1dLayer     = def "8864088c-c122-4da4-9dcb-14f6b5dd4ccc" "Quadtree.Intensities1d.Layer" "Quadtree. Intensities1d layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
      
    [<Obsolete("Use HeightsBilinear4f instead.")>]
    let BilinearParams4f       = def "67f0e87d-7ba6-47c9-b443-17e7b835bb4e" "Quadtree.BilinearParams4f" "Obsolete. Use Quadtree.HeightsBilinear4f instead." Durable.Aardvark.V4fArray
    [<Obsolete("Use HeightsBilinear4fLayer instead.")>]
    let BilinearParams4fLayer  = def "376d01f1-7bfe-4bd5-b459-9a63e81a08d9" "Quadtree.BilinearParams4f.Layer" "Obsolete. Use Quadtree.HeightsBilinear4f.Layer instead." Durable.Primitives.DurableMapAligned16
                             
    [<Obsolete("Use HeightsBilinear4d instead.")>]  
    let BilinearParams4d       = def "a9064e6e-f967-4017-8415-08193b77aea0" "Quadtree.BilinearParams4d" "Obsolete. Use Quadtree.HeightsBilinear4d instead." Durable.Aardvark.V4dArray
    [<Obsolete("Use HeightsBilinear4dLayer instead.")>]
    let BilinearParams4dLayer  = def "49303969-ffa5-4c7e-89ea-8d5ad7dd6d3d" "Quadtree.BilinearParams4d.Layer" "Obsolete. Use Quadtree.HeightsBilinear4d.Layer instead." Durable.Primitives.DurableMapAligned16
             
             
    let Volumes1f              = def "702521f3-2474-4072-9f9f-d506b6e19e7a" "Quadtree.Volumes1f" "Quadtree. Volume (height difference) value per sample. Float32[]." Durable.Primitives.Float32Array
    let Volumes1fLayer         = def "f84c4628-9969-43ad-9a93-813430bae8e1" "Quadtree.Volumes1f.Layer" "Quadtree. Volumes1f layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
    
    let VolumesBilinear4f      = def "127e11b1-ee31-4df8-b1ea-d780dfd03e70" "Quadtree.VolumesBilinear4f" "Quadtree. Volume value (height difference) per sample as bilinear params. volume(x,y) = A + B*x + C*y+ D*x*y, where x,y in range [-sample.Size/2, +sample.Size/2], (x=0,y=0) corresponds to center of sample (A), and A=v.X, B=v.Y, C=v.Z, D=v.W. V4f[]." Durable.Aardvark.V4fArray
    let VolumesBilinear4fLayer = def "5a743ae9-b37d-4e94-842f-b544588a56d5" "Quadtree.VolumesBilinear4f.Layer" "Quadtree. VolumesBilinearParams4f layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16

    let Volumes1d              = def "c88bb26c-1145-4e65-aa59-2e2d4cdfa0ad" "Quadtree.Volumes1d" "Quadtree. Height value per sample. Float64[]." Durable.Primitives.Float64Array
    let Volumes1dLayer         = def "5d376ccc-6d02-47ee-bff0-06ebe9680c9c" "Quadtree.Volumes1d.Layer" "Quadtree. Volumes1d layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16
                               
    let VolumesBilinear4d      = def "9aadc102-68b7-4e6f-ba22-bad1440e1bcf" "Quadtree.VolumesBilinear4d" "Quadtree. Volume value (height difference) per sample as bilinear params. volume(x,y) = A + B*x + C*y+ D*x*y, where x,y in range [-sample.Size/2, +sample.Size/2], (x=0,y=0) corresponds to center of sample (A), and A=v.X, B=v.Y, C=v.Z, D=v.W. V4d[]." Durable.Aardvark.V4dArray
    let VolumesBilinear4dLayer = def "fd547ab1-88e9-4fc2-9c7d-a67545bfd3d1" "Quadtree.VolumesBilinear4d.Layer" "Quadtree. VolumesBilinearParams4d layer. DurableMapAligned16." Durable.Primitives.DurableMapAligned16

    let private def2layer = Map.ofList [
        (Heights1f         , Heights1fLayer         )
        (HeightsBilinear4f , HeightsBilinear4fLayer )
        (Heights1d         , Heights1dLayer         )
        (HeightsBilinear4d , HeightsBilinear4dLayer )
        (Normals3f         , Normals3fLayer         )
        (Normals3d         , Normals3dLayer         )
        (HeightStdDevs1f   , HeightStdDevs1fLayer   )
        (HeightStdDevs1d   , HeightStdDevs1dLayer   )
        (Colors3b          , Colors3bLayer          )
        (Colors4b          , Colors4bLayer          )
        (Colors3f          , Colors3fLayer          )
        (Colors4f          , Colors4fLayer          )
        (Intensities1i     , Intensities1iLayer     )
        (Intensities1l     , Intensities1lLayer     )
        (Intensities1f     , Intensities1fLayer     )
        (Intensities1d     , Intensities1dLayer     )
        (BilinearParams4f  , BilinearParams4fLayer  )
        (BilinearParams4d  , BilinearParams4dLayer  )
        (Volumes1f         , Volumes1fLayer         )
        (VolumesBilinear4f , VolumesBilinear4fLayer )
        (Volumes1d         , Volumes1dLayer         )
        (VolumesBilinear4d , VolumesBilinear4dLayer )
        ]

    let GetLayerFromDef def = def2layer.[def]

    let private layer2def = Map.ofList [
        (Heights1fLayer         , Heights1f         )
        (HeightsBilinear4fLayer , HeightsBilinear4f )
        (Heights1dLayer         , Heights1d         )
        (HeightsBilinear4dLayer , HeightsBilinear4d )
        (Normals3fLayer         , Normals3f         )
        (Normals3dLayer         , Normals3d         )
        (HeightStdDevs1fLayer   , HeightStdDevs1f   )
        (HeightStdDevs1dLayer   , HeightStdDevs1d   )
        (Colors3bLayer          , Colors3b          )
        (Colors4bLayer          , Colors4b          )
        (Colors3fLayer          , Colors3f          )
        (Colors4fLayer          , Colors4f          )
        (Intensities1iLayer     , Intensities1i     )
        (Intensities1lLayer     , Intensities1l     )
        (Intensities1fLayer     , Intensities1f     )
        (Intensities1dLayer     , Intensities1d     )
        (BilinearParams4fLayer  , BilinearParams4f  )
        (BilinearParams4dLayer  , BilinearParams4d  )
        (Volumes1fLayer         , Volumes1f         )
        (VolumesBilinear4fLayer , VolumesBilinear4f )
        (Volumes1dLayer         , Volumes1d         )
        (VolumesBilinear4dLayer , VolumesBilinear4d )
        ]

    let TryGetDefFromLayer layerDef = layer2def |> Map.tryFind layerDef
    
    [<MethodImpl(MethodImplOptions.NoInlining ||| MethodImplOptions.NoOptimization)>]
    let private keep (_ : 'a) = ()

    [<OnAardvarkInit;CompilerMessage("Internal only",1337,IsHidden=true)>]
    let init () = keep Node