namespace Aardvark.Geometry.Quadtree

open Aardvark.Base
open Aardvark.Data

type BorderMode<'a> =
    | Fail
    | ClampToEdge
    | ClampToBorder of 'a

module internal Resamplers =

    let resampleInt32 (a,b,c,d)       = (a+b+c+d)/4

    let resampleInt64 (a,b,c,d)     = (a+b+c+d)/4L

    let resampleFloat64 (a,b,c,d)     = (a+b+c+d)*0.25

    let resampleFloat32 (a,b,c,d)   = (a+b+c+d)*0.25f

    let resampleV2f (a:V2f,b:V2f,c:V2f,d:V2f) = (a+b+c+d)*0.25f
    
    let resampleV2fNorm (a:V2f,b:V2f,c:V2f,d:V2f) = ((a+b+c+d)*0.25f).Normalized
    
    let resampleV2d (a:V2d,b:V2d,c:V2d,d:V2d) = (a+b+c+d)*0.25
    
    let resampleV2dNorm (a:V2d,b:V2d,c:V2d,d:V2d) = ((a+b+c+d)*0.25).Normalized
    
    let resampleV3f (a:V3f,b:V3f,c:V3f,d:V3f) = (a+b+c+d)*0.25f
    
    let resampleV3fNorm (a:V3f,b:V3f,c:V3f,d:V3f) = ((a+b+c+d)*0.25f).Normalized
    
    let resampleV3d (a:V3d,b:V3d,c:V3d,d:V3d) = (a+b+c+d)*0.25
    
    let resampleV3dNorm (a:V3d,b:V3d,c:V3d,d:V3d) = ((a+b+c+d)*0.25).Normalized
    
    let resampleV4f (a:V4f,b:V4f,c:V4f,d:V4f) = (a+b+c+d)*0.25f
    
    let resampleV4fNorm (a:V4f,b:V4f,c:V4f,d:V4f) = ((a+b+c+d)*0.25f).Normalized
    
    let resampleV4d (a:V4d,b:V4d,c:V4d,d:V4d) = (a+b+c+d)*0.25
    
    let resampleV4dNorm (a:V4d,b:V4d,c:V4d,d:V4d) = ((a+b+c+d)*0.25).Normalized
    
    let resampleC3b (a:C3b,b:C3b,c:C3b,d:C3b) =
        let _r = (int a.R + int b.R + int c.R + int d.R) / 4
        let _g = (int a.G + int b.G + int c.G + int d.G) / 4
        let _b = (int a.B + int b.B + int c.B + int d.B) / 4
        C3b(_r,_g,_b)
    
    let resampleC4b (a:C4b,b:C4b,c:C4b,d:C4b) =
        let _r = (int a.R + int b.R + int c.R + int d.R) / 4
        let _g = (int a.G + int b.G + int c.G + int d.G) / 4
        let _b = (int a.B + int b.B + int c.B + int d.B) / 4
        let _a = (int a.A + int b.A + int c.A + int d.A) / 4
        C4b(_r,_g,_b,_a)

    let resampleC3f (a:C3f,b:C3f,c:C3f,d:C3f) =
        let _r = (a.R + b.R + c.R + d.R) * 0.25f
        let _g = (a.G + b.G + c.G + d.G) * 0.25f
        let _b = (a.B + b.B + c.B + d.B) * 0.25f
        C3f(_r,_g,_b)
    
    let resampleC4f (a:C4f,b:C4f,c:C4f,d:C4f) =
        let _r = (a.R + b.R + c.R + d.R) * 0.25f
        let _g = (a.G + b.G + c.G + d.G) * 0.25f
        let _b = (a.B + b.B + c.B + d.B) * 0.25f
        let _a = (a.A + b.A + c.A + d.A) * 0.25f
        C4f(_r,_g,_b,_a)
        
    let resamplers = Map.ofSeq [
        (Defs.Normals3f.Id, resampleV3fNorm :> obj)
        (Defs.Normals3d.Id, resampleV3dNorm :> obj)
        (Defs.Colors3b.Id, resampleC3b :> obj)
        (Defs.Colors4b.Id, resampleC4b :> obj)
        (Defs.Colors3f.Id, resampleC3f :> obj)
        (Defs.Colors4f.Id, resampleC4f :> obj)
        (Defs.Heights1f.Id, resampleFloat32 :> obj)
        (Defs.Heights1d.Id, resampleFloat64 :> obj)
        (Defs.HeightStdDevs1f.Id, resampleFloat32 :> obj)
        (Defs.HeightStdDevs1d.Id, resampleFloat64 :> obj)
        (Defs.Intensities1i.Id, resampleInt32 :> obj)
        (Defs.Intensities1l.Id, resampleInt64 :> obj)
        (Defs.Intensities1f.Id, resampleFloat32 :> obj)
        (Defs.Intensities1d.Id, resampleFloat64 :> obj)
        (Defs.BilinearParams4f.Id, resampleV4fNorm :> obj)
        (Defs.BilinearParams4d.Id, resampleV4dNorm :> obj)
    ]

    let getResamplerFor (def : Durable.Def) =
        match resamplers |> Map.tryFind def.Id with
        | Some x -> x
        | None   -> failwith <| sprintf "Resampling not supported for %A. Invariant 16ff1f91-b678-41d5-a223-e5bcf69609fa." def
