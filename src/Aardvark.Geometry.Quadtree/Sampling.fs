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

    let resampleV3f (a:V3f,b:V3f,c:V3f,d:V3f) = (a+b+c+d)*0.25f
    
    let resampleV3fNorm (a:V3f,b:V3f,c:V3f,d:V3f) = ((a+b+c+d)*0.25f).Normalized
    
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
        
    let resamplers = Map.ofSeq [
        (Defs.Normals3f.Id, resampleV3fNorm :> obj)
        (Defs.Colors3b.Id, resampleC3b :> obj)
        (Defs.Colors4b.Id, resampleC4b :> obj)
        (Defs.Heights1d.Id, resampleFloat64 :> obj)
        (Defs.Heights1f.Id, resampleFloat32 :> obj)
        (Defs.HeightStdDevs1f.Id, resampleFloat32 :> obj)
        (Defs.Intensities1i.Id, resampleInt32 :> obj)
    ]

    let getResamplerFor (def : Durable.Def) =
        match resamplers |> Map.tryFind def.Id with
        | Some x -> x
        | None   -> failwith <| sprintf "Resampling not supported for %A. Invariant 16ff1f91-b678-41d5-a223-e5bcf69609fa." def
