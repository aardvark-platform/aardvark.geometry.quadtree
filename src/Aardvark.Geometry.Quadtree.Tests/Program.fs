open Aardvark.Base
open Aardvark.Geometry.Quadtree
open Aardvark.Geometry.Quadtree.Tests


module Program =

    let [<EntryPoint>] main _ =
    
        //Tests.cpunz.``punz_merge_verySmall_into_coarse_volume`` ()
        
        //Tests.cpunz.``punz_double_merge_volume`` ()

        //Tests.cpunz.``cpunz_20201130`` ()

        //Tests.cpunz.``punz_merge_withOverlap_overboarder_other_volume`` ()
        
        //Tests.cpunz.``punz_merge_2_levels`` ()

        //Tests.cpunz.``punz_merge_withOverlap_overboarder_overOrigin_other_volume`` ()
        
        //StructureTests.``sm 2020-12-07`` ()

        //QueryTests.Positions ()


        let safeContains (a : Cell2d) (b : Cell2d) =
            match a.IsCenteredAtOrigin, b.IsCenteredAtOrigin with
            | false, false -> if b.Exponent > a.Exponent then false else getParentForLevel a.Exponent b = a
            | true,  false -> if b.Exponent > a.Exponent - 1 then false else (getParentForLevel (a.Exponent-1) b).TouchesOrigin
            | false, true  -> false
            | true,  true  -> b.Exponent <= a.Exponent

        let a = Cell2d(0,0,6)
        let b = Cell2d(16)

        let x = safeContains a b
        let y = safeContains b a

        printfn "%A %A" x y

        0
