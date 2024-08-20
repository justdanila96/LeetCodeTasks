namespace LeetCode

module Medium =

    let ArabicToRoman n =

        let convMap =
            Map
                [ (1, "I")
                  (4, "IV")
                  (5, "V")
                  (9, "IX")
                  (10, "X")
                  (40, "XL")
                  (50, "L")
                  (90, "XC")
                  (100, "C")
                  (400, "CD")
                  (500, "D")
                  (900, "CM")
                  (1000, "M") ]

        let rec toRoman coef digit =
            let conv = toRoman coef

            match digit with
            | 1
            | 4
            | 5
            | 9 -> convMap.[digit * coef]
            | 2
            | 3 as n -> conv 1 |> String.replicate n
            | 6
            | 7
            | 8 as n -> conv 5 + conv (n - 5)
            | _ -> ""

        (n, 1)
        |> Seq.unfold (fun (value, mult) ->
            match value with
            | 0 -> None
            | _ ->
                let struct (newValue, arabic) = System.Math.DivRem(value, 10)
                let roman = toRoman mult arabic
                let newState = (newValue, mult * 10)
                Some(roman, newState))
        |> Seq.rev
        |> String.concat ""
