namespace LeetCode

module Medium =

    let ArabicToRoman arabic =

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

        let rec convDigit coef digit =
            let conv = convDigit coef

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

        let rec convert coef number result =
            if number = 0 then
                result
            else
                let struct (div, rem) = System.Math.DivRem(number, 10)
                let roman = convDigit coef rem
                convert (10 * coef) div (roman + result)

        convert 1 arabic ""
