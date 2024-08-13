namespace LeetCode

module Medium =

    let ArabicToRoman arabic =

        let dictionary =
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

            let convcoef = convDigit coef

            match digit with
            | 1
            | 4
            | 5
            | 9 -> dictionary.[digit * coef]
            | k when k > 1 && k < 4 -> convcoef 1 |> String.replicate k
            | k when k > 5 && k <= 8 -> convcoef 5 + (convcoef 1 |> String.replicate (k - 5))
            | _ -> failwith "Something's wrong!"

        let rec convert coef number result =
            if number = 0 then
                result
            else
                convDigit (pown 10 coef) (number % 10) + result
                |> convert (coef + 1) (number / 10)

        convert 0 arabic ""
