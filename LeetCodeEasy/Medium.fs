namespace LeetCode

module Medium =

    let ArabicToRoman n =

        let convMap =
            [ 1, "I"
              4, "IV"
              5, "V"
              9, "IX"
              10, "X"
              40, "XL"
              50, "L"
              90, "XC"
              100, "C"
              400, "CD"
              500, "D"
              900, "CM"
              1000, "M" ]
            |> Map

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

    let MessageVariants s friends =

        let update value setopt =
            match setopt with
            | Some set -> set |> Set.add value
            | None -> Set[value]
            |> Some

        let graph =
            (Map.empty, friends)
            ||> List.fold (fun map (a, b) ->
                let updateAB = a |> Map.change <| update b
                let updateBA = b |> Map.change <| update a
                map |> updateAB |> updateBA)

        let rec bfs queue d p =
            match queue with
            | [] -> p
            | v :: q ->
                ((q, d, p), graph.[v])
                ||> Set.fold (fun (q, d, p) u ->
                    match d |> Map.find u with
                    | -1 ->
                        let q = q @ [ u ]
                        let d = d |> Map.change u (fun _ -> d.[v] + 1 |> Some)
                        let p = p |> Map.change u (fun _ -> Some v)
                        q, d, p
                    | _ -> q, d, p)
                |||> bfs

        let keysToMap k =
            graph.Keys |> Seq.map (fun key -> key, k) |> Map

        let p = ([ s ], keysToMap -1, keysToMap 0) |||> bfs

        let rec getSum sum v =
            if v = s || p |> Map.tryFind v |> Option.isNone then
                sum
            else
                getSum (sum + 1) p.[v]

        (0, graph.Keys)
        ||> Seq.fold (fun res n ->
            n
            |> getSum 0
            |> function
                | sum when sum % 2 = 0 -> res + 1
                | _ -> res)
