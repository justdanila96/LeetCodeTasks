﻿namespace LeetCode

module Easy =

    // Наибольшая общая подстрока (с начала слова)
    let longestCommonPrefix words =

        let shortestWord = words |> List.minBy String.length

        let allWordsHaveSameChar (index, char) =
            words |> Seq.forall (fun word -> word.[index] = char)

        shortestWord
        |> Seq.indexed
        |> Seq.takeWhile allWordsHaveSameChar
        |> fun res -> shortestWord.Substring(0, Seq.length res)

    // Задача FizzBuzz
    let FizzBuzz n =

        let convert =
            function
            | k when k % 3 = 0 && k % 5 <> 0 -> "Fizz"
            | k when k % 5 = 0 && k % 3 <> 0 -> "Buzz"
            | k when k % 3 = 0 && k % 5 = 0 -> "FizzBuzz"
            | i -> string i

        [ 1..n ] |> List.map convert

    // Являются ли два слова анаграммами друг друга?
    let IsAnagram str1 str2 =
        let setValue value = (1, value) ||> Option.fold (+) |> Some

        let countBySymbol seq =
            (Map [], seq) ||> Seq.fold (fun map key -> map |> Map.change key setValue)

        (countBySymbol str1, countBySymbol str2) ||> (=)

    // Быстрое извлечение квадратного корня
    let MySqrt x =

        let rec setd d = if d <= x then d else setd (d >>> 2)

        let rec calc d k c =
            if d = 0 then
                c
            else
                let cshr = c >>> 1
                (if k >= c + d then k - c - d, cshr + d else k, cshr) ||> calc (d >>> 2)

        1 <<< 30 |> setd |> calc <|| (x, 0)

    // Перевод чисел из римских в арабские
    let RomanToArabic s =

        let convMap =
            Map
                [ ("I", 1)
                  ("IV", 4)
                  ("V", 5)
                  ("IX", 9)
                  ("X", 10)
                  ("XL", 40)
                  ("L", 50)
                  ("XC", 90)
                  ("C", 100)
                  ("CD", 400)
                  ("D", 500)
                  ("CM", 900)
                  ("M", 1000) ]

        let convert acc chunk =
            match chunk with
            | [| a; b |] ->
                convMap
                |> Map.tryFind (b + a)
                |> Option.defaultValue (convMap.[a] + convMap.[b])
            | [| c |] -> convMap.[c]
            | _ -> failwith "Wrong data"
            |> (+) acc

        s |> Seq.rev |> Seq.map string |> Seq.chunkBySize 2 |> Seq.fold convert 0

    // Перевод числа в номер колонки Excel
    let ExcelSheetColumnTitle columnNumber =
        columnNumber
        |> Seq.unfold (fun d ->
            match d with
            | 0 -> None
            | _ ->
                let struct (div, rem) = System.Math.DivRem(d - 1, 26)
                let letter = System.Char.ConvertFromUtf32(rem + 65)
                Some(string letter, div))
        |> Seq.rev
        |> String.concat ""

    // Является ли строка палиндромом
    let IsPalindrome input =
        input
        |> Seq.where (fun symbol -> System.Char.IsLetter(symbol))
        |> Seq.map (fun symbol -> System.Char.ToLowerInvariant(symbol))
        |> fun symbols -> (symbols, Seq.rev symbols)
        ||> Seq.forall2 (=)

    // Индекс первого вхождения подстроки
    let IndexOfFirstOccurrence haystack needle =
        haystack
        |> Seq.windowed (String.length needle)
        |> Seq.tryFindIndex (fun substring -> (substring, needle) ||> Seq.forall2 (=))
        |> Option.defaultValue -1

    // Длина последнего слова
    let LengthOfLastWord word =
        word
        |> Seq.rev
        |> Seq.skipWhile ((=) ' ')
        |> Seq.takeWhile ((<>) ' ')
        |> Seq.length

    // Пронумеровать дубликаты в списке файлов и отсортировать лексикографически
    let hugeDownload files =
        files
        |> List.countBy id
        |> List.collect (fun (fileName: string, count) ->
            let dotIndex = fileName |> Seq.tryFindIndexBack ((=) '.')

            [ 0 .. count - 1 ]
            |> List.map (fun i ->
                let number = if i = 0 then "" else $"({i})"

                match dotIndex with
                | Some i -> fileName.Insert(i, number)
                | None -> fileName + number))
        |> List.sort
