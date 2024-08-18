namespace LeetCode

module Easy =

    // Наибольшая общая подстрока (с начала слова)
    let longestCommonPrefix words =

        let shortestWord = words |> List.minBy String.length

        shortestWord
        |> Seq.indexed
        |> Seq.takeWhile (fun (i, char) -> words |> Seq.forall (fun word -> word.[i] = char))
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

    let private countDuplicates list =

        let setNewValue =
            function
            | Some i -> i + 1
            | None -> 1
            >> Some

        let change map symbol = map |> Map.change symbol setNewValue

        let rec fillMap map charlist =
            match charlist with
            | head :: tail -> head |> change map |> fillMap <| tail
            | [] -> map

        list |> fillMap (Map [])

    // Являются ли два слова анаграммами друг друга?
    let IsAnagram str1 str2 =
        let seqToMap = Seq.toList >> countDuplicates
        (seqToMap str1, seqToMap str2) ||> (=)

    // Быстрое извлечение квадратного корня
    let MySqrt x =

        let rec setd d = if d <= x then d else d >>> 2

        let rec calc k c d =
            if d = 0 then
                c
            else if k >= c + d then
                (k - c - d, (c >>> 1) + d, d >>> 2) |||> calc
            else
                (k, c >>> 1, d >>> 2) |||> calc

        1 <<< 30 |> setd |> calc x 0

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

        let convert =
            function
            | [| a; b |] ->
                convMap
                |> Map.tryFind (b + a)
                |> Option.defaultValue (convMap.[a] + convMap.[b])
            | [| c |] -> convMap.[c]
            | _ -> failwith "Wrong data"

        s
        |> Seq.rev
        |> Seq.map string
        |> Seq.chunkBySize 2
        |> Seq.fold (fun acc i -> acc + convert i) 0

    // Перевод числа в номер колонки Excel
    let ExcelSheetColumnTitle columnNumber =

        let digitToLetter k =
            System.Char.ConvertFromUtf32(k + 65) |> string

        let rec convert col result =
            if col <= 0 then
                result
            else
                let struct (div, rem) = System.Math.DivRem(col - 1, 26)
                rem |> digitToLetter |> convert div |> (+) <| result

        convert columnNumber ""

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
        |> Seq.tryFindIndex (fun window -> (window, needle) ||> Seq.forall2 (=))
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

        let getParts (delimiter: char) (str: string) =
            let delimiterIndex = str |> Seq.findIndex ((=) delimiter)
            let part1 = str.[.. delimiterIndex - 1]
            let part2 = string delimiter
            let part3 = str.[delimiterIndex + 1 ..]
            (part1, part2, part3)

        files
        |> countDuplicates
        |> Map.toList
        |> List.collect (fun (fileName, fileCount) ->
            [ 0 .. fileCount - 1 ]
            |> List.map (fun i ->
                let number = if i = 0 then "" else $"({i})"

                if fileName |> String.exists ((=) '.') then
                    let name, dot, ext = fileName |> getParts '.'
                    name + number + dot + ext
                else
                    fileName + number))
        |> List.sort
