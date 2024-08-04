namespace LeetCode

module Easy =

    // Наибольшая общая подстрока (с начала слова)
    let longestCommonPrefix words =

        let findShortestWord = List.minBy String.length
        let shortestWord = findShortestWord words

        let allWordsHaveSameChar index =
            words |> List.forall (fun word -> word[index] = shortestWord[index])

        let rec findPrefixLength i =
            let endOfString = i >= shortestWord.Length
            let someWordHasWrongSymbol = i |> allWordsHaveSameChar |> not

            if endOfString || someWordHasWrongSymbol then
                i
            else
                findPrefixLength (i + 1)

        0
        |> findPrefixLength
        |> function
            | 0 -> ""
            | length -> shortestWord.Substring(0, length)

    // Задача FizzBuzz
    let FizzBuzz n =

        let convert =
            function
            | k when k % 3 = 0 && k % 5 <> 0 -> "Fizz"
            | k when k % 5 = 0 && k % 3 <> 0 -> "Buzz"
            | k when k % 3 = 0 && k % 5 = 0 -> "FizzBuzz"
            | i -> $"{i}"

        [ 1..n ] |> List.map convert

    // Являются ли два слова анаграммами друг друга?
    let IsAnagram s t =

        let setNewValue =
            function
            | Some i -> i + 1
            | None -> 1
            >> Some

        let rec fillMap i value dict =
            if i >= String.length value then
                dict
            else
                dict |> Map.change value[i] setNewValue |> fillMap (i + 1) value

        let convertToMap word = fillMap 0 word (Map [])

        let containsKeyValuePair dict key value =
            dict |> Map.tryFind key |> Option.contains value

        let mapsAreEqual dict1 dict2 =
            dict1 |> Map.forall (containsKeyValuePair dict2)

        (convertToMap s, convertToMap t) ||> mapsAreEqual

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

    // Перевод римских цифр в арабские
    let RomanToArabic s =

        let dict =
            Map
                [ ('I', 1)
                  ('V', 5)
                  ('X', 10)
                  ('L', 50)
                  ('C', 100)
                  ('D', 500)
                  ('M', 1000) ]

        let convert =
            function
            | [ digit ] -> dict[digit]
            | [ 'I'; 'V' ] -> 4
            | [ 'I'; 'X' ] -> 9
            | [ 'I'; digit ] -> 1 + dict[digit]
            | [ 'X'; 'L' ] -> 40
            | [ 'X'; 'C' ] -> 90
            | [ 'X'; digit ] -> 10 + dict[digit]
            | [ 'C'; 'D' ] -> 400
            | [ 'C'; 'M' ] -> 900
            | [ 'C'; digit ] -> 100 + dict[digit]
            | [ a; b ] -> dict[a] + dict[b]
            | _ -> failwith "Wrong input data!"

        s
        |> Seq.toList
        |> List.rev
        |> List.chunkBySize 2
        |> List.map (List.rev >> convert)
        |> List.sum

    // Перевод числа в номер колонки Excel
    let ExcelSheetColumnTitle columnNumber =

        let digitToLetter k =
            System.Char.ConvertFromUtf32(k + 65) |> string

        let N = 26

        let rec convert col s =
            if col <= 0 then
                s
            else
                let div = (col - 1) / N
                let rem = (col - 1) % N
                rem |> digitToLetter |> convert div |> (fun k -> k + s)

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
