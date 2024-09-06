module LeetCodeTests

open NUnit.Framework
open LeetCode

[<SetUp>]
let Setup () = ()

[<Test>]
let MySqrtTest () =

    let N = 10000

    let inputData =
        List.init N (fun _ -> System.Random.Shared.Next(System.Int32.MaxValue))

    let myOutput = inputData |> List.map Easy.MySqrt
    let systemOutput = inputData |> List.map (float >> sqrt >> int)

    (myOutput, systemOutput)
    ||> List.compareWith (fun a b -> if a = b then 0 else -1)
    |> fun res -> res = 0 |> Assert.That

[<Test>]
let AnagramTest () =

    Assert.IsTrue(Easy.IsAnagram "anagram" "nagaram")
    Assert.IsFalse(Easy.IsAnagram "rat" "car")

[<Test>]
let RomanToArabicTest () =

    Assert.That(Easy.RomanToArabic "III", Is.EqualTo(3))
    Assert.That(Easy.RomanToArabic "LVIII", Is.EqualTo(58))
    Assert.That(Easy.RomanToArabic "MCMXCIV", Is.EqualTo(1994))
    Assert.That(Easy.RomanToArabic "MMMCMXCIX", Is.EqualTo(3999))
    Assert.That(Easy.RomanToArabic "MM", Is.EqualTo(2000))

[<Test>]
let LongestCommonPrefixTest () =

    Assert.That(Easy.longestCommonPrefix [| "flower"; "flow"; "flight" |], Is.EqualTo("fl"))
    Assert.That(Easy.longestCommonPrefix [| "dog"; "racecar"; "car" |], Is.EqualTo(""))
    Assert.That(Easy.longestCommonPrefix [| "a"; "a"; "a" |], Is.EqualTo("a"))
    Assert.That(Easy.longestCommonPrefix [| ""; "abc"; "ert" |], Is.EqualTo(""))

[<Test>]
let FizzBuzzTest () =

    Assert.That(Easy.FizzBuzz 3, Is.EqualTo([ "1"; "2"; "Fizz" ]))
    Assert.That(Easy.FizzBuzz 5, Is.EqualTo([ "1"; "2"; "Fizz"; "4"; "Buzz" ]))

    Assert.AreEqual(
        [ "1"
          "2"
          "Fizz"
          "4"
          "Buzz"
          "Fizz"
          "7"
          "8"
          "Fizz"
          "Buzz"
          "11"
          "Fizz"
          "13"
          "14"
          "FizzBuzz" ],
        Easy.FizzBuzz 15
    )

[<Test>]
let ExcelSheetColumnTitleTest () =
    Assert.That(Easy.ExcelSheetColumnTitle 1, Is.EqualTo("A"))
    Assert.That(Easy.ExcelSheetColumnTitle 28, Is.EqualTo("AB"))
    Assert.That(Easy.ExcelSheetColumnTitle 701, Is.EqualTo("ZY"))
    Assert.That(Easy.ExcelSheetColumnTitle 728, Is.EqualTo("AAZ"))

[<Test>]
let IsPalindromeTest () =
    Assert.IsTrue(Easy.IsPalindrome "A man, a plan, a canal: Panama")
    Assert.IsFalse(Easy.IsPalindrome "race a car")

[<Test>]
let IndexOfFirstOccurrenceTest () =
    Assert.That(Easy.IndexOfFirstOccurrence "sadbutsad" "sad", Is.EqualTo(0))
    Assert.That(Easy.IndexOfFirstOccurrence "leetcode" "leeto", Is.EqualTo(-1))
    Assert.That(Easy.IndexOfFirstOccurrence "leetcode" "etc", Is.EqualTo(2))

[<Test>]
let LengthOfLastWordTest () =
    Assert.That(Easy.LengthOfLastWord "Hello World", Is.EqualTo(5))
    Assert.That(Easy.LengthOfLastWord "   fly me   to   the moon  ", Is.EqualTo(4))
    Assert.That(Easy.LengthOfLastWord "luffy is still joyboy", Is.EqualTo(6))

[<Test>]
let ArabicToRomanTest () =
    Assert.That(Medium.ArabicToRoman 3749, Is.EqualTo("MMMDCCXLIX"))
    Assert.That(Medium.ArabicToRoman 58, Is.EqualTo("LVIII"))
    Assert.That(Medium.ArabicToRoman 1994, Is.EqualTo("MCMXCIV"))

[<Test>]
let HugeDownloadTest () =

    let files =
        [| "song.mp3"
           "problem.cfg"
           "problem"
           "picture.png"
           "problem.cfg"
           "sol123.c"
           "data.dat"
           "problem"
           "sol13.c"
           "sol123.c"
           "song.mp3" |]
        |> Easy.hugeDownload

    let result =
        [| "data.dat"
           "picture.png"
           "problem"
           "problem(1)"
           "problem(1).cfg"
           "problem.cfg"
           "sol123(1).c"
           "sol123.c"
           "sol13.c"
           "song(1).mp3"
           "song.mp3" |]

    CollectionAssert.AreEqual(files, result)

[<Test>]
let MessageVariantsTest () =
    Assert.That([ 3, 2; 3, 4; 1, 3; 1, 2; 6, 5 ] |> Medium.MessageVariants 1, Is.EqualTo 2)

    Assert.That(
        [ 1, 2; 1, 7; 2, 1; 3, 7; 4, 5; 4, 6; 5, 4; 5, 7; 6, 4; 7, 1; 7, 3; 7, 5 ]
        |> Medium.MessageVariants 1,
        Is.EqualTo 4
    )
