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

    Assert.That(Easy.IsAnagram "anagram" "nagaram")
    Assert.That(Easy.IsAnagram "rat" "car" |> not)

[<Test>]
let RomanToArabicTest () =

    Assert.AreEqual(3, Easy.RomanToArabic "III")
    Assert.AreEqual(58, Easy.RomanToArabic "LVIII")
    Assert.AreEqual(1994, Easy.RomanToArabic "MCMXCIV")
    Assert.AreEqual(3999, Easy.RomanToArabic "MMMCMXCIX")
    Assert.AreEqual(2000, Easy.RomanToArabic "MM")

[<Test>]
let LongestCommonPrefixTest () =

    Assert.AreEqual("fl", Easy.longestCommonPrefix [ "flower"; "flow"; "flight" ])
    Assert.AreEqual("", Easy.longestCommonPrefix [ "dog"; "racecar"; "car" ])
    Assert.AreEqual("a", Easy.longestCommonPrefix [ "a"; "a"; "a" ])
    Assert.AreEqual("", Easy.longestCommonPrefix [ ""; "abc"; "ert" ])

[<Test>]
let FizzBuzzTest () =

    Assert.AreEqual([ "1"; "2"; "Fizz" ], Easy.FizzBuzz 3)
    Assert.AreEqual([ "1"; "2"; "Fizz"; "4"; "Buzz" ], Easy.FizzBuzz 5)

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
    Assert.AreEqual("A", Easy.ExcelSheetColumnTitle 1)
    Assert.AreEqual("AB", Easy.ExcelSheetColumnTitle 28)
    Assert.AreEqual("ZY", Easy.ExcelSheetColumnTitle 701)
    Assert.AreEqual("AAZ", Easy.ExcelSheetColumnTitle 728)

[<Test>]
let IsPalindromeTest () =
    Assert.IsTrue(Easy.IsPalindrome "A man, a plan, a canal: Panama")
    Assert.IsFalse(Easy.IsPalindrome "race a car")

[<Test>]
let IndexOfFirstOccurrenceTest () =
    Assert.AreEqual(0, Easy.IndexOfFirstOccurrence "sadbutsad" "sad")
    Assert.AreEqual(-1, Easy.IndexOfFirstOccurrence "leetcode" "leeto")
    Assert.AreEqual(2, Easy.IndexOfFirstOccurrence "leetcode" "etc")

[<Test>]
let LengthOfLastWordTest () =
    Assert.AreEqual(5, Easy.LengthOfLastWord "Hello World")
    Assert.AreEqual(4, Easy.LengthOfLastWord "   fly me   to   the moon  ")
    Assert.AreEqual(6, Easy.LengthOfLastWord "luffy is still joyboy")

[<Test>]
let ArabicToRomanTest () =
    Assert.AreEqual("MMMDCCXLIX", Medium.ArabicToRoman 3749)
    Assert.AreEqual("LVIII", Medium.ArabicToRoman 58)
    Assert.AreEqual("MCMXCIV", Medium.ArabicToRoman 1994)
