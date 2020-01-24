module Benchmarks exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)


reverseRange : Int -> Int -> List Int
reverseRange hi lo =
    reverseRangeHelp hi lo []


reverseRangeHelp : Int -> Int -> List Int -> List Int
reverseRangeHelp hi lo list =
    if hi >= lo then
        reverseRangeHelp hi (lo + 1) (lo :: list)

    else
        list


simpleReverseRange : Int -> Int -> List Int
simpleReverseRange hi lo =
    List.reverse (List.range lo hi)


reverseRangeBenchmark : Benchmark
reverseRangeBenchmark =
    let
        lo =
            1

        lowHi =
            10

        mediumHi =
            100

        highHi =
            10000
    in
    describe "Reverse Range Benchmarks"
        [ Benchmark.compare "reverse range implementations, low range"
            "reverseRange"
            (\_ -> reverseRange lowHi lo)
            "simpleReverseRangeBenchmark"
            (\_ -> simpleReverseRange lowHi lo)
        , Benchmark.compare "reverse range implementations, medium range"
            "reverseRange"
            (\_ -> reverseRange mediumHi lo)
            "simpleReverseRangeBenchmark"
            (\_ -> simpleReverseRange mediumHi lo)
        , Benchmark.compare "reverse range implementations, high range"
            "reverseRange"
            (\_ -> reverseRange highHi lo)
            "simpleReverseRangeBenchmark"
            (\_ -> simpleReverseRange highHi lo)
        ]


main : BenchmarkProgram
main =
    program <|
        Benchmark.describe "sample"
            [ reverseRangeBenchmark
            ]
