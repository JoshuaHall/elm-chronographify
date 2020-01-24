module Benchmarks exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Main exposing (reverseRange)


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
