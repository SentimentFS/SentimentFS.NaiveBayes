namespace SentimentFS.NaiveBayes.Tests

open Expecto
open SentimentFS.NaiveBayes.Dto

module State =

    [<Tests>]
    let tests =
        testList "State" [
            testList "empty" [
                testCase "test get empty state function" <| fun _ ->
                    let subject: ClassifierState<int>  = ClassifierState.empty()
                    Expect.equal subject ({ categories = Map.empty<int, Category>; trainings = 0 }) "should equal"
            ]

            testList "incrementTrainings" [
                testCase "test incrementTrainings in empty state function" <| fun _ ->
                    let subject: ClassifierState<int>  = ClassifierState.empty() |> ClassifierState.incrementTrainings
                    Expect.equal subject ({ categories = Map.empty<int, Category>; trainings = 1 }) "should equal"
            ]
        ]
