namespace SentimentFS.NaiveBayes.Tests

open Expecto
open SentimentFS.NaiveBayes.Dto
open Expecto.Flip

module State =

    [<Tests>]
    let tests =
        testList "State" [
            testList "empty" [
                testCase "test get empty state function" <| fun _ ->
                    let subject: ClassifierState<int>  = ClassifierState.empty(None)
                    Expect.equal "" subject.categories (Map.empty<int, Category>)
                    Expect.equal "" subject ({ categories = Map.empty<int, Category>; trainings = 0; config = Config.empty() })
            ]

            testList "incrementTrainings" [
                testCase "test incrementTrainings in empty state function" <| fun _ ->
                    let subject: ClassifierState<int>  = ClassifierState.empty(None) |> ClassifierState.incrementTrainings
                    Expect.equal subject ({ categories = Map.empty<int, Category>; trainings = 1; config = Config.empty() }) "should equal"
            ]
        ]
