namespace SentimentFS.NaiveBayes.Tests

open Expecto
open SentimentFS.NaiveBayes.Dto

module State =

    [<Tests>]
    let tests = 
        testList "State" [
            testList "empty" [
                testCase "test get empty state function" <| fun _ ->
                    let subject: State<int>  = State.empty()
                    Expect.equal subject ({ categories = Map.empty<int, Category> }) "should equal"
            ]
        ] 