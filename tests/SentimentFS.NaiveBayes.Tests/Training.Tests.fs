namespace SentimentFS.NaiveBayes.Tests

open Expecto
open SentimentFS.Core.Caching
open SentimentFS.NaiveBayes.Training
open SentimentFS.NaiveBayes.Dto

module Trainer =

    [<Tests>]
    let tests = 
        testList "Trainer" [
            testList "empty" [
                testCase "test get empty trainer function" <| fun _ ->
                    let subject = Trainer.empty<int, string>() |> Trainer.get
                    Expect.isNone subject "should be None"
            ]
        ] 