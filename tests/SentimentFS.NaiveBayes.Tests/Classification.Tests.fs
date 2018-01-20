namespace SentimentFS.NaiveBayes.Tests

open Expecto
open SentimentFS.NaiveBayes.Dto
open SentimentFS.NaiveBayes.Training
open SentimentFS.NaiveBayes.Classification
open Swensen.Unquote
open System.Linq.Expressions

type Emotion =
    | Negative
    | Positive

type Fruit =
    | Apple
    | Orange
    | Banana

type Country =
    | Yes
    | No

module Classifier =

    [<Tests>]
    let tests =
        testList "Classifier" [
            testList "Multinominal" [
                testCase "get category Probability" <| fun _ ->
                    let subject = ClassifierState.empty(None)
                                    |> Trainer.train({ value = "Chinese Beijing Chinese"; category = Yes; weight = None })
                                    |> Trainer.train({ value = "Chinese Chinese Shanghai"; category = Yes; weight = None })
                                    |> Trainer.train({ value = "Chinese Macao"; category = Yes; weight = None })
                                    |> Trainer.train({ value = "Tokyo Japan Chinese"; category = No; weight = None })
                                    |> Multinominal.categoryProbability

                    test <@ (subject(Yes)).Value >= 0.72 @>
                    test <@ (subject(Yes)).Value <= 0.73 @>
                    test <@ (subject(No)).Value >= 0.27 @>
                    test <@ (subject(No)).Value <= 0.28 @>
                testCase "P(Chinese|yes)" <| fun _ ->
                    let subject = ClassifierState.empty(None)
                                    |> Trainer.train({ value = "Chinese Beijing Chinese"; category = Yes; weight = None })
                                    |> Trainer.train({ value = "Chinese Chinese Shanghai"; category = Yes; weight = None })
                                    |> Trainer.train({ value = "Chinese Macao"; category = Yes; weight = None })
                                    |> Trainer.train({ value = "Tokyo Japan Chinese"; category = No; weight = None })
                                    |> Multinominal.countP "chinese" Yes
                    test <@ (subject).Value = (3.0/7.0) @>
            ]
        ]
