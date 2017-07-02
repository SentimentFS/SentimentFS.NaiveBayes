namespace SentimentFS.NaiveBayes.Tests

open Expecto
open SentimentFS.NaiveBayes.Dto
open SentimentFS.NaiveBayes.Training
open SentimentFS.NaiveBayes.Classification

type Emotion =
    | Negative
    | Positive

type Fruit =
    | Apple
    | Orange
    | Banana

module Classifier =

    [<Tests>]
    let tests =
        testList "Classifier" [
            testList "Naive" [
                testCase "test when text is negative" <| fun _ ->
                    let positiveText = "I love fsharp"
                    let negativeText = "I hate java"
                    let subject = Trainer.init<Emotion>(None)
                                    |> Trainer.train({ value = positiveText; category = Positive; weight = None })
                                    |> Trainer.train({ value = negativeText; category = Negative; weight = None })
                                    |> Classifier.classify("My brother hate java")

                    Expect.equal (subject.score.TryFind(Positive)) (Some 0.0) "should equal"
                    Expect.floatClose Accuracy.low (subject.score.TryFind(Negative).Value) (0.05555555556) "should equal"
                testCase "test when text is positive" <| fun _ ->
                    let positiveText = "I love fsharp"
                    let negativeText = "I hate java"
                    let subject = Trainer.init<Emotion>(None)
                                    |> Trainer.train({ value = positiveText; category = Positive; weight = None })
                                    |> Trainer.train({ value = negativeText; category = Negative; weight = None })
                                    |> Classifier.classify("My brother love fsharp")

                    Expect.equal (subject.score.TryFind(Negative)) (Some 0.0) "should equal"
                    Expect.floatClose Accuracy.low (subject.score.TryFind(Positive).Value) (0.05555555556) "should equal"
                test "Fruit classification" {
                    let subject = Trainer.init<Fruit>(None)
                                    |> Trainer.train({ value = "red sweet"; category = Apple; weight = Some 2 })
                                    |> Trainer.train({ value = "green"; category = Apple; weight = None })
                                    |> Trainer.train({ value = "round"; category = Apple; weight = Some 4 })
                                    |> Trainer.train({ value = "sweet"; category = Banana; weight = Some 2 })
                                    |> Trainer.train({ value = "green"; category = Banana; weight = None })
                                    |> Trainer.train({ value = "yellow long"; category = Banana; weight = Some 4 })
                                    |> Trainer.train({ value = "red"; category = Orange; weight = Some 2 })
                                    |> Trainer.train({ value = "yellow sweet"; category = Orange; weight = None })
                                    |> Trainer.train({ value = "round"; category = Orange; weight = Some 4 })
                                    |> Classifier.classify("Maybe green maybe red but definitely round and sweet.")

                    Expect.isOk (Ok(2)) "should be ok"
                }
            ]
        ]
