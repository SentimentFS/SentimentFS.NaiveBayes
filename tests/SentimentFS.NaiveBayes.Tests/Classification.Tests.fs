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
    let probabilityCountTests =
        testList "Probabilities" [
            testList "Naive P(A|Bi) = TTP(Bi|A) * P(A)" [
                testList "P(A)" [
                    test "apriori" {
                        let state =  ClassifierState.empty(None)
                                                    |> Trainer.train({ value = "positiveText"; category = Positive; weight = None })
                                                    |> Trainer.train({ value = "negativeText"; category = Negative; weight = None })
                        let subject = state |> NaiveProbability.apriori
                        Expect.equal subject.[Emotion.Negative] 0.5 "negative emotion probability should equal 0.5"
                        Expect.equal subject.[Emotion.Positive] 0.5 "positive emotion probability should equal 0.5"
                    }
                    test "apriori2" {
                        let state =  ClassifierState.empty(None)
                                                    |> Trainer.train({ value = "positiveText"; category = Positive; weight = None })
                                                    |> Trainer.train({ value = "negativeText"; category = Negative; weight = None })
                                                    |> Trainer.train({ value = "negativeText"; category = Negative; weight = None })
                                                    |> Trainer.train({ value = "negativeText"; category = Negative; weight = None })
                        let subject = state |> NaiveProbability.apriori
                        Expect.equal subject.[Emotion.Negative] 0.75 "negative emotion probability should equal 0.75"
                        Expect.equal subject.[Emotion.Positive] 0.25 "positive emotion probability should equal 0.25"
                    }
                ]
            ]
        ]


    [<Tests>]
    let tests =
        testList "Classifier" [
            testList "Naive" [
                testCase "test when text is negative" <| fun _ ->
                    let positiveText = "I love fsharp"
                    let negativeText = "I hate java"
                    let subject =  ClassifierState.empty(None)
                                    |> Trainer.train({ value = positiveText; category = Positive; weight = None })
                                    |> Trainer.train({ value = negativeText; category = Negative; weight = None })
                                    |> Classifier.classify("My brother hate java")

                    Expect.isGreaterThan (subject.score.TryFind(Negative).Value) (subject.score.TryFind(Positive).Value) "negative score should be greater than positive"
                testCase "test when text is positive" <| fun _ ->
                    let positiveText = "I love fsharp"
                    let negativeText = "I hate java"
                    let subject =  ClassifierState.empty(None)
                                    |> Trainer.train({ value = positiveText; category = Positive; weight = None })
                                    |> Trainer.train({ value = negativeText; category = Negative; weight = None })
                                    |> Classifier.classify("My brother love fsharp")

                    Expect.isGreaterThan (subject.score.TryFind(Positive).Value) (subject.score.TryFind(Negative).Value) "negative score should be greater than positive"
                test "Fruit classification" {
                    let subject =  ClassifierState.empty(None)
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
