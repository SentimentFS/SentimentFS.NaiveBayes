namespace SentimentFS.NaiveBayes.Tests

open Expecto
open SentimentFS.NaiveBayes.Dto
open SentimentFS.NaiveBayes.Training
open SentimentFS.NaiveBayes.Classification
open Swensen.Unquote

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
                    testCase "apriori" <| fun _ ->
                        let state =  ClassifierState.empty(None)
                                                    |> Trainer.train({ value = "positiveText"; category = Positive; weight = None })
                                                    |> Trainer.train({ value = "negativeText"; category = Negative; weight = None })
                        let subject = state |> NaiveProbability.apriori
                        test <@ subject.[Emotion.Negative] = 0.5 @>
                        test <@ subject.[Emotion.Positive] = 0.5 @>
                    testCase "apriori2" <| fun _ ->
                        let state =  ClassifierState.empty(None)
                                                    |> Trainer.train({ value = "positiveText"; category = Positive; weight = None })
                                                    |> Trainer.train({ value = "negativeText"; category = Negative; weight = None })
                                                    |> Trainer.train({ value = "negativeText"; category = Negative; weight = None })
                                                    |> Trainer.train({ value = "negativeText"; category = Negative; weight = None })
                        let subject = state |> NaiveProbability.apriori
                        test <@ subject.[Emotion.Negative] = 0.75 @>
                        test <@ subject.[Emotion.Positive] = 0.25 @>
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

                    test <@ subject.score.TryFind(Negative).Value > subject.score.TryFind(Positive).Value @>
                testCase "test when text is positive" <| fun _ ->
                    let positiveText = "I love fsharp"
                    let negativeText = "I hate java"
                    let subject =  ClassifierState.empty(None)
                                    |> Trainer.train({ value = positiveText; category = Positive; weight = None })
                                    |> Trainer.train({ value = negativeText; category = Negative; weight = None })
                                    |> Classifier.classify("My brother love fsharp")

                    test <@ subject.score.TryFind(Positive).Value > subject.score.TryFind(Negative).Value @>
                testCase "Fruit classification" <| fun _ ->
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
            ]
        ]
