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
                        let func = NaiveProbability.categoryProbability(state)
                        let subjectPositive = func(Positive)
                        test <@ subjectPositive = Some(0.5) @>
                        let subjectNegaive = func(Negative)
                        test <@ subjectNegaive = Some(0.5) @>
                    testCase "apriori2" <| fun _ ->
                        let state =  ClassifierState.empty(None)
                                                    |> Trainer.train({ value = "positiveText"; category = Positive; weight = None })
                                                    |> Trainer.train({ value = "negativeText"; category = Negative; weight = None })
                                                    |> Trainer.train({ value = "negativeText"; category = Negative; weight = None })
                                                    |> Trainer.train({ value = "negativeText"; category = Negative; weight = None })
                        let func = NaiveProbability.categoryProbability(state)
                        let subjectPositive = func(Positive)
                        test <@ subjectPositive = Some(0.25) @>
                        let subjectNegaive = func(Negative)
                        test <@ subjectNegaive = Some(0.75) @>
                ]
                testList "P(Bi|A)" [
                    testCase "probability when category and text exists and has one element" <| fun _ ->
                        let state =  ClassifierState.empty(None)
                                                    |> Trainer.train({ value = "test"; category = Positive; weight = None })
                                                    |> Trainer.train({ value = "test"; category = Negative; weight = None })
                        let subjectPositive = NaiveProbability.wordWhenCategory ("test") (Positive) (state)
                        test <@ subjectPositive = Some(1.0) @>
                        let subjectNegaive = NaiveProbability.wordWhenCategory ("test") (Negative) (state)
                        test <@ subjectNegaive = Some(1.0) @>
                    testCase "probability when category and text exists and has two element" <| fun _ ->
                        let state =  ClassifierState.empty(None)
                                                    |> Trainer.train({ value = "test"; category = Positive; weight = Some(2) })
                                                    |> Trainer.train({ value = "test2"; category = Positive; weight = Some(2) })
                                                    |> Trainer.train({ value = "test"; category = Negative; weight = Some(2) })
                                                    |> Trainer.train({ value = "test2"; category = Negative; weight = Some(2) })
                        let subjectPositive = NaiveProbability.wordWhenCategory ("test") (Positive) (state)
                        test <@ subjectPositive = Some(0.5) @>
                        let subjectNegaive = NaiveProbability.wordWhenCategory ("test") (Negative) (state)
                        test <@ subjectNegaive = Some(0.5) @>
                    testCase "probability when category non exists in map" <| fun _ ->
                        let state =  ClassifierState.empty(None)
                                                    |> Trainer.train({ value = "test"; category = Positive; weight = Some(2) })
                                                    |> Trainer.train({ value = "test2"; category = Positive; weight = Some(2) })
                        let subject= NaiveProbability.wordWhenCategory ("test") (Negative) (state)
                        test <@ subject = None @>
                    testCase "probability when token non exists in map" <| fun _ ->
                        let state =  ClassifierState.empty(None)
                                                    |> Trainer.train({ value = "test"; category = Positive; weight = Some(2) })
                                                    |> Trainer.train({ value = "test2"; category = Positive; weight = Some(2) })
                        let subject= NaiveProbability.wordWhenCategory ("nonexistenttoken") (Positive) (state)
                        test <@ subject = None @>
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
