namespace SentimentFS.NaiveBayes.Tests

open Expecto
open SentimentFS.NaiveBayes.Dto
open Swensen.Unquote

module State =

    [<Tests>]
    let tests =
        testList "State" [
            testList "empty" [
                testCase "test get empty state function" <| fun _ ->
                    let subject: ClassifierState<int>  = ClassifierState.empty(None)
                    test <@  subject.trainings = 0 @>
            ]

            testList "incrementTrainings" [
                testCase "test incrementTrainings in empty state function" <| fun _ ->
                    let subjectBefore: ClassifierState<int>  = ClassifierState.empty(None)
                    test <@  subjectBefore.trainings = 0 @>
                    let subjectAfter = subjectBefore |> ClassifierState.incrementTrainings
                    test <@  subjectAfter.trainings = 1 @>
            ]
        ]
