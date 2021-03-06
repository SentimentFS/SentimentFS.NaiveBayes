namespace SentimentFS.NaiveBayes.Tests

open Expecto
open SentimentFS.NaiveBayes.Training
open SentimentFS.NaiveBayes.Dto
open Swensen.Unquote
module Trainer =

    [<Tests>]
    let tests =
        testList "Trainer" [
            testList "parseTokens" [
               testCase "parse" <| fun _ ->
                  let tokens = "cute dog" |> Trainer.parseTokens(Config.empty())
                  Expect.equal (tokens) (["cute"; "dog"]) "tokens should has two keys"
            ]
            testList "incrementTrainings" [
                testCase "increment training" <| fun _ ->
                    let subject = ClassifierState.empty(None) |> ClassifierState.incrementTrainings
                    test <@ subject.trainings = 1 @>
            ]
            testList "addTokens" [
               testCase "test" <| fun _ ->
                   let state = {categories = ([(2, { trainings = 1; tokens = ([("test", 1)] |> Map.ofList) })] |> Map.ofList); tokens = ([("test", 1)] |> Map.ofList); trainings = 1; config = Config.empty()}
                   let subject =  state |> Trainer.addTokens({ value = ""; category = 2; weight = None }, ["cute"; "dog"])
                   let tokens = subject.tokens
                   test <@ tokens = (([("test", 1); ("cute", 1); ("dog", 1)] |> Map.ofList)) @>
            ]

            testList "categorize" [
               testCase "when category no exist in state" <| fun _ ->
                   let subject =  ClassifierState.empty(None) |> Trainer.categorize({ value = ""; category = 2; weight = None }, ["cute"; "dog"])
                   Expect.isTrue (subject.categories.ContainsKey(2)) "should contain 2"
                   Expect.equal ((subject.categories.[2]).trainings) 1 "trainings should equal 1"
                   Expect.equal ((subject.categories.[2]).tokens) ([("cute", 1); ("dog", 1)] |> Map.ofList) "trainings should equal 1"
               testCase "when category exist in state" <| fun _ ->
                   let state = {categories = ([(2, { trainings = 1; tokens = ([("test", 1)] |> Map.ofList) })] |> Map.ofList); tokens = ([("test", 1)] |> Map.ofList); trainings = 1; config = Config.empty()}
                   Expect.isTrue (state.categories.ContainsKey(2)) "should contain 2"
                   let subject =  state |> Trainer.categorize({ value = ""; category = 2; weight = None }, ["cute"; "dog"])
                   Expect.isTrue (subject.categories.ContainsKey(2)) "should contain 2"
                   Expect.equal ((subject.categories.[2]).trainings) 2 "trainings should equal 2"
                   Expect.equal ((subject.categories.[2]).tokens) ([("test", 1); ("cute", 1); ("dog", 1)] |> Map.ofList) ""
            ]
            testList "train" [
                testCase "test train function (one training)" <| fun _ ->
                    let subject = ClassifierState.empty(None) |> Trainer.train({ value = "test"; category = 2; weight = None })
                    Expect.equal (subject.trainings) (1) "trainings quantity should equal 1"
                    let categoryOpt = subject.categories.TryFind(2)
                    Expect.isSome categoryOpt "category of 2 should be some"
                    Expect.equal (categoryOpt.Value) ({ trainings = 1; tokens = ([("test", 1)] |> Map.ofList) }) "should"
                    let tokens = subject.tokens
                    test <@ tokens = ([("test", 1)] |> Map.ofList) @>

                testCase "test train function (two training, one category)" <| fun _ ->
                    let subject =  ClassifierState.empty(None)
                                                    |> Trainer.train({ value = "test"; category = 2; weight = None })
                                                    |> Trainer.train({ value = "test2"; category = 2; weight = None })
                    Expect.equal (subject.trainings) (2) "trainings quantity should equal 2"
                    let categoryOpt = subject.categories.TryFind(2)
                    Expect.isSome categoryOpt "category of 2 should be some"
                    Expect.equal (categoryOpt.Value) ({ trainings = 2; tokens = ([("test", 1); ("test2", 1)] |> Map.ofList) }) "should"

                    let tokens = subject.tokens
                    test <@ tokens = ([("test2", 1); ("test", 1)] |> Map.ofList) @>

                testCase "test train function (two training, two category)" <| fun _ ->
                        let subject =  ClassifierState.empty(None)
                                                    |> Trainer.train({ value = "test"; category = 2; weight = None })
                                                    |> Trainer.train({ value = "test2"; category = 1; weight = None })
                        Expect.equal (subject.trainings) (2) "trainings quantity should equal 2"

                        let categoryOpt = subject.categories.TryFind(2)
                        Expect.isSome categoryOpt "category of 2 should be some"
                        test <@ categoryOpt.Value = ({ trainings = 1; tokens = ([("test", 1)] |> Map.ofList) }) @>

                        let categoryOpt2 = subject.categories.TryFind(1)
                        Expect.isSome categoryOpt2 "category of 2 should be some"
                        test <@ categoryOpt2.Value = ({ trainings = 1; tokens = ([("test2", 1)] |> Map.ofList) }) @>

                        let tokens = subject.tokens
                        test <@ tokens = ([("test2", 1); ("test", 1)] |> Map.ofList) @>
                ]
        ]
