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
                    let subject = Trainer.empty<int>(None) |> Trainer.get
                    Expect.isNone subject "should be None"
            ]
            testList "train" [
                testCase "test train function (one training)" <| fun _ -> 
                    let subject = Trainer.empty<int>(None) |> Trainer.train({ value = "test"; category = 2; weight = None }) |> Trainer.get
                    Expect.isSome subject "should be some"
                    let categoryOpt = subject.Value.categories.TryFind(2)
                    Expect.isSome categoryOpt "category of 2 should be some"
                    Expect.equal (categoryOpt.Value) ({ trainings = 1; tokens = ([("test", 1)] |> Map.ofList) }) "should"
                testCase "test train function (two training, one category)" <| fun _ -> 
                    let subject = Trainer.empty<int>(None) 
                                    |> Trainer.train({ value = "test"; category = 2; weight = None }) 
                                    |> Trainer.train({ value = "test2"; category = 2; weight = None })
                                    |> Trainer.get
                    Expect.isSome subject "should be some"
                    let categoryOpt = subject.Value.categories.TryFind(2)
                    Expect.isSome categoryOpt "category of 2 should be some"
                    Expect.equal (categoryOpt.Value) ({ trainings = 2; tokens = ([("test", 1); ("test2", 1)] |> Map.ofList) }) "should"
                testCase "test train function (two training, two category)" <| fun _ -> 
                    let subject = Trainer.empty<int>(None) 
                                    |> Trainer.train({ value = "test"; category = 2; weight = None }) 
                                    |> Trainer.train({ value = "test2"; category = 1; weight = None })
                                    |> Trainer.get
                    Expect.isSome subject "should be some"
                    
                    let categoryOpt = subject.Value.categories.TryFind(2)
                    Expect.isSome categoryOpt "category of 2 should be some"
                    Expect.equal (categoryOpt.Value) ({ trainings = 1; tokens = ([("test", 1)] |> Map.ofList) }) "should"

                    let categoryOpt2 = subject.Value.categories.TryFind(1)
                    Expect.isSome categoryOpt2 "category of 2 should be some"
                    Expect.equal (categoryOpt2.Value) ({ trainings = 1; tokens = ([("test2", 1)] |> Map.ofList) }) "should"
            ]
        ] 