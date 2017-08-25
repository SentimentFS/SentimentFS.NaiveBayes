namespace SentimentFS.NaiveBayes.Classification
open System

module NaiveProbability =
    open SentimentFS.NaiveBayes.Dto
    let internal apriori (state: State<_>) =
        let l = state.categories |> Map.toList
        let allTokensQuantity = l |> List.sumBy (fun (k,v) -> ( v.tokens |> Map.toList |> List.sumBy (fun (_, v1) -> v1 |> float)))
        l |> List.map(fun (k,v) -> (k, ((v.tokens |> Map.toList |> List.sumBy (fun (_, v1) -> v1)) |> float) / allTokensQuantity)) |> Map.ofList
    let internal aposterori (element: _) (category: _) (state: State<_>) =
        match state.categories.TryFind(category) with
        | Some cat ->
            match cat.tokens.TryFind(element) with
            | Some v ->
                let allQ = cat.tokens |> Map.toList |> List.sumBy (fun (_,v) -> v) |> float
                (v |> float) / allQ
            | None -> 1.0
        | None -> 1.0

    let compute (elements: _ list) (state: State<_>) =
        let func cat = (elements |> List.map(fun x -> (aposterori x cat state)) |> List.fold(( * )) 1.0)
        let aprioriP = state |> apriori
        state.categories
                |> Map.toList
                |> List.map(fun (k, v) -> (k, (match (func k) with | prob when prob = 1.0 -> 0.0 | prob -> prob * match aprioriP.TryFind(k) with Some vl -> vl | None -> 1.0)))
                |> Map.ofList


module Classifier =
    open SentimentFS.NaiveBayes.Dto
    open SentimentFS.Core.Caching
    open SentimentFS.Core
    open SentimentFS.TextUtilities
    open SentimentFS.NaiveBayes.Training
    let internal parseTokens(config: Config)(word: string) =
        let result = word
                        |> Tokenizer.tokenize
                        |> Filter.filterOut(config.stopWords)
                        |> List.map(config.stem)
        result

    let classify(element: _) struct (stateOpt: State<_> option, config: Config)  =
        let tokens = element |> parseTokens(config)
        match config.model with
        | _ ->
            match stateOpt  with
            | Some state ->
                { score = (NaiveProbability.compute(tokens)(state)) }
            | None -> { score = Map.empty<_, float> }
