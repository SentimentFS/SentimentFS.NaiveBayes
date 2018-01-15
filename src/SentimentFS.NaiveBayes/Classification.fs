namespace SentimentFS.NaiveBayes.Classification

module NaiveProbability =
    open SentimentFS.NaiveBayes.Dto
    let apriori (state: ClassifierState<_>) =
        let l = state.categories |> Map.toList
        let allTokensQuantity = l |> List.sumBy (fun (_,v) -> ( v.tokens |> Map.toList |> List.sumBy (fun (_, v1) -> v1 |> float)))
        l |> List.map(fun (k,v) -> (k, ((v.tokens |> Map.toList |> List.sumBy (fun (_, v1) -> v1)) |> float) / allTokensQuantity)) |> Map.ofList

    let aposterori (element: _) (category: _) (state: ClassifierState<_>) =
        match state.categories.TryFind(category) with
        | Some cat ->
            match cat.tokens.TryFind(element) with
            | Some v ->
                let allQ = cat.tokens |> Map.toList |> List.sumBy (fun (_,v) -> v) |> float
                (v |> float) / allQ
            | None -> 1.0
        | None -> 1.0

    let compute (elements: _ list) (state: ClassifierState<_>) =
        let func cat = (elements |> List.map(fun x -> (aposterori x cat state)) |> List.fold(( * )) 1.0)
        let aprioriP = state |> apriori
        state.categories
                |> Map.toList
                |> List.map(fun (k, _) -> (k, (match (func k) with | prob when prob = 1.0 -> 0.0 | prob -> prob * match aprioriP.TryFind(k) with Some vl -> vl | None -> 1.0)))
                |> Map.ofList


module Classifier =
    open SentimentFS.NaiveBayes.Dto
    open SentimentFS.TextUtilities
    open SentimentFS.Common

    let internal parseTokens(config: Config)(word: string) =
        let result = word
                        |> Tokenizer.tokenize
                        |> List.filterOut(config.stopWords)
                        |> List.map(config.stem)
        result

    let classify(element: _) (state: ClassifierState<_>)  =
        let tokens = element |> parseTokens(state.config)
        match state.config.model with
        | _ ->
            { score = (NaiveProbability.compute(tokens)(state)) }
