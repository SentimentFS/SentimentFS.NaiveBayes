namespace SentimentFS.NaiveBayes.Classification

module NaiveProbability =
    open SentimentFS.NaiveBayes.Dto

    let categoryProbability (state: ClassifierState<_>) =
        let tokensQuantityByCategory = state.categories |> Map.map(fun _ v -> v.tokens |> Map.fold(fun acc _ v1 -> acc + (v1 |> float)) 0.0)
        let allTokensQuantity = tokensQuantityByCategory |> Map.fold(fun acc _ v -> acc + v) 0.0
        fun (category: _) ->
            tokensQuantityByCategory.TryFind(category)
                |> Option.bind(fun value -> Some(value / allTokensQuantity))

    let wordWhenCategory (element: _) (category: _) (state: ClassifierState<_>) =
        state.categories.TryFind(category)
            |> Option.bind(fun cat -> cat.tokens.TryFind(element))
            |> Option.map(fun tokenQuantity ->
                                let catProb = categoryProbability state category
                                let allTokensQuantity = state.categories |> Map.map(fun _ v -> v.tokens |> Map.fold(fun acc _ v1 -> acc + (v1 |> float)) 0.0) |> Map.fold(fun acc _ v -> acc + v) 0.0
                                ((tokenQuantity |> float) / allTokensQuantity) / catProb.Value
                            )

    let compute (elements: _ list) (state: ClassifierState<_>) =
        // let func cat = (elements |> List.map(fun x -> (aposterori x cat state)) |> List.fold(( * )) 1.0)
        // let aprioriP = state |> categoryProbabity
        // state.categories
        //         |> Map.toList
        //         |> List.map(fun (k, _) -> (k, (match (func k) with | prob when prob = 1.0 -> 0.0 | prob -> prob * match aprioriP.TryFind(k) with Some vl -> vl | None -> 1.0)))
        //         |> Map.ofList
        Map.empty<_, float>


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
