namespace SentimentFS.NaiveBayes.Classification

module Multinominal =
    open SentimentFS.NaiveBayes.Dto

    let categoryProbability (state: ClassifierState<_>) =
        let tokensQuantityByCategory = state.categories |> Map.map(fun _ v -> v.tokens |> Map.fold(fun acc _ v1 -> acc + (v1 |> float)) 0.0)
        let allTokensQuantity = tokensQuantityByCategory |> Map.fold(fun acc _ v -> acc + v) 0.0
        fun (category: _) ->
            tokensQuantityByCategory.TryFind(category)
            |> Option.map(fun value -> value / allTokensQuantity)

    let countP (element: _) (category :_) (state: ClassifierState<_>) =
        let b = state.tokens.Count |> float
        state.categories.TryFind(category)
            |> Option.map(fun cat ->
                            let allTokensQuantity = cat.tokens |> Map.fold(fun acc _ v1 -> acc + (v1 |> float)) 0.0
                            match cat.tokens.TryFind(element) with
                            | Some x ->
                                ((x |> float) + 1.0) / (allTokensQuantity + b)
                            | None ->
                                1.0 / (allTokensQuantity + b)
                          )
    let compute (tokens: _ list) (category: _) (state: ClassifierState<_>) =
        categoryProbability state category
            |> Option.map(fun catProb ->
                    let prob = tokens |> List.map((fun t -> countP t category state) >> (fun opt -> defaultArg opt 0.0)) |> List.fold(( * )) 1.0
                    prob * catProb
                )


module Classifier =
    open SentimentFS.NaiveBayes.Dto
    open SentimentFS.TextUtilities
    open SentimentFS.Common

    let internal parseTokens(config: Config)(word: string) =
        word
            |> Tokenizer.tokenize
            |> List.filterOut(config.stopWords)
            |> List.map(config.stem)

    let classify (element: _) (model: ProbabilityModel) (state: ClassifierState<_>)  =
        let tokens = element |> parseTokens(state.config)
        match model with
        | _ ->
            { score = Map.empty<_, float> }
