namespace SentimentFS.NaiveBayes.Classification

module Multinominal =
    open SentimentFS.NaiveBayes.Dto

    let categoryProbability (state: ClassifierState<_>) =
        let tokensQuantityByCategory = state.categories |> Map.map(fun _ v -> v.tokens |> Map.fold(fun acc _ v1 -> acc + (v1 |> float)) 0.0)
        let allTokensQuantity = tokensQuantityByCategory |> Map.fold(fun acc _ v -> acc + v) 0.0
        fun (category: _) ->
            tokensQuantityByCategory.TryFind(category)
            |> Option.bind(fun value -> Some(value / allTokensQuantity))

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
