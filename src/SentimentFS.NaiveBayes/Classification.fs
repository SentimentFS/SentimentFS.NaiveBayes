namespace SentimentFS.NaiveBayes.Classification

module Multinominal =
    open SentimentFS.NaiveBayes.Dto

    let a = 2

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
            { score = Map.empty<_, float> }
