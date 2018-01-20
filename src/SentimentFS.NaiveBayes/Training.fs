namespace SentimentFS.NaiveBayes.Training
open SentimentFS.NaiveBayes.Dto
open SentimentFS.TextUtilities
open SentimentFS.Common

module Trainer =

    let parseTokens(config: Config)(word: string) =
        word
            |> Tokenizer.tokenize
            |> List.filterOut(config.stopWords)
            |> List.map(config.stem)

    let categorize(query: TrainingQuery<_>, tokens: string list)(state: ClassifierState<_>) =
        let accumulate = fun oldTokens -> tokens
                                            |> Map.mapValues(match query.weight with Some x -> x | None -> state.config.defaultWeight)
                                            |> Map.merge (fun (_, v1, v2) -> v1 + v2 ) oldTokens
        let newCategory =
            match state.categories.TryFind(query.category) with
            | Some cat ->
                { trainings = cat.trainings + 1; tokens = (cat.tokens |> accumulate) }
            | None ->
                { trainings = 1; tokens = Map.empty<string, int> |> accumulate }
        { state with categories = (state.categories.Add(query.category, newCategory)) }

    let addTokens(query: TrainingQuery<_>, tokens: string list)(state: ClassifierState<_>) =
        let accumulate = fun oldTokens -> tokens
                                            |> Map.mapValues(match query.weight with Some x -> x | None -> state.config.defaultWeight)
                                            |> Map.merge (fun (_, v1, v2) -> v1 + v2 ) oldTokens
        { state with tokens = (state.tokens |> accumulate) }

    let train(query: TrainingQuery<_>) (state: ClassifierState<_>) =
        let parsedTokens = (query.value) |> parseTokens(state.config)
        let newState = state |> categorize(query, parsedTokens) |> addTokens(query, parsedTokens) |> ClassifierState.incrementTrainings
        newState


