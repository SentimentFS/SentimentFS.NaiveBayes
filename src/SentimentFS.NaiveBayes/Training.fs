namespace SentimentFS.NaiveBayes.Training
open SentimentFS.NaiveBayes.Dto
open SentimentFS.TextUtilities
open SentimentFS.Common

module Naive =
    let parseTokens(config: Config)(word: string) =
        let result = word
                        |> Tokenizer.tokenize
                        |> List.filterOut(config.stopWords)
                        |> List.map(config.stem)
        result

    let categorize(query: TrainingQuery<_>, tokens: string list)(config: Config, state: State<_>) =
        let accumulate = fun oldTokens -> tokens
                                            |> Map.mapValues(match query.weight with Some x -> x | None ->config.defaultWeight)
                                            |> Map.merge (fun (_, v1, v2) -> v1 + v2 ) oldTokens
        let newCategory =
            match state.categories.TryFind(query.category) with
            | Some cat ->
                { trainings = cat.trainings + 1; tokens = (cat.tokens |> accumulate) }
            | None ->
                { trainings = 1; tokens = Map.empty<string, int> |> accumulate }
        { state with categories = (state.categories.Add(query.category, newCategory)) }

    let train(query: TrainingQuery<_>) struct (stateOpt: State<_> option, config: Config) =
        let state =
           match stateOpt with
           | Some oldState -> oldState
           | None -> State.empty()
        let parsedTokens = (query.value) |> parseTokens(config)
        let newState = (config, state) |> categorize(query, parsedTokens) |> State.incrementTrainings
        struct (Some newState, config)

module Multinominal =

    let parseTokens(config: Config)(word: string) =
        let result = word
                        |> Tokenizer.tokenize
                        |> List.filterOut(config.stopWords)
                        |> List.map(config.stem)
        result

    let train(query: TrainingQuery<_>) struct (stateOpt: State<_> option, config: Config) =
        let state =
           match stateOpt with
           | Some oldState -> oldState
           | None -> State.empty()
        let parsedTokens = query.value |> parseTokens(config)
        struct (Some state, config)

module Trainer =
    let init<'a when 'a : comparison>(config: Config option): struct (State<'a> option * Config) =
        struct (None, match config with Some c -> c | None -> Config.Default())

    let train(query: TrainingQuery<_>) struct (stateOpt: State<_> option, config: Config) =
        match config.model with
        | Naive -> Naive.train query struct (stateOpt, config)
        | Multinominal -> Naive.train query struct (stateOpt, config)


