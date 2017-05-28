namespace SentimentFS.NaiveBayes.Training

module Trainer =
    open SentimentFS.NaiveBayes.Dto
    open SentimentFS.Core.Caching
    open SentimentFS.Core
    open SentimentFS.TextUtilities

    [<Literal>]
    let internal StateKey = "SentimentFS.NaiveBayes.Training.Trainer.StateKey"

    let init<'a when 'a : comparison>(config: Config option) =
        (Cache.empty<State<'a>>(), match config with Some c -> c | None -> Config.Default())

    let parseTokens(config: Config)(word: string) =
        let result = word
                        |> Tokenizer.tokenize
                        |> Filter.filterOut(config.stopWords)
                        |> List.map(config.stem)
        result

    let incrementTrainings(state: State<_>) =
        { state with trainings = state.trainings + 1  }

    let train(query: TrainingQuery<_>)(cache: Cache<State<_>>, config: Config) =
        let state =
           match cache |> Cache.get(StateKey) with
           | Some oldState -> oldState
           | None -> State.empty()
        let tokens = (query.value) |> parseTokens(config)
        let newState = state |> incrementTrainings
        (cache |> Cache.set(StateKey, newState), config)


    let get(cache: Cache<State<_>>, _: Config) =
        cache |> Cache.get(StateKey)

