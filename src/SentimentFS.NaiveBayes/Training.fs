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

    let parseTokens(word: string)(cache: Cache<State<_>>, config: Config) =
        let result = word 
                        |> Tokenizer.tokenize 
                        |> Filter.filterOut(config.stopWords) 
                        |> List.map(config.stem) 
        (result, cache, config)
    let train(query: TrainingQuery<_>)(cache: Cache<State<_>>, config: Config) = 
        (cache, config)

    let get(cache: Cache<State<_>>, _: Config) =
        cache |> Cache.get(StateKey)

    