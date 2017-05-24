namespace SentimentFS.NaiveBayes.Training

module Trainer =
    open SentimentFS.NaiveBayes.Dto
    open SentimentFS.Core.Caching

    [<Literal>]
    let internal StateKey = "SentimentFS.NaiveBayes.Training.Trainer.StateKey"

    let empty<'a, 'b when 'a : comparison and 'b : comparison>(config: Config option) = 
        (Cache.empty<State<'a,'b>>(), match config with Some c -> c | None -> Config.Default()) 

    let train(query: TrainingQuery<_,_>)(cache: Cache<State<_, _>>, config: Config) = 
        (cache, config)

    let get(cache: Cache<State<_, _>>, _: Config) =
        cache |> Cache.get(StateKey)

    