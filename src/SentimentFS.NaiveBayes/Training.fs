namespace SentimentFS.NaiveBayes.Training

module Trainer =
    open SentimentFS.NaiveBayes.Dto
    open SentimentFS.Core.Caching

    [<Literal>]
    let internal StateKey = "SentimentFS.NaiveBayes.Training.Trainer.StateKey"

    let empty<'a, 'b when 'a : comparison and 'b : comparison>() = Cache.empty<State<'a,'b>>()

    let train(query: TrainingQuery<_,_>)(cache: Cache<State<_, _>>) = 
        cache

    let get(cache: Cache<State<_, _>>) =
        cache |> Cache.get(StateKey)

    