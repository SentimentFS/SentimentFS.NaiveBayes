namespace SentimentFS.NaiveBayes.Training

module Trainer =
    open SentimentFS.NaiveBayes.Dto
    open SentimentFS.Core.Caching

    let empty<'a, 'b when 'a : comparison and 'b : comparison>() = Cache.empty<State<'a,'b>>()

    let train(query: TrainingQuery<_,_>)(cache: Cache<State<_, _>>) = 
        cache