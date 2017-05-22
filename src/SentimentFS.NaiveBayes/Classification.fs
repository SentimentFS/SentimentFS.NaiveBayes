namespace SentimentFS.NaiveBayes.Classification

module Classifier =
    open SentimentFS.NaiveBayes.Dto
    open SentimentFS.Core.Caching
    let classify(element: _)(cache: Cache<State<_,_>>) = { score = Map.empty<_, double> }
