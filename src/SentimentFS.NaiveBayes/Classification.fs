namespace SentimentFS.NaiveBayes.Classification

module Classifier =
    open SentimentFS.NaiveBayes.Dto
    open SentimentFS.NaiveBayes.Training.StateCache
    let classify(element: _)(cache: Cache<_,_>) = { score = Map.empty<_, double> }
