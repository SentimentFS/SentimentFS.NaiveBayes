namespace SentimentFS.NaiveBayes.Classification

module NaiveProbability =
    let internal apriori = 2
    let internal aposterori = 2
    let compute = 2

module Classifier =
    open SentimentFS.NaiveBayes.Dto
    open SentimentFS.Core.Caching
    let classify(element: _)(cache: Cache<State<_>>) = { score = Map.empty<_, double> }
