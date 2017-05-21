namespace SentimentFS.NaiveBayes.Classification

module Classifier =
    open SentimentFS.NaiveBayes.Dto
    let classify(element: _) = { score = Map.empty<_, double> }
