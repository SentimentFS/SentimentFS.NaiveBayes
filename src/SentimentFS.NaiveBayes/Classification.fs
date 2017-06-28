namespace SentimentFS.NaiveBayes.Classification

module NaiveProbability =
    open SentimentFS.NaiveBayes.Dto
    let internal apriori (state: State<_>) =
        let l = state.categories |> Map.toList
        let allTokensQuantity = l |> List.sumBy (fun (k,v) -> ( v.tokens |> Map.toList |> List.sumBy (fun (_, v1) -> v1 |> float)))
        l |> List.map(fun (k,v) -> (k, ((v.tokens |> Map.toList |> List.sumBy (fun (_, v1) -> v1)) |> float) / allTokensQuantity))
    let internal aposterori state = 2
    let compute  state = 2

module Classifier =
    open SentimentFS.NaiveBayes.Dto
    open SentimentFS.Core.Caching
    let classify(element: _)(cache: Cache<State<_>>) = { score = Map.empty<_, double> }
