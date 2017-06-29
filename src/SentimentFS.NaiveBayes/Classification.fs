namespace SentimentFS.NaiveBayes.Classification
open System

module NaiveProbability =
    open SentimentFS.NaiveBayes.Dto
    let internal apriori (state: State<_>) =
        let l = state.categories |> Map.toList
        let allTokensQuantity = l |> List.sumBy (fun (k,v) -> ( v.tokens |> Map.toList |> List.sumBy (fun (_, v1) -> v1 |> float)))
        l |> List.map(fun (k,v) -> (k, ((v.tokens |> Map.toList |> List.sumBy (fun (_, v1) -> v1)) |> float) / allTokensQuantity)) |> Map.ofList
    let internal aposterori (element: _) (category: _) (state: State<_>) =
        state.categories.TryFind(category)
            |> Option.bind(fun cat -> cat.tokens.TryFind(element))

    let compute (elements: _ list) (state: State<_>) =
        let func cat = (elements |> List.fold(fun acc x -> acc * (aposterori x cat state)) 1.0)
        let aprioriP = state |> apriori
        state.categories
                |> Map.toList
                |> List.map(fun (k, v) -> (k, (func k) * match aprioriP.TryFind(k) with Some vl -> vl | None -> 1.0))


module Classifier =
    open SentimentFS.NaiveBayes.Dto
    open SentimentFS.Core.Caching
    let classify(element: _)(cache: Cache<State<_>>) = { score = Map.empty<_, float> }
