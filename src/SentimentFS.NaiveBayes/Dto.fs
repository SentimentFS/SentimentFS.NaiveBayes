namespace SentimentFS.NaiveBayes.Dto

type Category<'a when 'a: comparison> = { trainings: int; tokens: Map<'a, int> }

type State<'a, 'b when 'a : comparison and 'b : comparison> = { categories: Map<'a, Category<'b>>; trainings: int; tokens: Map<'b, int> }

module State = 
    
    [<CompiledName("Empty")>]
    let empty = { categories = Map.empty<'a, Category<'b>>; tokens = Map.empty<'b, int>; trainings = 0 }
