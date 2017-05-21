namespace SentimentFS.NaiveBayes.Dto

type TrainingQuery<'a, 'b when 'a : comparison and 'b : comparison> = { value: 'a; category: 'b; weight : int option }

type Category<'a when 'a: comparison> = { trainings: int; tokens: Map<'a, int> }

type State<'a, 'b when 'a : comparison and 'b : comparison> = { categories: Map<'a, Category<'b>> }

module State = 
    
    [<CompiledName("Empty")>]
    let empty() = { categories = Map.empty<'a, Category<'b>> }
