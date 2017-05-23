namespace SentimentFS.NaiveBayes.Dto

type ProbabilityModel =
    | Naive

type ClassificationScore<'a when 'a : comparison>  = { score: Map<'a, double> }

type TrainingQuery<'a, 'b when 'a : comparison and 'b : comparison> = { value: 'a; category: 'b; weight : int option }

type Category<'a when 'a: comparison> = { trainings: int; tokens: Map<'a, int> }

type State<'a, 'b when 'a : comparison and 'b : comparison> = { categories: Map<'a, Category<'b>> }

type Config = { model : ProbabilityModel; defaultWeight: int; stem: string -> string; stopWords: string list }
    with static member Default() = { stem = id; stopWords = []; model = ProbabilityModel.Naive; defaultWeight = 1 }

module State = 
    
    [<CompiledName("Empty")>]
    let empty() = { categories = Map.empty<'a, Category<'b>> }
