namespace SentimentFS.NaiveBayes.Dto

type ProbabilityModel =
    | Naive

type ClassificationScore<'a when 'a : comparison>  = { score: Map<'a, double> }

type TrainingQuery<'a when 'a : comparison> = { value: string; category: 'a; weight : int option }

type Category = { trainings: int; tokens: Map<string, int> }

type State<'a when 'a : comparison> = { categories: Map<'a, Category> }

type Config = { model : ProbabilityModel; defaultWeight: int; stem: string -> string; stopWords: string list }
    with static member Default() = { stem = id; stopWords = []; model = ProbabilityModel.Naive; defaultWeight = 1 }

module State = 
    
    [<CompiledName("Empty")>]
    let empty() = { categories = Map.empty<'a, Category> }
