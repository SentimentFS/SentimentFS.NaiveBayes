namespace SentimentFS.NaiveBayes.Dto

type ProbabilityModel =
    | Naive

type ClassificationScore<'a when 'a : comparison>  = { score: Map<'a, float> }

type TrainingQuery<'a when 'a : comparison> = { value: string; category: 'a; weight : int option }

type Category = { trainings: int; tokens: Map<string, int> }

type State<'a when 'a : comparison> = { categories: Map<'a, Category>; trainings: int }

type Config = { model : ProbabilityModel; defaultWeight: int; stem: string -> string; stopWords: string list }
    with static member Default() = { stem = id; stopWords = []; model = ProbabilityModel.Naive; defaultWeight = 1 }

module State =

    [<CompiledName("Empty")>]
    let empty() = { categories = Map.empty<'a, Category>; trainings = 0 }

    let incrementTrainings(state: State<_>) =
        { state with trainings = state.trainings + 1}
