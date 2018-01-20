namespace SentimentFS.NaiveBayes.Dto

type ProbabilityModel =
    | Multinominal
    | BinarizedMultinomial
    | Bernulli

type ClassificationScore<'a when 'a : comparison>  = { score: Map<'a, float> }

type TrainingQuery<'a when 'a : comparison> = { value: string; category: 'a; weight : int option }

type Category = { trainings: int; tokens: Map<string, int> }

type Config = { defaultWeight: int; stem: string -> string; stopWords: string list }

type ClassifierState<'a when 'a : comparison> = { categories: Map<'a, Category>; trainings: int; tokens: Map<string, int>; config: Config }


module Config =

    [<CompiledName("Empty")>]
    let empty() = { stem = id; stopWords = []; defaultWeight = 1 }

module Category =

    [<CompiledName("Empty")>]
    let empty() = { trainings = 0; tokens = Map.empty<string, int> }

    [<CompiledName("IncrementTrainings")>]
    let incrementTrainings(category: Category) =
        { category with trainings = category.trainings + 1 }


module ClassifierState =

    [<CompiledName("Empty")>]
    let empty(config: Config option) =
        match config with
        | Some conf ->
            { categories = Map.empty<'a, Category>; trainings = 0; tokens = Map.empty<string, int>; config = conf }
        | None ->
            { categories = Map.empty<'a, Category>; trainings = 0; tokens = Map.empty<string, int>; config = Config.empty() }

    [<CompiledName("IncrementTrainings")>]
    let incrementTrainings(state: ClassifierState<_>) =
        { state with trainings = state.trainings + 1 }
