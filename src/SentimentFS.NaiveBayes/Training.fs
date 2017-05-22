namespace SentimentFS.NaiveBayes.Training

module StateCache =
    open SentimentFS.NaiveBayes.Dto
    
    type Msg<'TValue> =
    | Add of string * 'TValue
    | Get of string * AsyncReplyChannel<option<'TValue>>
    | Clear

    type Cache<'a, 'b when 'a : comparison and 'b : comparison>  = MailboxProcessor<Msg<State<'a, 'b>>>

    let caching<'a, 'b when 'a : comparison and 'b : comparison>(): Cache<'a, 'b> = MailboxProcessor.Start(fun agent ->
        let rec loop(map : Map<string, 'TValue>) =
            async {
                let! msg = agent.Receive()
                match msg with
                | Add(key, value) -> 
                    return! loop(map.Add(key, value))
                | Get(key, repl) ->
                    match map.TryFind(key) with
                    | Some(value) -> 
                        repl.Reply(Some value) 
                        return! loop(map)
                    | None ->
                        repl.Reply(None) 
                        return! loop(map)
                | Clear -> 
                    return! loop(Map.empty<string, 'TValue>)}
        loop Map.empty<string, 'TValue> )

module Trainer =
    open SentimentFS.NaiveBayes.Dto
    open StateCache

    let empty<'a, 'b when 'a : comparison and 'b : comparison>() = caching<'a, 'b>()

    let train(query: TrainingQuery<_,_>)(cache: Cache<_,_>) = 
        cache