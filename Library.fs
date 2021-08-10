namespace Fable.Reflux

type ListenerId = ListenerId of int

type ('state, 'action) Store(reducer: 'state -> 'action -> 'state, defaultState: 'state) =
    let mutable state = defaultState
    
    let mutable listeners = Map.empty
    
    let mutable nextListenerId = 0
    
    let notifySubscribers () =
        for (_, callback) in Map.toSeq listeners do
            callback state
    
    member this.dispatch(action: 'action) =
        state <- reducer state action
    
    member this.getState() = state
    
    member this.subscribe(callback: 'state -> unit) =
        let id = nextListenerId
        nextListenerId <- nextListenerId + 1
        listeners <- Map.add (ListenerId id) callback listeners
        (ListenerId id)
    
    member this.unsubscribe(id) = listeners <- Map.remove id listeners
