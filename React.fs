module Fable.Reflux.React

open System
open Feliz
open Fable.Reflux

type UseStoreFn<'state, 'action> = unit -> Store<'state, 'action>

let makeStoreContext (store: Store<'state, 'action>) = React.createContext ("reflux-store", store)

let makeUseStore storeContext () = React.useContext (storeContext)

let makeUseDispatch (useStore: UseStoreFn<'state, 'action>) () =
  let store = useStore ()
  store.dispatch

let makeUseSelector (useStore: UseStoreFn<'state, 'action>) (selector: 'state -> 'a) =
  let store = useStore ()

  let (state, setState) =
    React.useState (store.getState () |> selector)

  React.useEffect (
    (fun () ->
      let listenerId =
        store.subscribe
          (fun newState ->
            let selectorState = newState |> selector
            if selectorState <> state then
              setState (selectorState))

      { new IDisposable with
          member this.Dispose() = store.unsubscribe (listenerId) }),
    [||]
  )

  state
