module FuzzyAutoCompleteBox

open System
open System.Collections.Generic
open Avalonia.Controls
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open FuzzySharp
open FuzzySharp.SimilarityRatio
open FuzzySharp.SimilarityRatio.Scorer
open FuzzySharp.SimilarityRatio.Scorer.Composite
open FuzzySharp.SimilarityRatio.Scorer.StrategySensitive

type Item =
    { DisplayText: string }
    
type FuzzyAutoCompleteBoxState = {
    Items: Item list
    FilteredItems: Item list
    SelectedItem: Item option
    DEBUG_MSG : string
}

let InitialState = {
    Items = list.Empty
    FilteredItems = list.Empty
    SelectedItem = None
    DEBUG_MSG = "init"
}
    
type FuzzyAutoCompleteBoxMsg =
    | UpdateSelectedItem of string option
    | UpdateMatchedItems of string
    | RunSelectedItem
    
type FuzzyAutoCompleteBox() =
    inherit AutoCompleteBox()
    
type FuzzyAutoCompleteBox with        
    static member dataItems<'t when 't :> AutoCompleteBox>(items: IEnumerable<Item>) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<System.Collections.IEnumerable>(
            AutoCompleteBox.ItemsProperty,
            (items |> Seq.map (fun c -> c.DisplayText)) ,
            ValueSome (fun (input: obj*obj) ->
                let s1 = input |> fst :?> IEnumerable<_>
                let s2 = input |> snd :?> IEnumerable<_>
                Seq.length s1 = Seq.length s2 && Seq.forall2 (fun i1 i2 -> i1.Equals i2) s1 s2))
        
let update (msg: FuzzyAutoCompleteBoxMsg) (state: FuzzyAutoCompleteBoxState) : FuzzyAutoCompleteBoxState =
    match msg with
    | UpdateMatchedItems input ->
        if input.Length < 2 then
            { state with FilteredItems= List.empty }
        else
            let identifyMatchedCommands =
                let sortedCommands = state.Items |> List.sort
                let score (scorer: IRatioScorer)=
                   ( Process.ExtractAll(
                        input,
                        sortedCommands |> List.map (fun c -> c.DisplayText),
                        scorer= scorer)
                     |> Seq.map (fun r -> r.Score))
                fun input -> 
                    sortedCommands
                    |> Seq.zip3
                       (ScorerCache.Get<WeightedRatioScorer>() |> score)
                       (ScorerCache.Get<TokenInitialismScorer>() |> score)
                    |> Seq.sortByDescending (fun ( f, s, t ) -> (f, s))
                    |> Seq.filter (fun (weightedScore, tokenScore, _) -> weightedScore > 75 || tokenScore > 50) 
                    |> Seq.map (fun (_, _, item) -> item)
        
            let filteredItems = identifyMatchedCommands input
            { state with FilteredItems = filteredItems |> Seq.toList }
        
    | UpdateSelectedItem displayText ->
        match displayText with
        | Some displayTextValue -> 
            { state with SelectedItem= state.FilteredItems |> List.tryFind (fun c -> c.DisplayText = displayTextValue) }
        | None -> { state with SelectedItem= None }
        
    | RunSelectedItem ->
        match state.SelectedItem with
        | Some item -> { state with DEBUG_MSG= sprintf "%A Run already selected command %A" DateTime.Now.Millisecond item }
        | None -> { state with DEBUG_MSG= sprintf "%A Run best matched command %A" (DateTime.Now.Millisecond) (state.FilteredItems |> List.tryHead) }

let create (state: FuzzyAutoCompleteBoxState) dispatch (attrs: IAttr<AutoCompleteBox> list): IView<AutoCompleteBox> =
    AutoCompleteBox.create (attrs@[
          FuzzyAutoCompleteBox.dataItems state.FilteredItems
          FuzzyAutoCompleteBox.filterMode AutoCompleteFilterMode.Custom
          FuzzyAutoCompleteBox.itemFilter (fun (_:string) (_:obj) -> true)
          FuzzyAutoCompleteBox.onSearchTextChanged (fun searchText ->
                        dispatch (UpdateMatchedItems searchText))
          FuzzyAutoCompleteBox.onSelectedItemChanged (fun item ->
                        match item with
                        | :? string as commandName -> dispatch (UpdateSelectedItem (Some commandName))
                        | _ -> dispatch (UpdateSelectedItem None))
          FuzzyAutoCompleteBox.onKeyDown (fun e ->
                        match e.Key with
                        | Avalonia.Input.Key.Enter -> dispatch RunSelectedItem
                        | _ -> ()) ])

