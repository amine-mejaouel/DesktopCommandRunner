namespace DesktopCommandRunner

open System
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open FuzzySharp
open FuzzySharp.SimilarityRatio
open FuzzySharp.SimilarityRatio.Scorer.Composite

module CommandRunner =
    
    type State =
        { Commands : Command list
          SelectedCommand : Command option
          Msg : string }
    
    let init =
        { Commands= []
          SelectedCommand= None
          Msg= "" }

    type Msg =
        | UpdateMatchedCommands of string
        | UpdateSelectedCommand of string option
        | RunCommand

    let update (msg: Msg) (state: State) : State =
        match msg with
        | UpdateMatchedCommands input ->
            if input.Length < 3 then
                { state with Commands= List.empty }
            else
                let identifyMatchedCommands =
                    let sortedCommands = commands |> List.sort
                    fun input -> 
                        sortedCommands
                        |> Seq.zip
                               ( Process.ExtractAll(
                                    input,
                                    sortedCommands |> List.map (fun c -> c.Name),
                                    scorer= ScorerCache.Get<WeightedRatioScorer>())
                                 |> Seq.map (fun r -> r.Score))
                        |> Seq.sortByDescending fst
                        |> Seq.map snd
                        |> Seq.toList
            
                let matchedCommands = identifyMatchedCommands input
                { state with Commands = matchedCommands }
            
        | UpdateSelectedCommand input ->
            match input with
            | Some cmdName -> 
                { state with SelectedCommand= state.Commands |> List.tryFind (fun c -> c.Name = cmdName) }
            | None -> { state with SelectedCommand= None }
            
        | RunCommand ->
            match state.SelectedCommand with
            | Some command -> { state with Msg= sprintf "%A Run already selected command %A" DateTime.Now.Millisecond command }
            | None -> { state with Msg= sprintf "%A Run best matched command %A" (DateTime.Now.Millisecond) (state.Commands |> List.tryHead) }
            
    let view (state: State) (dispatch) =
        DockPanel.create [
            DockPanel.children [
                AutoCompleteBox.create [
                    AutoCompleteBox.dock Dock.Top
                    AutoCompleteBox.fontSize 24.0
                    AutoCompleteBox.verticalAlignment VerticalAlignment.Center
                    AutoCompleteBox.horizontalAlignment HorizontalAlignment.Stretch
                    AutoCompleteBox.dataItems (state.Commands |> List.map (fun c -> c.Name))
                    AutoCompleteBox.filterMode AutoCompleteFilterMode.Custom
                    AutoCompleteBox.itemFilter (fun (_:string) (_:obj) -> true)
                    AutoCompleteBox.onSearchTextChanged (fun searchText ->
                        dispatch (UpdateMatchedCommands searchText))
                    AutoCompleteBox.onSelectedItemChanged (fun item ->
                        match item with
                        | :? string as commandName -> dispatch (UpdateSelectedCommand (Some commandName))
                        | _ -> dispatch (UpdateSelectedCommand None))
                    AutoCompleteBox.onKeyDown (fun e ->
                        match e.Key with
                        | Avalonia.Input.Key.Enter -> dispatch RunCommand
                        | _ -> ())
                ]       
                Button.create [
                    Button.dock Dock.Bottom
                    Button.content state.Msg
                ]                
            ]
        ]       
