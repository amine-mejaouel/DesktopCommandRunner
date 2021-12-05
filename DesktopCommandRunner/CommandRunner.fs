namespace DesktopCommandRunner

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open FuzzyAutoCompleteBox

module CommandRunner =

  type State =
    { CommandsState: FuzzyAutoCompleteBoxState }

  let init =
    { CommandsState =
        { InitialState with
            Items = seq {
              for i in [1..10] -> { DisplayText = (sprintf "Sample command %i" i) }
            } |> List.ofSeq } }

  type Msg = FuzzyAutoCompleteBoxMsg of FuzzyAutoCompleteBoxMsg

  let update (msg: Msg) (state: State) : State =
    match msg with
    | FuzzyAutoCompleteBoxMsg msg ->
      { state with
          CommandsState = update msg state.CommandsState }


  let view (state: State) (dispatch) =
    DockPanel.create [
      DockPanel.children [
        FuzzyAutoCompleteBox.create state.CommandsState (FuzzyAutoCompleteBoxMsg >> dispatch)
          [ AutoCompleteBox.dock Dock.Top
            AutoCompleteBox.background "Red"
            AutoCompleteBox.fontSize 24.0
            AutoCompleteBox.verticalAlignment VerticalAlignment.Center
            AutoCompleteBox.horizontalAlignment HorizontalAlignment.Stretch ]
        TextBlock.create
          [ TextBlock.text state.CommandsState.DEBUG_MSG ]] ]
