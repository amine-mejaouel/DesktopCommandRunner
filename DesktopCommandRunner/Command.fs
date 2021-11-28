[<AutoOpen>]
module DesktopCommandRunner.Command

    open System

    [<CustomEquality;CustomComparison>]
    type Command =
        { Name: string
          Run: unit -> unit }
        
          interface IEquatable<Command> with
                member this.Equals other = other.Name.Equals this.Name
                
          interface IComparable with
                member this.CompareTo other =
                    this.Name.CompareTo (other :?> Command).Name
                
          override this.Equals other =
              match other with
              | :? Command as c -> (this :> IEquatable<_>).Equals c
              | _ -> false
          override this.GetHashCode () = this.Name.GetHashCode()
    

    let commands = [
      { Name= "Sample Command 1"
        Run= ignore }
      { Name= "Sample Command 2"
        Run= ignore }
      { Name= "Sample Command 3"
        Run= ignore } ]
