/// Extensions To LiteDB.FSharp https://github.com/Zaid-Ajaj/LiteDB.FSharp
module LiteDB.FSharp
open LiteDB

// NOTE: These extensions make use of anonymous records
//       to prevent introducing a named type that is of
//       little or no value to the consumers of these extensions

/// A Collection Of At Most One Item
type LiteSingleItemCollection<'T>(col: LiteCollection<{| Id: int; Value: 'T |}>) =
    member _.get () =
        col.FindAll()
           |> Seq.map (fun x -> x.Value)
           |> Seq.tryHead

    member _.set (value: 'T) =
        col.Upsert({| Id = 1; Value = value |})
           |> ignore

    member _.delete () =
        col.Delete(BsonValue 1)
           |> ignore

type LiteDatabase with
    /// Returns A Collection Of At Most One Item
    member this.SingleItemCollection<'T>(name: string) =
        this.GetCollection<{| Id: int; Value: 'T |}>(name)
        |> LiteSingleItemCollection