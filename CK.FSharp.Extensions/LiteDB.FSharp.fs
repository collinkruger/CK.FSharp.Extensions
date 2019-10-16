/// Extensions To LiteDB.FSharp https://github.com/Zaid-Ajaj/LiteDB.FSharp
module LiteDB.FSharp
open LiteDB

/// Type Used Internally By Collection Functions.
/// There Shouldn't Be A Reason To Consume This Publicly.
/// This Type Must Be Public For The Serializer To Behave Correctly.
type IDed<'T> = {
    Id: int
    Value: 'T
}

type LiteSingleItemCollection<'T>(col: LiteCollection<IDed<'T>>) =
    member _.get () =
        col.FindAll()
           |> Seq.map (fun x -> x.Value)
           |> Seq.tryHead

    member _.set (value: 'T) =
        col.Upsert({ Id = 1; Value = value })
           |> ignore

    member _.delete () =
        col.Delete(BsonValue 1)
           |> ignore

type LiteDatabase with
    member this.SingleCollection<'T>(name: string) =
        this.GetCollection<IDed<'T>>(name)
        |> LiteSingleItemCollection