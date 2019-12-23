/// A Small Logging Implementations With Zero Dependencies
module CK.TinyLog

open System
open System.IO

type Logger internal (log_string, log_exception, dispose) =
    member this.log_string(str: string): unit = log_string(str)
    member this.log_exception(exn: Exception): unit = log_exception(exn)
    interface IDisposable with
        member this.Dispose() = dispose()    

/// Internal Representation Of Logging Operations
[<NoComparison>]
type private LogMsg =
    | LogString of string
    | LogException of exn
    | Dispose
        
/// Internal Representation Of Looping Operations
[<NoComparison>]
type private MBPMsg =
    | InitStreamWriter
    | CloseAndInitStreamWriter of StreamWriter
    | HandleMessage of (StreamWriter * int32)
    | Terminate of StreamWriter
    
/// Generates A Stream Writer Based
/// Off A Root Directory And A DateTime
let private gen_stream_writer (dt: DateTime) (root: DirectoryInfo) =
    let fs_friendly_datetime_str = dt.ToString("yyyy-MM-dd HH-mm-ss-fff")
    let fp = sprintf "%s/%s.log" root.FullName fs_friendly_datetime_str
    new StreamWriter(File.OpenWrite(fp))
        
let private format_date (dt: DateTime) =
    dt.ToString("yyyy/MM/dd HH:mm:ss: ")
    
let private format_exn (ex: Exception) =
    // TODO: Add more data from exception
    ex.Message
    
/// Generate A New Logger
let gen (root: DirectoryInfo): Logger =

    let mbp = MailboxProcessor.Start(fun inbox ->
        let mutable op = InitStreamWriter
        let mutable do_loop = true
        async {
            while do_loop do
                match op with
                | InitStreamWriter ->
                        let sw = gen_stream_writer DateTime.Now root
                        op <- HandleMessage(sw, 0)
                    
                | CloseAndInitStreamWriter sw ->
                        sw.Dispose()
                        op <- InitStreamWriter
                        
                | HandleMessage (sw, count) when count = 1000 ->
                        op <- CloseAndInitStreamWriter sw
                        
                | HandleMessage (sw, count) ->
                        let! msg = inbox.Receive()
                            
                        let line = match msg with
                                    | LogString str    -> Some str
                                    | LogException exn -> Some (format_exn exn)
                                    | Dispose          -> None
                                       
                        let lineWithDT = line
                                            |> Option.map (fun str -> format_date DateTime.Now + str)
                            
                        match lineWithDT with
                        | Some line ->
                            //sw.WriteLine(line)
                            op <- HandleMessage(sw, count + 1)
                                
                        | None ->
                            op <- Terminate sw
                                
                | Terminate sw ->
                        sw.Dispose()
                        do_loop <- false
        })
                    
    let log_string str    = mbp.Post (LogString str)
    let log_exception exn = mbp.Post (LogException exn)
    let dispose ()        = mbp.Post Dispose
        
    new Logger(log_string, log_exception, dispose)


module RuntimeTests =

    [<NoComparison>]
    type TestFailure =
        | DirectoryDoesntExistOrDoesntHavePermissions
        | CantCreateFile of Exception
        | CantDeleteFile of Exception
        
    let test_filesystem (root: DirectoryInfo) (time: DateTime) : Result<unit, TestFailure> =
        let ticks_str = time.Ticks.ToString()
    
        if not <| root.Exists then
            Error DirectoryDoesntExistOrDoesntHavePermissions
        else
            let fp = sprintf "%s/%s.test" root.FullName ticks_str
            let fi = new FileInfo(fp)
            
            try
                fi.Create()
                |> ignore
                
                try
                    fi.Delete()
                    Ok ()
                with
                    | ex -> Error (CantDeleteFile ex)
                
            with
                | ex -> Error (CantCreateFile ex)