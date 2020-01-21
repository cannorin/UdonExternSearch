module App.View

open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome
open System

open Thoth.Elmish

type ExternType<'a> =
  | StaticFunc of args:'a[] * ret:'a option
  | StaticGenericFunc of typrm:string list * args:'a[] * ret:'a option
  | InstanceFunc of args:'a[] * ret:'a option
  | InstanceGenericFunc of typrm:string list * args:'a[] * ret:'a option
  | Constructor of args:'a[] * ty:'a
  | Unknown of arity:int * argret:'a[][]

type ExternInfo = { Namespace: string; Type: ExternType<string>; Signature: string }

type State = Initial | IsTyping | StoppedTyping

type Model = {
  Query : string[]
  Data: (string * ExternInfo[])[] option
  Debouncer: Debouncer.State
  InputState: State
}

type Msg =
  | DebouncerSelfMsg of Debouncer.SelfMessage<Msg>
  | ChangeQuery of string
  | EndOfInput
  | LoadData
  | SetData of (string * ExternInfo[])[]

let init _ =
  { Query = [||]; Data = None; Debouncer = Debouncer.create (); InputState = Initial },
  Cmd.ofMsg LoadData

open Thoth.Json
let charCoder =
  Extra.empty
  |> Extra.withCustom
    (string >> Encode.string)
    (fun str obj ->
      Decode.string str obj
      |> Result.bind (fun s ->
        if s.Length = 1 then Ok s.[0]
        else Error ("not a char", FailMessage "not a char")))

let decoder : Decoder<(string * ExternInfo[])[]> =
  Decode.Auto.generateDecoderCached(extra=charCoder)

open Thoth.Fetch

let getData () : Cmd<Msg> =
  promise {
    let url = "assets/udon_externs.json"
    let! data = Fetch.get(url, decoder)
    return SetData data
  } |> Cmd.OfPromise.result

let private update msg model =
  match msg with
  | DebouncerSelfMsg m ->
    let dM, dC = Debouncer.update m model.Debouncer
    { model with Debouncer = dM }, dC
  | ChangeQuery newValue ->
    let dM, dC =
      model.Debouncer |> Debouncer.bounce (TimeSpan.FromSeconds 1.0) "user_input" EndOfInput
    { model with Query = newValue.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                 InputState = IsTyping; Debouncer = dM },
    Cmd.batch [ Cmd.map DebouncerSelfMsg dC ]
  | EndOfInput ->
    { model with InputState = StoppedTyping }, Cmd.none
  | LoadData -> model, getData ()
  | SetData data ->
    { model with Data = Some data }, Cmd.none

let private viewExtern (info: ExternInfo) =
  [
    let descr, table =
      match info.Type with
      | StaticFunc (args, ret) ->
        "Static function",
        [
          for arg in args do
            yield sprintf "arg: %s" arg
          match ret with
          | None -> ()
          | Some r -> yield sprintf "output: %s" r
        ]
      | StaticGenericFunc (typrms, args, ret) ->
        "Static generic function",
        [
          for arg in args do
            yield sprintf "arg: %s" arg
          for typrm in typrms do
            yield sprintf "type parameter: SystemType object of type %s" typrm
          match ret with
          | None -> ()
          | Some r -> yield sprintf "output: %s" r
        ]
      | InstanceFunc (args, ret) ->
        "Instance function",
        [
          yield sprintf "instance: %s" info.Namespace 
          for arg in args do
            yield sprintf "arg: %s" arg
          match ret with
          | None -> ()
          | Some r -> yield sprintf "output: %s" r
        ]
      | InstanceGenericFunc (typrms, args, ret) ->
        "Instance generic function",
        [
          yield sprintf "instance: %s" info.Namespace 
          for arg in args do
            yield sprintf "arg: %s" arg
          for typrm in typrms do
            yield sprintf "type parameter: SystemType object of type %s" typrm
          match ret with
          | None -> ()
          | Some r -> yield sprintf "output: %s" r
        ]
      | Constructor (args, ty) ->
        "Constructor",
        [
          for arg in args do
            yield sprintf "arg: %s" arg
          yield sprintf "output: %s" ty
        ]
      | Unknown (arity, _) ->
        "Unknown", [ for i = 1 to arity do yield "unknown" ]

    h5 [] [str (descr + " "); code [] [str (info.Namespace + "." + info.Signature)]]
    Table.table [ Table.IsFullWidth ] [
      thead [] [
        tr [] [
          th [] [str "Stack Position"]
          th [] [str "Usage"]
        ]
      ]
      tbody [] [
        for i, u in table |> Seq.rev |> Seq.indexed |> Seq.rev do
          tr [] [
            th [ ] [ str (string (i+1)) ]
            td [ ] [ str u ]
          ]
      ]
    ]
  ]

let private containsCaseInsensitive (str: string) (substr: string) =
  str.ToLower().Contains(substr.ToLower())

let private view model dispatch = 
  body [] [
    Section.section [] [
      Content.content [] [
        h1 [] [
          str "Udon Extern Search (Udon 関数検索)"
          a [ Href "https://github.com/cannorin/UdonExternSearch" ] [
            Icon.icon [Icon.Size IsLarge ] [
              Fa.i [Fa.Brand.Github] []
            ]
          ]
          a [ Href "https://twitter.com/cannorin_vrc" ] [
            Icon.icon [Icon.Size IsLarge ] [
              Fa.i [Fa.Brand.Twitter] []
            ]
          ]
        ]
        p [] [str "UDONSDK version: 2020.01.14.10.47, VRCSDK3 version: 2020.01.14.10.40"]
        p [] [str "Udon で使える関数を検索できます．"]
        p [] [str "Here you can search extern functions available in Udon."]
        p [] [str "関数の完全名と，呼び出すにはスタックの何番目に何を入れればいいかを見ることができます．"]
        p [] [str "You can also see the signature (full name) and stack usage per function."]
        Control.div [ Control.HasIconLeft ] [
          Input.text [
            Input.Size IsMedium
            Input.Placeholder (if model.Data.IsSome then "Search" else "Loading")
            Input.Disabled model.Data.IsNone
            Input.OnChange (fun e -> dispatch (ChangeQuery e.Value))
          ]
          Icon.icon [ Icon.Size IsMedium; Icon.IsLeft ] [
            Fa.i [ Fa.Solid.Search ] []
          ]
        ]
      ]
    ]

    if model.InputState = StoppedTyping && Array.isEmpty model.Query |> not then
      Section.section [] [
        Content.content [] [
          match model.Data with
          | Some data ->
            let xs = data |> Seq.filter (fun (k, _) -> model.Query |> Array.forall (containsCaseInsensitive k))
            Container.container [ ] [
                if Seq.isEmpty xs then
                  yield
                    div [ ClassName "block" ] [
                      Notification.notification [ Notification.Color IsWarning ] [
                        str "No match"
                      ]
                    ]
                for k, infos in xs do
                  yield 
                    div [ ClassName "block" ] [
                      Box.box' [ ] [
                        h2 [] [str k]
                        h5 [] [str (sprintf "%i overload(s)" infos.Length)]
                        for info in infos do
                          yield! viewExtern info
                      ]
                  ]
            ]
          | None -> ()
        ]
      ]
  ]
  

open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run
