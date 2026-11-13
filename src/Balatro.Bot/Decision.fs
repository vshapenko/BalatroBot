module Balatro.Bot.Decision

open System.Text.Json.Nodes

type Candidate =
    { Action: string
      Value: float
      Detail: string }

type Decision =
    { Seq: int
      Ante: int
      Round: int
      Phase: string
      Chosen: string
      Rule: string
      Candidates: Candidate list
      Predicted: float option }

type Outcome =
    { Seq: int
      Predicted: float
      Actual: float }

let private str (s: string) = JsonValue.Create s :> JsonNode
let private num (f: float) = JsonValue.Create f :> JsonNode
let private int' (i: int) = JsonValue.Create i :> JsonNode

let private candidateJson (c: Candidate) =
    let o = JsonObject()
    o.Add("action", str c.Action)
    o.Add("value", num c.Value)
    o.Add("detail", str c.Detail)
    o :> JsonNode

let toJson (d: Decision) : string =
    let o = JsonObject()
    o.Add("kind", str "decision")
    o.Add("seq", int' d.Seq)
    o.Add("ante", int' d.Ante)
    o.Add("round", int' d.Round)
    o.Add("phase", str d.Phase)
    o.Add("chosen", str d.Chosen)
    o.Add("rule", str d.Rule)
    o.Add("candidates", JsonArray(d.Candidates |> List.map candidateJson |> Array.ofList))
    o.Add("predicted", d.Predicted |> Option.map num |> Option.toObj)
    o.ToJsonString()

let toText (d: Decision) : string =
    let chosen =
        match d.Candidates with
        | first :: _ -> sprintf "%s (%.0f)" d.Chosen first.Value
        | [] -> d.Chosen

    let next =
        match d.Candidates with
        | _ :: second :: _ -> sprintf "; next best %s (%.0f)" second.Action second.Value
        | _ -> ""

    sprintf "#%d ante %d round %d %s: %s%s; rule: %s" d.Seq d.Ante d.Round d.Phase chosen next d.Rule

let outcomeToJson (o: Outcome) : string =
    let j = JsonObject()
    j.Add("kind", str "outcome")
    j.Add("seq", int' o.Seq)
    j.Add("predicted", num o.Predicted)
    j.Add("actual", num o.Actual)
    j.ToJsonString()

let outcomeToText (o: Outcome) : string =
    sprintf
        "#%d outcome: predicted %.0f, actual %.0f, %s"
        o.Seq
        o.Predicted
        o.Actual
        (if o.Predicted = o.Actual then "exact" else "MISMATCH")
