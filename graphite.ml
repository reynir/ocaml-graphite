module RawMetric = struct
  type datapoint = float option * int [@@deriving yojson]

  type t = {
    target : string;
    datapoints : datapoint list;
  } [@@deriving yojson]

  type metrics = t list [@@deriving yojson]
end

module Metric = struct
  module Datapoint = struct
    type t = {
      timestamp : int;
      value : float option;
    } [@@deriving show]

    let map ~timestamp ~value d =
      { timestamp = timestamp d.timestamp;
        value = value d.value }
  end

  open Datapoint

  type t = {
    target : string;
    datapoints : Datapoint.t list;
  } [@@deriving show]

  let datapoint_of_raw_datapoint (value, timestamp) =
    { value; timestamp }

  let of_raw { RawMetric.target; RawMetric.datapoints } =
    { target;
      datapoints = List.map datapoint_of_raw_datapoint datapoints }

  let map ~target ~datapoints m =
    { target = target m.target;
      datapoints = datapoints m.datapoints }
end

type nonrec datapoint = Metric.Datapoint.t
and metric = Metric.t

let get url =
  let%lwt (resp, body) = Cohttp_lwt_unix.Client.get url in
  let%lwt body = Cohttp_lwt_body.to_string body in
  let json = Yojson.Safe.from_string body in
  match RawMetric.metrics_of_yojson json with
  | `Error msg -> Lwt.fail_with msg
  | `Ok raw ->
    Lwt.return (List.map Metric.of_raw raw)

type seconds = int
type time =
  | Relative of seconds
  | Epoch of seconds


let string_of_time =
  let string_of_relative s =
    if s < 60 || s mod 60 <> 0
    then Printf.sprintf "-%ds" s
    else if s < 60*60 || s mod (60*60) <> 0
    then Printf.sprintf "-%dmin" (s / 60)
    else if s < 60*60*24 || s mod (60*60*24) <> 0
    then Printf.sprintf "-%dh" (s / 60 / 60)
    else Printf.sprintf "-%dd" (s / 60 / 60 / 24)
        (* TODO: weeks, months, years *)
  in function
  | Relative s ->
    string_of_relative s
  | Epoch s -> string_of_int s

(* Maybe use format=raw ? *)
let mk_url host ?(port=8081) ?(from=Relative (5*60)) ?until targets =
  let query =
    [("format", ["json"]);
     ("from", [string_of_time from])] @
    (match until with
     | None -> []
     | Some until -> [("until", [string_of_time until])]) @
    List.map (fun target -> ("target", [target])) targets in
  Uri.make
    ~scheme:"http"
    ~host
    ~port
    ~path:"/render/"
    ~query
    ()

let all_metrics host ?(port=8081) () =
  let url =
    Uri.make
      ~scheme:"http"
      ~host
      ~port
      ~path:"/metrics/index.json"
      () in
  let%lwt (resp, body) = Cohttp_lwt_unix.Client.get url in
  let%lwt body = Cohttp_lwt_body.to_string body in
  let json = Yojson.Safe.from_string body in
  Lwt.return @@ match json with
  | `List metrics ->
    List.map (function (`String m) -> m | _ -> failwith "all_metrics: Not a json string") metrics
  | _ -> failwith "all_metrics: Response was not a json list"
