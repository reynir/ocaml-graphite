module RawMetric = struct
  type datapoint = float option * int [@@deriving yojson]

  type t = {
    target : string;
    datapoints : datapoint list;
  } [@@deriving yojson]

  type metrics = t list [@@deriving yojson]
end

module Metric = struct
  type datapoint = {
    timestamp : int;
    value : float option;
  }

  type t = {
    target : string;
    datapoints : datapoint list;
  }

  let datapoint_of_raw_datapoint (value, timestamp) =
    { value; timestamp }

  let of_raw { RawMetric.target; RawMetric.datapoints } =
    { target;
      datapoints = List.map datapoint_of_raw_datapoint datapoints }
end

type datapoint = Metric.datapoint
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
let mk_url host ?(from=Relative (5*60)) ?until targets =
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
    ~port:8081
    ~path:"/render/"
    ~query
    ()
