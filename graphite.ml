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
