let save_to_disk path serialize_to_events event_to_yojson state =
  serialize_to_events state |> Seq.map event_to_yojson |> List.of_seq
  |> fun json -> `List json |> Yojson.Safe.to_file path

let load restore event_of_yojson empty_state path =
  Yojson.Safe.from_file path |> Yojson.Safe.Util.to_list
  |> List.map event_of_yojson
  |> List.filter_map Result.to_option
  |> List.fold_left restore empty_state
