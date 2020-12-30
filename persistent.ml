let save_to_disk path event_to_yojson events =
  events |> List.map event_to_yojson
  |> (fun json -> `List json)
  |> Yojson.Safe.to_file path

let load restore event_of_yojson empty_state path =
  if Sys.file_exists path then
    Yojson.Safe.from_file path |> Yojson.Safe.Util.to_list
    |> List.map event_of_yojson
    |> List.filter_map Result.to_option
    |> List.fold_left restore empty_state
  else empty_state
