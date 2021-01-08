let save_to_disk init_state path event_to_yojson =
  let prev_states = ref init_state in
  fun events ->
    prev_states := events @ !prev_states ;
    !prev_states |> List.map event_to_yojson
    |> (fun json -> `List json)
    |> Yojson.Safe.to_file path

let load_events event_of_yojson path =
  if Sys.file_exists path then
    Yojson.Safe.from_file path |> Yojson.Safe.Util.to_list
    |> List.map event_of_yojson
    |> List.filter_map Result.to_option
  else []

let restore_from_events restore empty_state events =
  events |> List.rev |> List.fold_left restore empty_state
