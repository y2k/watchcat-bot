open Domain

let user_option_to_string = Option.fold ~none:"" ~some:Domain.user_to_string

let state_diff_to_string (events : StateEvents.event list) =
  events
  |> List.map StateEvents.event_to_yojson
  |> (fun xs -> `List xs)
  |> Yojson.Safe.to_string

let effect_to_string _env = function
  | `DeleteMessage message_id ->
      Printf.sprintf "RemoveMessage (%i)" message_id
  | `KickUser user_id ->
      Printf.sprintf "KickUser (%i)" user_id
  | `UpdateState updates ->
      state_diff_to_string updates |> Printf.sprintf "UpdateState ([%s])"
  | `SendMessage message ->
      Printf.sprintf "SendMessage (%s)" message

let effects_to_string env = function
  | [] ->
      ""
  | [eff] ->
      effect_to_string env eff
  | eff :: effs ->
      effs
      |> List.fold_left
           (fun a x -> a ^ "; " ^ effect_to_string env x)
           (effect_to_string env eff)

let log env text reply_text effs =
  let log_message = effects_to_string env effs in
  Printf.sprintf
    "LOG: %s, is_admin=%b\n  REQUEST: %s\n  REPLAY_TEXT: %s\n  EFFECTS: [%s]"
    (user_option_to_string env#user)
    env#is_admin text
    (reply_text |> Option.fold ~none:"" ~some:Fun.id)
    log_message
  |> print_endline
