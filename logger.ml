let user_option_to_string = Option.fold ~none:"" ~some:Domain.user_to_string

let state_diff_to_string old_state new_state =
  let open Domain in
  let to_string xs =
    UserMap.fold
      (fun k (v : user_info) a ->
        Printf.sprintf "%s [%s(%i/%i)]" a v.name k.chat_id k.user_id)
      xs ""
  in
  let added =
    UserMap.fold
      (fun k _ a -> UserMap.remove k a)
      old_state.trusted_users new_state.trusted_users
  and removed =
    UserMap.fold
      (fun k _ a -> UserMap.remove k a)
      new_state.trusted_users old_state.trusted_users
  in
  Printf.sprintf "Added: %s, Removed: %s" (to_string added) (to_string removed)

let effect_to_string env = function
  | `DeleteMessage message_id ->
      Printf.sprintf "RemoveMessage (%i)" message_id
  | `KickUser user_id ->
      Printf.sprintf "KickUser (%i)" user_id
  | `UpdateState state ->
      state_diff_to_string env#state state |> Printf.sprintf "UpdateState (%s)"
  | `SendMessage message ->
      Printf.sprintf "SendMessage (%s)" message
  | `None ->
      "None"

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
    env#is_admin
    (text |> Option.fold ~none:"" ~some:Fun.id)
    (reply_text |> Option.fold ~none:"" ~some:Fun.id)
    log_message
  |> print_endline
