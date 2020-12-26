(* let user_to_string {TelegramApi.User.first_name; username; _} =
  Printf.sprintf "%s%s" first_name
    (username |> Option.fold ~none:"" ~some:(fun un -> " (@" ^ un ^ ")")) *)

let user_option_to_string = Option.fold ~none:"" ~some:Domain.user_to_string

(* let log env message =
  Printf.sprintf "LOG (%s): %s" (user_option_to_string env#user) message
  |> print_endline *)

let effect_to_string = function
  | `DeleteMessage message_id ->
      Printf.sprintf "RemoveMessage (%s)" message_id
  | `KickUser user ->
      Printf.sprintf "KickUser (%s)" user
  | `UpdateState state ->
      Printf.sprintf "UpdateState (%s)" state
  | `SendMessage message ->
      Printf.sprintf "SendMessage (%s)" message
  | `None ->
      "None"

let effects_to_string = function
  | [] ->
      ""
  | [eff] ->
      effect_to_string eff
  | eff :: effs ->
      effs
      |> List.fold_left
           (fun a x -> a ^ ", " ^ effect_to_string x)
           (effect_to_string eff)

let log env f message =
  let effs = f message in
  let log_message = effect_to_string effs in
  Printf.sprintf "LOG (%s): [%s]" (user_option_to_string env#user) log_message
  |> print_endline ;
  effs
