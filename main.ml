open Domain
open Telegram.Api

module WatchcatBot = Mk (struct
  open Command
  open Message
  include Telegram.BotDefaults

  let token = Sys.getenv "TELEGRAM_TOKEN"

  let command_postfix = Some "watchcat"

  let state_store = ref empty_state

  let handleEffect chat_id effect =
    let open Telegram.Actions in
    match effect with
    | `KickUser user ->
        kick_chat_member ~chat_id ~user_id:user
    | `UpdateState state ->
        state_store := state ;
        nothing
    | `SendMessage message ->
        send_message ~chat_id "%s" message
    | `None ->
        nothing

  let make_env () =
    object
      method state = !state_store

      method now = Unix.time ()
    end

  let new_chat_member (chat : Chat.chat) (user : User.user) =
    Domain.new_chat_member (make_env ()) user.id |> handleEffect chat.id

  let find_user_in_message entities =
    let open MessageEntity in
    entities
    |> Option.fold ~none:[] ~some:(fun x -> x)
    |> List.find_opt (fun x ->
           match x.entity_type with TextMention _ -> true | _ -> false)
    |> function
    | Some {entity_type= TextMention user; _} -> Some user.id | _ -> None

  let commands =
    let try_ban = function
      | { chat= {id= chatId; _}
        ; from= Some {id= userId; _}
        ; reply_to_message= Some {from= Some {id= repl_user_id; _}; _}
        ; _ } ->
          Domain.try_ban (make_env ()) userId repl_user_id
          |> handleEffect chatId
      | _ ->
          failwith "???"
    and add_trusted_user = function
      | {chat= {id= chatId; _}; entities; _} ->
          Domain.add_trusted_user (make_env ()) (find_user_in_message entities)
          |> handleEffect chatId
    in
    [ {name= "ban"; description= "Ban user"; enabled= true; run= try_ban}
    ; { name= "add_tu"
      ; description= "Add trusted user"
      ; enabled= true
      ; run= with_auth ~command:add_trusted_user } ]
end)

open Lwt

let run () =
  let process = function
    | Result.Success _ ->
        return ()
    | Result.Failure e ->
        if e <> "Could not get head" then Lwt_io.printl e else return ()
  in
  let rec loop () =
    WatchcatBot.pop_update ~run_cmds:true () >>= process >>= loop
  in
  print_endline "Bot started..." ;
  while true do
    (* Recover from errors if an exception is thrown *)
    try Lwt_main.run @@ loop ()
    with e -> print_endline @@ Printexc.to_string e
  done

let () = run ()
