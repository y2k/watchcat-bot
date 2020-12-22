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
    | `RemoveMessage _message_id ->
        nothing
    | `KickUser user ->
        kick_chat_member ~chat_id ~user_id:user
    | `UpdateState state ->
        state_store := state ;
        nothing
    | `SendMessage message ->
        send_message ~chat_id "%s" message
    | `None ->
        nothing

  let handleEffects chat_id effects =
    let open Telegram.Actions in
    effects |> List.map (handleEffect chat_id) |> sequence

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
      | { chat= {id= chat_id; _}
        ; from= Some {id= user_id; _}
        ; reply_to_message=
            Some {from= Some {id= repl_user_id; _}; message_id; _}
        ; _ } ->
          print_endline "LOG :: try_ban" ;
          Domain.try_ban (make_env ()) user_id repl_user_id message_id
          |> handleEffects chat_id
      | _ ->
          failwith "???"
    and add_trusted_user = function
      | {chat= {id= chat_id; _}; entities; _} ->
          Domain.add_trusted_user (make_env ()) (find_user_in_message entities)
          |> handleEffect chat_id
    and remove_trusted_user = function
      | {chat= {id= chat_id; _}; entities; _} ->
          Domain.remove_trusted_user (make_env ())
            (find_user_in_message entities)
          |> handleEffect chat_id
    in
    [ { name= "ban"
      ; description= "Забанить пользователя"
      ; enabled= true
      ; run= try_ban }
    ; { name= "add"
      ; description=
          "Добавить доверенного пользователя"
      ; enabled= true
      ; run= with_auth ~command:add_trusted_user }
    ; { name= "remove"
      ; description=
          "Удалить доверенного пользователя"
      ; enabled= true
      ; run= with_auth ~command:remove_trusted_user } ]
end)

let () =
  print_endline "Bot started..." ;
  WatchcatBot.run ~log:true ()
