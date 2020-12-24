open Domain
open Telegram.Api

module WatchcatBot = Mk (struct
  open Command
  open Message
  include Telegram.BotDefaults

  let token = Sys.getenv "TELEGRAM_TOKEN"

  let command_postfix = Some "watchcat"

  let state_store = ref empty_state

  let handleEffects chat_id effects =
    let open Telegram.Actions in
    let handleEffect chat_id effect =
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
    in
    effects |> List.map (handleEffect chat_id) |> sequence

  let make_env is_admin =
    object
      method is_admin = is_admin

      method state = !state_store

      method now = Unix.time ()
    end

  let new_chat_member (chat : Chat.chat) (user : User.user) =
    Domain.new_chat_member (make_env ()) user.id |> handleEffects chat.id

  let is_admin f msg =
    let open Telegram.Actions in
    match msg with
    | {chat= {id= chat_id; _}; from= Some {id= user_id; _}; _} ->
        let is_member =
          let open ChatMember in
          List.exists (fun {user= member; _} -> user_id = member.id)
        in
        get_chat_administrators ~chat_id ~and_then:(function
          | Result.Success members when is_member members ->
              f msg true
          | _ ->
              f msg false)
    | _ ->
        nothing

  let commands =
    let wrap f =
      is_admin (fun msg is_admin ->
          f (make_env is_admin) msg |> handleEffects msg.chat.id)
    in
    Domain.user_commands
    |> List.map (fun (uc : _ Domain.user_command) ->
           { name= uc.name
           ; description= uc.description
           ; enabled= true
           ; run= wrap uc.run })
end)

let () =
  print_endline "Bot started..." ;
  WatchcatBot.run ~log:true ()