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

  let make_env () =
    object
      method state = !state_store

      method now = Unix.time ()
    end

  let new_chat_member (chat : Chat.chat) (user : User.user) =
    Domain.new_chat_member (make_env ()) user.id |> handleEffects chat.id

  let commands =
    let wrap f msg = f msg |> handleEffects msg.chat.id in
    Domain.user_commands
    |> List.map (fun (uc : _ Domain.user_command) ->
           { name= uc.name
           ; description= uc.description
           ; enabled= true
           ; run=
               ( if uc.auth then
                 with_auth ~command:(wrap @@ uc.run (make_env ()))
               else wrap @@ uc.run (make_env ()) ) })
end)

let () =
  print_endline "Bot started..." ;
  WatchcatBot.run ~log:true ()
