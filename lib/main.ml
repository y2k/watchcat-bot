open Domain
open Telegram.Api

let state_store = ref StateEvents.empty_state

let save_to_disk =
  Persistent.save_to_disk "data.json" StateEvents.event_to_yojson

module WatchcatBot = Mk (struct
  include Telegram.BotDefaults
  open Command
  open Message

  let token = Sys.getenv "TELEGRAM_TOKEN"

  let command_postfix = Some "watchcat"

  let handle_effects chat_id effects =
    let open Telegram.Actions in
    let handleEffect chat_id effect =
      match effect with
      | `DeleteMessage message_id ->
          delete_message ~chat_id ~message_id
      | `KickUser user_id ->
          kick_chat_member ~chat_id ~user_id
      | `UpdateState events ->
          state_store :=
            events |> List.fold_left StateEvents.restore !state_store ;
          save_to_disk events ;
          nothing
      | `SendMessage message ->
          send_message ~chat_id ~disable_notification:true "%s" message
      | `None ->
          nothing
    in
    effects |> List.map (handleEffect chat_id) |> sequence

  let make_env (is_admin : bool) (user : User.user option) =
    object
      method user = user

      method is_admin = is_admin

      method state = !state_store
    end

  let new_chat_member (chat : Chat.chat) (user : User.user) =
    Domain.new_chat_member (make_env false (Some user)) user.id
    |> handle_effects chat.id

  let is_admin f msg =
    let open Telegram.Actions in
    ( match msg with
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
        nothing )
    |> Lwt.return

  let last_json = ref ""

  let hook_update json data =
    last_json := json ;
    data

  let commands =
    let wrap f =
      is_admin (fun msg is_admin ->
          let env = make_env is_admin msg.from in
          let effs = f env msg in
          let reply_text = Option.bind msg.reply_to_message (fun x -> x.text) in
          Logger.log env !last_json reply_text effs ;
          effs |> handle_effects msg.chat.id)
    in
    Domain.user_commands
    |> List.map (fun (uc : _ Domain.user_command) ->
           { name= uc.name
           ; description= uc.description
           ; enabled= true
           ; run= wrap uc.run })
end)

let run () =
  state_store :=
    Persistent.load Domain.StateEvents.restore
      Domain.StateEvents.event_of_yojson Domain.StateEvents.empty_state
      "data.json" ;
  print_endline "Bot started..." ;
  WatchcatBot.run ~log:true ()
