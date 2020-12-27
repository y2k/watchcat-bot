open Domain
open Telegram.Api

let state_store = ref Serializer.empty_state

let save_to_disk =
  Persistent.save_to_disk "data.json" Domain.Serializer.serialize
    Domain.Serializer.event_to_yojson

module WatchcatBot = Mk (struct
  include Telegram.BotDefaults
  open Command
  open Message

  let token = Sys.getenv "TELEGRAM_TOKEN"

  let command_postfix = Some "watchcat"

  let delete_message ~chat_id ~message_id =
    let url = "https://api.telegram.org/bot" ^ token ^ "/" in
    let body =
      `Assoc [("chat_id", `Int chat_id); ("message_id", `Int message_id)]
      |> Yojson.Safe.to_string
    in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    let open Lwt.Syntax in
    let* _, body =
      Cohttp_lwt_unix.Client.post ~headers
        ~body:(Cohttp_lwt.Body.of_string body)
        (Uri.of_string (url ^ "deleteMessage"))
    in
    let+ json = Cohttp_lwt.Body.to_string body in
    let obj = Yojson.Safe.from_string json in
    let open TelegramUtil in
    match get_field "ok" obj with
    | `Bool true ->
        ()
    | _ ->
        get_field "description" obj |> the_string |> fun x -> print_endline x

  let handle_effects chat_id effects =
    let open Telegram.Actions in
    let handleEffect chat_id effect =
      match effect with
      | `DeleteMessage message_id ->
          Lwt.async (fun _ -> delete_message ~chat_id ~message_id) ;
          nothing
      | `KickUser user_id ->
          kick_chat_member ~chat_id ~user_id
      | `UpdateState state ->
          state_store := state ;
          save_to_disk state ;
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

      method now = Unix.time ()
    end

  let new_chat_member (chat : Chat.chat) (user : User.user) =
    Domain.new_chat_member (make_env false (Some user)) user.id
    |> handle_effects chat.id

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
          let env = make_env is_admin msg.from in
          let effs = f env msg in
          let reply_text = Option.bind msg.reply_to_message (fun x -> x.text) in
          Logger.log env msg.text reply_text effs ;
          effs |> handle_effects msg.chat.id)
    in
    Domain.user_commands
    |> List.map (fun (uc : _ Domain.user_command) ->
           { name= uc.name
           ; description= uc.description
           ; enabled= true
           ; run= wrap uc.run })
end)

let () =
  state_store :=
    Persistent.load Domain.Serializer.restore Domain.Serializer.event_of_yojson
      Domain.Serializer.empty_state "data.json" ;
  print_endline "Bot started..." ;
  WatchcatBot.run ~log:true ()
