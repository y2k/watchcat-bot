open Lib.Domain
open TelegramApi

type ('f, 'eff) env =
  { func: 'f -> Message.message -> 'eff list
  ; is_admin: bool
  ; state: state
  ; mention_user_id: int option
  ; message_date: int }

let empty_env =
  { func= (fun _ _ -> [])
  ; is_admin= false
  ; state= {trusted_users= UserMap.empty}
  ; mention_user_id= None
  ; message_date= 0 }

let handle_command (env : _ env) json =
  env.func
    (object
       method is_admin = env.is_admin

       method state = env.state
    end)
    (let msg = Yojson.Safe.from_string json in
     Message.read msg)

type available_commands = [`DeleteMessage of int | `KickUser of int] list
[@@deriving show]

let asset_ban_message message_json =
  let json =
    {|{"message_id":100,"from":{"id":400,"is_bot":false,"first_name":"Igor","username":"angmarr","language_code":"en"},"chat":{"id":400,"first_name":"Igor","username":"angmarr","type":"private"},"date":0,"reply_to_message":|}
    ^ message_json
    ^ {|,"text":"/baka","entities":[{"offset":0,"length":5,"type":"bot_command"}]}|}
  in
  let actual =
    handle_command
      { empty_env with
        func= try_ban
      ; state=
          { trusted_users=
              UserMap.singleton {chat_id= 400; user_id= 400} {name= ""} } }
      json
  in
  if actual = [`DeleteMessage 200; `KickUser 300; `DeleteMessage 100] then true
  else (
    actual |> show_available_commands |> Printf.printf "WRONG COMMANDS: %s\n" ;
    false )

let%test "new test by JSON" =
  asset_ban_message
    {|{"message_id":200,"from":{"id":300,"is_bot":false,"first_name":"Igor","username":"angmarr","language_code":"en"},"chat":{"id":400,"first_name":"Igor","username":"angmarr","type":"private"},"date":0,"forward_from_chat":{"id":-1001218991791,"title":"Callum Law","type":"channel"},"forward_from_message_id":3,"forward_date":1610520008,"text":"https://t.me/joinchat/S5UjklG2Tx69tG8z","entities":[{"offset":0,"length":38,"type":"url"}]}|}

let%test "new test by JSON" =
  asset_ban_message
    {|{"message_id":200,"from":{"id":300,"is_bot":false,"first_name":"Igor","username":"angmarr","language_code":"en"},"chat":{"id":400,"first_name":"Igor","username":"angmarr","type":"private"},"date":0,"text":"https://t.me/joinchat/TyToURiKpfFB_ggN","entities":[{"offset":0,"length":38,"type":"url"}]}|}
