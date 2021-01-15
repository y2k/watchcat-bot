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

let%test "call try_ban by trusted user to late" =
  let json =
    {|{
      "message_id": 3273,
      "from": {
        "id": 241854720,
        "is_bot": false,
        "first_name": "Igor",
        "username": "angmarr",
        "language_code": "en"
      },
      "chat": {
        "id": 241854720,
        "first_name": "Igor",
        "username": "angmarr",
        "type": "private"
      },
      "date": 1610705347,
      "reply_to_message": {
        "message_id": 3270,
        "from": {
          "id": 241854720,
          "is_bot": false,
          "first_name": "Igor",
          "username": "angmarr",
          "language_code": "en"
        },
        "chat": {
          "id": 241854720,
          "first_name": "Igor",
          "username": "angmarr",
          "type": "private"
        },
        "date": 1610705269,
        "text": "https://t.me/joinchat/TyToURiKpfFB_ggN",
        "entities": [
          {
            "offset": 0,
            "length": 38,
            "type": "url"
          }
        ]
      },
      "text": "/baka",
      "entities": [
        {
          "offset": 0,
          "length": 5,
          "type": "bot_command"
        }
      ]
    }|}
  in
  let actual =
    handle_command {empty_env with func= try_ban; is_admin= true} json
  in
  actual = [`DeleteMessage 200; `KickUser 300; `DeleteMessage 100]
