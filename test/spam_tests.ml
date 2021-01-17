open Lib.Domain
open TelegramApi

type available_commands = [`DeleteMessage of int | `KickUser of int] list
[@@deriving show]

let asset_ban_message message_json =
  let json =
    {|{"message_id":100,"from":{"id":400,"is_bot":false,"first_name":"Igor","username":"angmarr","language_code":"en"},"chat":{"id":400,"first_name":"Igor","username":"angmarr","type":"private"},"date":0,"reply_to_message":|}
    ^ message_json
    ^ {|,"text":"/baka","entities":[{"offset":0,"length":5,"type":"bot_command"}]}|}
  and state =
    {trusted_users= UserMap.singleton {chat_id= 400; user_id= 400} {name= ""}}
  in
  let actual =
    try_ban
      (object
         method is_admin = false

         method state = state
      end)
      (Yojson.Safe.from_string json |> Message.read)
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

let%test "new test by JSON" =
  asset_ban_message
    {|{"message_id":200,"from":{"id":300,"is_bot":false,"first_name":"Igor","username":"angmarr","language_code":"en"},"chat":{"id":300,"first_name":"Igor","username":"angmarr","type":"private"},"date":0,"photo":[{"file_id":"AgACAgIAAxkBAAIMzmAEGDZnfl-0xL8cK0WRZsDDhxBEAAIysDEbu8IhSEMAAehZZiOJlGMM5JcuAAMBAAMCAANtAAMaxQUAAR4E","file_unique_id":"AQADYwzkly4AAxrFBQAB","file_size":19688,"width":311,"height":320},{"file_id":"AgACAgIAAxkBAAIMzmAEGDZnfl-0xL8cK0WRZsDDhxBEAAIysDEbu8IhSEMAAehZZiOJlGMM5JcuAAMBAAMCAAN4AAMbxQUAAR4E","file_unique_id":"AQADYwzkly4AAxvFBQAB","file_size":70321,"width":777,"height":800},{"file_id":"AgACAgIAAxkBAAIMzmAEGDZnfl-0xL8cK0WRZsDDhxBEAAIysDEbu8IhSEMAAehZZiOJlGMM5JcuAAMBAAMCAAN5AAMcxQUAAR4E","file_unique_id":"AQADYwzkly4AAxzFBQAB","file_size":80573,"width":1049,"height":1080}]}|}
