open Lib.Domain
open TelegramApi

type available_commands = [`DeleteMessage of int | `KickUser of int] list
[@@deriving show]

let asset_ban_message message_json =
  let json =
    {|{"message_id":100,"from":{"id":400,"is_bot":false,"first_name":"Igor","username":"angmarr","language_code":"en"},"chat":{"id":400,"first_name":"Igor","username":"angmarr","type":"private"},"date":0,"reply_to_message":|}
    ^ ( message_json
      |> Str.replace_first (Str.regexp {|,"date":[0-9]+,|}) {|,"date":0,|} )
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
  match actual with
  | [`DeleteMessage _; `KickUser _; `DeleteMessage _] ->
      true
  | _ ->
      actual |> show_available_commands |> Printf.printf "WRONG COMMANDS: %s\n" ;
      false

let%test "new test by JSON" =
  asset_ban_message
    {|{"message_id":200,"from":{"id":300,"is_bot":false,"first_name":"Igor","username":"angmarr","language_code":"en"},"chat":{"id":400,"first_name":"Igor","username":"angmarr","type":"private"},"date":0,"forward_from_chat":{"id":-1001218991791,"title":"Callum Law","type":"channel"},"forward_from_message_id":3,"forward_date":1610520008,"text":"https://t.me/joinchat/S5UjklG2Tx69tG8z","entities":[{"offset":0,"length":38,"type":"url"}]}|}

let%test "new test by JSON" =
  asset_ban_message
    {|{"message_id":200,"from":{"id":300,"is_bot":false,"first_name":"Igor","username":"angmarr","language_code":"en"},"chat":{"id":400,"first_name":"Igor","username":"angmarr","type":"private"},"date":0,"text":"https://t.me/joinchat/TyToURiKpfFB_ggN","entities":[{"offset":0,"length":38,"type":"url"}]}|}

let%test "new test by JSON" =
  asset_ban_message
    {|{"message_id":200,"from":{"id":300,"is_bot":false,"first_name":"Igor","username":"angmarr","language_code":"en"},"chat":{"id":300,"first_name":"Igor","username":"angmarr","type":"private"},"date":0,"photo":[{"file_id":"AgACAgIAAxkBAAIMzmAEGDZnfl-0xL8cK0WRZsDDhxBEAAIysDEbu8IhSEMAAehZZiOJlGMM5JcuAAMBAAMCAANtAAMaxQUAAR4E","file_unique_id":"AQADYwzkly4AAxrFBQAB","file_size":19688,"width":311,"height":320},{"file_id":"AgACAgIAAxkBAAIMzmAEGDZnfl-0xL8cK0WRZsDDhxBEAAIysDEbu8IhSEMAAehZZiOJlGMM5JcuAAMBAAMCAAN4AAMbxQUAAR4E","file_unique_id":"AQADYwzkly4AAxvFBQAB","file_size":70321,"width":777,"height":800},{"file_id":"AgACAgIAAxkBAAIMzmAEGDZnfl-0xL8cK0WRZsDDhxBEAAIysDEbu8IhSEMAAehZZiOJlGMM5JcuAAMBAAMCAAN5AAMcxQUAAR4E","file_unique_id":"AQADYwzkly4AAxzFBQAB","file_size":80573,"width":1049,"height":1080}]}|}

let%test "new test by JSON" =
  "eyJtZXNzYWdlX2lkIjo4MTA0LCJmcm9tIjp7ImlkIjoxNDU4OTQzNjY3LCJpc19ib3QiOmZhbHNlLCJmaXJzdF9uYW1lIjoiTWFuYSIsImxhc3RfbmFtZSI6IlBhdHRlcnNvbiIsInVzZXJuYW1lIjoiRWNob3ZpcnVzXzE4NzkifSwiY2hhdCI6eyJpZCI6LTEwMDExMzU2NTkxMTcsInRpdGxlIjoiQW5kcm9pZCBEZWNsYXJhdGl2ZSIsInVzZXJuYW1lIjoiYW5kcm9pZF9kZWNsYXJhdGl2ZSIsInR5cGUiOiJzdXBlcmdyb3VwIn0sImRhdGUiOjE2MTMxNTUxNTMsInRleHQiOiJXb29vdyEhIFJlYWxseT/wn5ixICBFbGxvbk1tdXNrIHB1bXBpbmcgYml0Y29pbiHwn5qA8J+agCAgTG9vb2sgaXMgdGhhdCBodHRwczovL2JpdC5seS8zYjY5YVd1IiwiZW50aXRpZXMiOlt7Im9mZnNldCI6NjYsImxlbmd0aCI6MjIsInR5cGUiOiJ1cmwifV19"
  |> Base64.decode_exn |> asset_ban_message

let%test "new test by JSON" =
  "ewogICJtZXNzYWdlX2lkIjogODA0OCwKICAiZnJvbSI6IHsKICAgICJpZCI6IDE2NDE3OTA5MTYsCiAgICAiaXNfYm90IjogZmFsc2UsCiAgICAiZmlyc3RfbmFtZSI6ICJTYW50YW5hIiwKICAgICJsYXN0X25hbWUiOiAiQ29mZmV5IiwKICAgICJ1c2VybmFtZSI6ICJib3JlZG9tc19CdWxsZW4iCiAgfSwKICAiY2hhdCI6IHsKICAgICJpZCI6IC0xMDAxMTM1NjU5MTE3LAogICAgInRpdGxlIjogIkFuZHJvaWQgRGVjbGFyYXRpdmUiLAogICAgInVzZXJuYW1lIjogImFuZHJvaWRfZGVjbGFyYXRpdmUiLAogICAgInR5cGUiOiAic3VwZXJncm91cCIKICB9LAogICJkYXRlIjogMTYxMzAzMDEzNCwKICAidGV4dCI6ICJcdTA0MDZuXHUwNDQxclx1MDQzNWRpYlx1MDQwNlx1MDQ0MywgXHUwNDIyXHUwNDM1c1x1MDQwNlx1MDQzMCBpcyBnXHUwNDU2dlx1MDQ1Nm5nIG91dCBcdTA0MTJcdTA0NTZ0XHUwNDQxXHUwNDNlXHUwNDU2biEhISAtIGh0dHBzOi8vYml0Lmx5LzN0TVg0dEYiLAogICJlbnRpdGllcyI6IFsKICAgIHsKICAgICAgIm9mZnNldCI6IDQ1LAogICAgICAibGVuZ3RoIjogMjIsCiAgICAgICJ0eXBlIjogInVybCIKICAgIH0KICBdCn0="
  |> Base64.decode_exn |> asset_ban_message
