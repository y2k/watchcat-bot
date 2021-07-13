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
      end )
      (Yojson.Safe.from_string json |> Message.read)
  in
  match actual with
  | [`DeleteMessage _; `KickUser _; `DeleteMessage _] ->
      ()
  | _ ->
      let error =
        actual |> show_available_commands
        |> Printf.sprintf "WRONG COMMANDS: %s\n"
      in
      Alcotest.fail error

let () =
  let open Alcotest in
  let samples =
    [ "eyJtZXNzYWdlX2lkIjoyMDAsImZyb20iOnsiaWQiOjMwMCwiaXNfYm90IjpmYWxzZSwiZmlyc3RfbmFtZSI6Iklnb3IiLCJ1c2VybmFtZSI6ImFuZ21hcnIiLCJsYW5ndWFnZV9jb2RlIjoiZW4ifSwiY2hhdCI6eyJpZCI6NDAwLCJmaXJzdF9uYW1lIjoiSWdvciIsInVzZXJuYW1lIjoiYW5nbWFyciIsInR5cGUiOiJwcml2YXRlIn0sImRhdGUiOjAsInRleHQiOiJodHRwczovL3QubWUvam9pbmNoYXQvVHlUb1VSaUtwZkZCX2dnTiIsImVudGl0aWVzIjpbeyJvZmZzZXQiOjAsImxlbmd0aCI6MzgsInR5cGUiOiJ1cmwifV19"
    ; "eyJtZXNzYWdlX2lkIjoyMDAsImZyb20iOnsiaWQiOjMwMCwiaXNfYm90IjpmYWxzZSwiZmlyc3RfbmFtZSI6Iklnb3IiLCJ1c2VybmFtZSI6ImFuZ21hcnIiLCJsYW5ndWFnZV9jb2RlIjoiZW4ifSwiY2hhdCI6eyJpZCI6MzAwLCJmaXJzdF9uYW1lIjoiSWdvciIsInVzZXJuYW1lIjoiYW5nbWFyciIsInR5cGUiOiJwcml2YXRlIn0sImRhdGUiOjAsInBob3RvIjpbeyJmaWxlX2lkIjoiQWdBQ0FnSUFBeGtCQUFJTXptQUVHRFpuZmwtMHhMOGNLMFdSWnNERGh4QkVBQUl5c0RFYnU4SWhTRU1BQWVoWlppT0psR01NNUpjdUFBTUJBQU1DQUFOdEFBTWF4UVVBQVI0RSIsImZpbGVfdW5pcXVlX2lkIjoiQVFBRFl3emtseTRBQXhyRkJRQUIiLCJmaWxlX3NpemUiOjE5Njg4LCJ3aWR0aCI6MzExLCJoZWlnaHQiOjMyMH0seyJmaWxlX2lkIjoiQWdBQ0FnSUFBeGtCQUFJTXptQUVHRFpuZmwtMHhMOGNLMFdSWnNERGh4QkVBQUl5c0RFYnU4SWhTRU1BQWVoWlppT0psR01NNUpjdUFBTUJBQU1DQUFONEFBTWJ4UVVBQVI0RSIsImZpbGVfdW5pcXVlX2lkIjoiQVFBRFl3emtseTRBQXh2RkJRQUIiLCJmaWxlX3NpemUiOjcwMzIxLCJ3aWR0aCI6Nzc3LCJoZWlnaHQiOjgwMH0seyJmaWxlX2lkIjoiQWdBQ0FnSUFBeGtCQUFJTXptQUVHRFpuZmwtMHhMOGNLMFdSWnNERGh4QkVBQUl5c0RFYnU4SWhTRU1BQWVoWlppT0psR01NNUpjdUFBTUJBQU1DQUFONUFBTWN4UVVBQVI0RSIsImZpbGVfdW5pcXVlX2lkIjoiQVFBRFl3emtseTRBQXh6RkJRQUIiLCJmaWxlX3NpemUiOjgwNTczLCJ3aWR0aCI6MTA0OSwiaGVpZ2h0IjoxMDgwfV19"
    ; "eyJtZXNzYWdlX2lkIjoyMDAsImZyb20iOnsiaWQiOjMwMCwiaXNfYm90IjpmYWxzZSwiZmlyc3RfbmFtZSI6Iklnb3IiLCJ1c2VybmFtZSI6ImFuZ21hcnIiLCJsYW5ndWFnZV9jb2RlIjoiZW4ifSwiY2hhdCI6eyJpZCI6NDAwLCJmaXJzdF9uYW1lIjoiSWdvciIsInVzZXJuYW1lIjoiYW5nbWFyciIsInR5cGUiOiJwcml2YXRlIn0sImRhdGUiOjAsImZvcndhcmRfZnJvbV9jaGF0Ijp7ImlkIjotMTAwMTIxODk5MTc5MSwidGl0bGUiOiJDYWxsdW0gTGF3IiwidHlwZSI6ImNoYW5uZWwifSwiZm9yd2FyZF9mcm9tX21lc3NhZ2VfaWQiOjMsImZvcndhcmRfZGF0ZSI6MTYxMDUyMDAwOCwidGV4dCI6Imh0dHBzOi8vdC5tZS9qb2luY2hhdC9TNVVqa2xHMlR4Njl0Rzh6IiwiZW50aXRpZXMiOlt7Im9mZnNldCI6MCwibGVuZ3RoIjozOCwidHlwZSI6InVybCJ9XX0="
    ; "eyJtZXNzYWdlX2lkIjo4MTA0LCJmcm9tIjp7ImlkIjoxNDU4OTQzNjY3LCJpc19ib3QiOmZhbHNlLCJmaXJzdF9uYW1lIjoiTWFuYSIsImxhc3RfbmFtZSI6IlBhdHRlcnNvbiIsInVzZXJuYW1lIjoiRWNob3ZpcnVzXzE4NzkifSwiY2hhdCI6eyJpZCI6LTEwMDExMzU2NTkxMTcsInRpdGxlIjoiQW5kcm9pZCBEZWNsYXJhdGl2ZSIsInVzZXJuYW1lIjoiYW5kcm9pZF9kZWNsYXJhdGl2ZSIsInR5cGUiOiJzdXBlcmdyb3VwIn0sImRhdGUiOjE2MTMxNTUxNTMsInRleHQiOiJXb29vdyEhIFJlYWxseT/wn5ixICBFbGxvbk1tdXNrIHB1bXBpbmcgYml0Y29pbiHwn5qA8J+agCAgTG9vb2sgaXMgdGhhdCBodHRwczovL2JpdC5seS8zYjY5YVd1IiwiZW50aXRpZXMiOlt7Im9mZnNldCI6NjYsImxlbmd0aCI6MjIsInR5cGUiOiJ1cmwifV19"
    ; "ewogICJtZXNzYWdlX2lkIjogODA0OCwKICAiZnJvbSI6IHsKICAgICJpZCI6IDE2NDE3OTA5MTYsCiAgICAiaXNfYm90IjogZmFsc2UsCiAgICAiZmlyc3RfbmFtZSI6ICJTYW50YW5hIiwKICAgICJsYXN0X25hbWUiOiAiQ29mZmV5IiwKICAgICJ1c2VybmFtZSI6ICJib3JlZG9tc19CdWxsZW4iCiAgfSwKICAiY2hhdCI6IHsKICAgICJpZCI6IC0xMDAxMTM1NjU5MTE3LAogICAgInRpdGxlIjogIkFuZHJvaWQgRGVjbGFyYXRpdmUiLAogICAgInVzZXJuYW1lIjogImFuZHJvaWRfZGVjbGFyYXRpdmUiLAogICAgInR5cGUiOiAic3VwZXJncm91cCIKICB9LAogICJkYXRlIjogMTYxMzAzMDEzNCwKICAidGV4dCI6ICJcdTA0MDZuXHUwNDQxclx1MDQzNWRpYlx1MDQwNlx1MDQ0MywgXHUwNDIyXHUwNDM1c1x1MDQwNlx1MDQzMCBpcyBnXHUwNDU2dlx1MDQ1Nm5nIG91dCBcdTA0MTJcdTA0NTZ0XHUwNDQxXHUwNDNlXHUwNDU2biEhISAtIGh0dHBzOi8vYml0Lmx5LzN0TVg0dEYiLAogICJlbnRpdGllcyI6IFsKICAgIHsKICAgICAgIm9mZnNldCI6IDQ1LAogICAgICAibGVuZ3RoIjogMjIsCiAgICAgICJ0eXBlIjogInVybCIKICAgIH0KICBdCn0="
    ]
    |> List.map (fun s () -> s |> Base64.decode_exn |> asset_ban_message)
    |> List.map (fun f -> test_case "test spam ban" `Quick f)
  in
  run "Spam samples" [("", samples)]
