module IntMap = Map.Make (Int)

module Domain = struct
  let trusted_users = [241854720]

  let user_registerd : int IntMap.t = IntMap.empty

  let contains xs x =
    match List.find_opt (fun i -> x = i) xs with
    | Some _ ->
        true
    | None ->
        false

  let try_ban userId banUserId =
    if contains trusted_users userId then
      let regDate = IntMap.find_opt banUserId user_registerd in
      match regDate with Some _ -> `KickUser banUserId | None -> `None
    else `None
end

open Telegram.Api

module WatchcatBot = Mk (struct
  open Chat
  open Command
  open Message
  include Telegram.BotDefaults

  let token = Sys.getenv "TELEGRAM_TOKEN"

  let command_postfix = Some "watchman"

  let handleEffect chatId userId e =
    let open Telegram.Actions in
    match e with
    | `KickUser user ->
        kick_chat_member ~chat_id:chatId ~user_id:user
    | `None ->
        send_message ~chat_id:chatId "UserID: %i" userId

  let commands =
    let try_ban {chat= {id= chatId; _}; from; reply_to_message; _} =
      match (from, reply_to_message) with
      | Some {id= userId; _}, Some {from= Some {id= repl_user_id; _}; _} ->
          Domain.try_ban userId repl_user_id |> handleEffect chatId userId
      | _ ->
          failwith "???"
    in
    [{name= "ban"; description= "Ban user"; enabled= true; run= try_ban}]
end)

open Lwt

let run ?(log = true) () =
  let process = function
    | Result.Success _ ->
        return ()
    | Result.Failure e ->
        if log && e <> "Could not get head" then
          (* Ignore spam *)
          Lwt_io.printl e
        else return ()
  in
  let rec loop () =
    WatchcatBot.pop_update ~run_cmds:true () >>= process >>= loop
  in
  print_endline "Bot started..." ;
  while true do
    (* Recover from errors if an exception is thrown *)
    try Lwt_main.run @@ loop ()
    with e -> print_endline @@ Printexc.to_string e
  done

let () = run ()
