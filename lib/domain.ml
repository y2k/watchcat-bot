module IntMap = Map.Make (Int)

type user_key = {chat_id: Int.t; user_id: Int.t} [@@deriving compare]

type user_info = {name: string}

module UserMap = Map.Make (struct
  type t = user_key [@@deriving compare]
end)

type state = {trusted_users: user_info UserMap.t}

module StateEvents = struct
  type event =
    | TrustedUserAdded of {chat_id: int; user_id: int; name: string}
    | TrustedUserDeleted of {chat_id: int; user_id: int}
  [@@deriving yojson]

  let empty_state = {trusted_users= UserMap.empty}

  let restore state = function
    | TrustedUserAdded {chat_id; user_id; name} ->
        { trusted_users=
            UserMap.add {chat_id; user_id} {name} state.trusted_users }
    | TrustedUserDeleted {chat_id; user_id} ->
        {trusted_users= UserMap.remove {chat_id; user_id} state.trusted_users}
end

let new_chat_member _env _user_id = []

let find_user_in_message' entities =
  let open TelegramApi.MessageEntity in
  entities
  |> Option.fold ~none:[] ~some:Fun.id
  |> List.find_opt (fun x ->
         match x.entity_type with TextMention _ -> true | _ -> false )
  |> function Some {entity_type= TextMention user; _} -> Some user | _ -> None

open TelegramApi.Message

let find_user_in_message msg =
  match msg with
  | {reply_to_message= Some {from= Some user; _}; _} ->
      Some user
  | _ ->
      find_user_in_message' msg.entities

let user_to_string {TelegramApi.User.first_name; username; _} =
  username
  |> Option.fold ~none:"" ~some:(fun un -> " (@" ^ un ^ ")")
  |> Printf.sprintf "%s%s" first_name

let add_trusted_user' ({chat= {id= chat_id; _}; _} as msg) =
  match find_user_in_message msg with
  | Some trusted_user ->
      let tu_title = user_to_string trusted_user in
      [ `UpdateState
          [ StateEvents.TrustedUserAdded
              {chat_id; user_id= trusted_user.id; name= tu_title} ] ]
  | None ->
      [`SendMessage "Пользователь не указан"]

let remove_trusted_user' ({chat= {id= chat_id; _}; _} as msg) =
  match find_user_in_message msg with
  | Some trusted_user ->
      [ `UpdateState
          [StateEvents.TrustedUserDeleted {chat_id; user_id= trusted_user.id}]
      ]
  | None ->
      [`SendMessage "Пользователь не указан"]

let try_ban env msg =
  let is_spam reply_msg =
    let jc_regex = Str.regexp "https://t\\.me/joinchat/.+" in
    let bit_regex = Str.regexp ".*https://bit\\.ly/.+" in
    (Option.is_none reply_msg.text && Option.is_some reply_msg.photo)
    || Str.string_match jc_regex
         (reply_msg.text |> Option.fold ~none:"" ~some:Fun.id)
         0
    || Str.string_match bit_regex
         (reply_msg.text |> Option.fold ~none:"" ~some:Fun.id)
         0
  in
  match msg with
  | { from= Some {id= user_id; _}
    ; reply_to_message= Some ({from= Some {id= spam_user_id; _}; _} as spam_msg)
    ; _ } ->
      let state = env#state
      and delete_spam =
        [ `DeleteMessage spam_msg.message_id
        ; `KickUser spam_user_id
        ; `DeleteMessage msg.message_id ]
      in
      if env#is_admin then delete_spam
      else if
        UserMap.mem {chat_id= msg.chat.id; user_id} state.trusted_users
        && msg.date - spam_msg.date <= 90
        && is_spam spam_msg
      then delete_spam
      else [`DeleteMessage msg.message_id]
  | _ ->
      [`DeleteMessage msg.message_id]

type ('env, 'a) user_command =
  {name: string; description: string; run: 'env -> message -> 'a}

let only_admin f env msg = match env#is_admin with true -> f msg | false -> []

let delete_message f msg = `DeleteMessage msg.message_id :: f msg

let add_trusted_user msg = delete_message (only_admin add_trusted_user' msg)

let remove_trusted_user msg =
  delete_message (only_admin remove_trusted_user' msg)

let user_commands =
  [ {name= "ban"; description= "Забанить пользователя"; run= try_ban}
  ; { name= "baka"
    ; description= "Забанить пользователя (синоним ban)"
    ; run= try_ban }
  ; { name= "add"
    ; description= "Добавить доверенного пользователя"
    ; run= add_trusted_user }
  ; { name= "remove"
    ; description= "Удалить доверенного пользователя"
    ; run= remove_trusted_user }
  ; {name= "version"; description= "Версия 0.2"; run= (fun _ _ -> [])} ]
