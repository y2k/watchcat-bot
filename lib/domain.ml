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

let find_user_in_message entities =
  let open TelegramApi.MessageEntity in
  entities
  |> Option.fold ~none:[] ~some:Fun.id
  |> List.find_opt (fun x ->
         match x.entity_type with TextMention _ -> true | _ -> false)
  |> function Some {entity_type= TextMention user; _} -> Some user | _ -> None

let user_to_string {TelegramApi.User.first_name; username; _} =
  Printf.sprintf "%s%s" first_name
    (username |> Option.fold ~none:"" ~some:(fun un -> " (@" ^ un ^ ")"))

open TelegramApi.Message

let add_trusted_user env {chat= {id= chat_id; _}; message_id; entities; _} =
  match env#is_admin with
  | true -> (
    match find_user_in_message entities with
    | Some trusted_user ->
        let tu_title = user_to_string trusted_user in
        [ `UpdateState
            [ StateEvents.TrustedUserAdded
                {chat_id; user_id= trusted_user.id; name= tu_title} ]
        ; `DeleteMessage message_id ]
    | None ->
        [ `DeleteMessage message_id
        ; `SendMessage "Пользователь не указан" ] )
  | false ->
      [`DeleteMessage message_id]

let remove_trusted_user env {chat= {id= chat_id; _}; message_id; entities; _} =
  match env#is_admin with
  | true -> (
    match find_user_in_message entities with
    | Some trusted_user ->
        [ `UpdateState
            [StateEvents.TrustedUserDeleted {chat_id; user_id= trusted_user.id}]
        ; `DeleteMessage message_id ]
    | None ->
        [ `DeleteMessage message_id
        ; `SendMessage "Пользователь не указан" ] )
  | false ->
      [`DeleteMessage message_id]

let try_ban env msg =
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
        && Option.is_none spam_msg.text
        && Option.is_some spam_msg.photo
      then delete_spam
      else [`DeleteMessage msg.message_id]
  | _ ->
      [`DeleteMessage msg.message_id]

type ('env, 'a) user_command =
  {name: string; description: string; run: 'env -> message -> 'a}

let user_commands =
  [ { name= "baka"
    ; description= "Забанить пользователя"
    ; run= try_ban }
  ; { name= "add"
    ; description=
        "Добавить доверенного пользователя"
    ; run= add_trusted_user }
  ; { name= "remove"
    ; description=
        "Удалить доверенного пользователя"
    ; run= remove_trusted_user } ]
