module IntMap = Map.Make (Int)

type user_key = {chat_id: Int.t; user_id: Int.t} [@@deriving compare]

type user_info = {name: string}

module UserMap = Map.Make (struct
  type t = user_key [@@deriving compare]
end)

type state = {trusted_users: user_info UserMap.t; users_reg_time: float IntMap.t}

module Serializer = struct
  type event = TrustedUserAdded of {chat_id: int; user_id: int; name: string}
  [@@deriving yojson {strict= false}]

  let empty_state = {trusted_users= UserMap.empty; users_reg_time= IntMap.empty}

  let serialize current =
    UserMap.to_seq current.trusted_users
    |> Seq.map (fun ({chat_id; user_id}, {name}) ->
           TrustedUserAdded {chat_id; user_id; name})

  let restore state (TrustedUserAdded {chat_id; user_id; name}) =
    { state with
      trusted_users= UserMap.add {chat_id; user_id} {name} state.trusted_users
    }
end

let new_chat_member env user_id =
  let state = env#state in
  [ `UpdateState
      { state with
        users_reg_time= IntMap.add user_id env#now state.users_reg_time } ]

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
        let tu_title = user_to_string trusted_user and state = env#state in
        [ `UpdateState
            { state with
              trusted_users=
                UserMap.add
                  {chat_id; user_id= trusted_user.id}
                  {name= tu_title} state.trusted_users }
        ; `DeleteMessage message_id ]
    | None ->
        [`SendMessage "Пользователь не указан"] )
  | false ->
      []

let remove_trusted_user env {chat= {id= chat_id; _}; message_id; entities; _} =
  match env#is_admin with
  | true -> (
    match find_user_in_message entities with
    | Some trusted_user ->
        let state = env#state in
        [ `UpdateState
            { state with
              trusted_users=
                UserMap.remove
                  {chat_id; user_id= trusted_user.id}
                  state.trusted_users }
        ; `DeleteMessage message_id ]
    | None ->
        [`SendMessage "Пользователь не указан"] )
  | false ->
      []

let try_ban env ({chat= {id= chat_id; _}; message_id; _} as msg) =
  match msg with
  | { from= Some {id= user_id; _}
    ; reply_to_message=
        Some {from= Some {id= spam_user_id; _}; message_id= spam_message_id; _}
    ; _ } ->
      let state = env#state
      and delete_spam =
        [ `DeleteMessage spam_message_id
        ; `KickUser spam_user_id
        ; `DeleteMessage message_id ]
      in
      if env#is_admin then delete_spam
      else if UserMap.mem {chat_id; user_id} state.trusted_users then
        delete_spam
      else [`DeleteMessage message_id]
  | _ ->
      [`DeleteMessage message_id]

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
