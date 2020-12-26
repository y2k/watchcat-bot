module IntMap = Map.Make (Int)

type state = {trusted_users: string IntMap.t; users_reg_time: float IntMap.t}

module Serializer = struct
  type event = TrustedUserAdded of int * string
  [@@deriving yojson {strict= false}]

  let empty_state = {trusted_users= IntMap.empty; users_reg_time= IntMap.empty}

  let serialize current =
    IntMap.to_seq current.trusted_users
    |> Seq.map (fun (id, name) -> TrustedUserAdded (id, name))

  let restore state (TrustedUserAdded (id, name)) =
    {state with trusted_users= IntMap.add id name state.trusted_users}
end

let new_chat_member env user_id =
  let state = env#state in
  [ `UpdateState
      { state with
        users_reg_time= IntMap.add user_id env#now state.users_reg_time } ]

let find_user_in_message entities =
  let open TelegramApi.MessageEntity in
  entities
  |> Option.fold ~none:[] ~some:(fun x -> x)
  |> List.find_opt (fun x ->
         match x.entity_type with TextMention _ -> true | _ -> false)
  |> function Some {entity_type= TextMention user; _} -> Some user | _ -> None

open TelegramApi.Message

let user_to_string {TelegramApi.User.first_name; username; _} =
  Printf.sprintf "%s%s" first_name
    (username |> Option.fold ~none:"" ~some:(fun un -> " (@" ^ un ^ ")"))

let add_trusted_user env {message_id; entities; _} =
  match env#is_admin with
  | true -> (
    match find_user_in_message entities with
    | Some trusted_user ->
        let tu_title = user_to_string trusted_user and state = env#state in
        [ `UpdateState
            { state with
              trusted_users=
                IntMap.add trusted_user.id tu_title state.trusted_users }
        ; `DeleteMessage message_id ]
    | None ->
        [`SendMessage "Пользователь не указан"] )
  | false ->
      []

let remove_trusted_user env {message_id; entities; _} =
  match env#is_admin with
  | true -> (
    match find_user_in_message entities with
    | Some trusted_user ->
        let state = env#state in
        [ `UpdateState
            { state with
              trusted_users= IntMap.remove trusted_user.id state.trusted_users
            }
        ; `DeleteMessage message_id ]
    | None ->
        [`SendMessage "Пользователь не указан"] )
  | false ->
      []

let try_ban env msg =
  let try_ban message_id user_id spam_user_id spam_message_id =
    let state = env#state
    and delete_spam =
      [ `DeleteMessage spam_message_id
      ; `KickUser spam_user_id
      ; `DeleteMessage message_id ]
    in
    if env#is_admin then delete_spam
    else if IntMap.mem user_id state.trusted_users then
      match IntMap.find_opt spam_user_id state.users_reg_time with
      | Some reg_date ->
          let reg_duration = env#now -. reg_date in
          if reg_duration < 2.0 *. 24.0 *. 3600.0 then delete_spam
          else
            [ `SendMessage
                (Printf.sprintf
                   {|Пользователь добавился слишком давно (%g сек).|}
                   reg_duration) ]
      | _ ->
          [ `SendMessage
              {|Пользователь добавился слишком давно или ненайден.|}
          ]
    else
      [ `SendMessage
          {|Извините, но вас нет в списке доверенных пользователей. Обратитесь пожалуйста к администраторам, что бы вас добавили.|}
      ]
  in
  match msg with
  | { message_id
    ; from= Some {id= user_id; _}
    ; reply_to_message=
        Some {from= Some {id= repl_user_id; _}; message_id= spam_message_id; _}
    ; _ } ->
      try_ban message_id user_id repl_user_id spam_message_id
  | {message_id; _} ->
      [`DeleteMessage message_id]

type ('env, 'a) user_command =
  {name: string; description: string; run: 'env -> message -> 'a}

let user_commands =
  [ { name= "ban"
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
