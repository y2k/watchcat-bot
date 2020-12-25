module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type state = {trusted_users: IntSet.t; users_reg_time: float IntMap.t}

module Serializer = struct
  type event = TrustedUserAdded of int [@@deriving yojson {strict= false}]

  let empty_state = {trusted_users= IntSet.empty; users_reg_time= IntMap.empty}

  let serialize current =
    IntSet.to_seq current.trusted_users
    |> Seq.map (fun user -> TrustedUserAdded user)

  let restore state (TrustedUserAdded x) =
    {state with trusted_users= IntSet.add x state.trusted_users}
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
  |> function
  | Some {entity_type= TextMention user; _} -> Some user.id | _ -> None

open TelegramApi.Message

let add_trusted_user env {entities; _} =
  match env#is_admin with
  | true -> (
    match find_user_in_message entities with
    | Some user_id ->
        let state = env#state in
        [ `UpdateState
            {state with trusted_users= IntSet.add user_id state.trusted_users}
        ]
    | None ->
        [`SendMessage "Пользователь не указан"] )
  | false ->
      []

let remove_trusted_user env {entities; _} =
  match env#is_admin with
  | true -> (
    match find_user_in_message entities with
    | Some user_id ->
        let state = env#state in
        [ `UpdateState
            {state with trusted_users= IntSet.remove user_id state.trusted_users}
        ]
    | None ->
        [`SendMessage "Пользователь не указан"] )
  | false ->
      []

let try_ban env msg =
  let try_ban user_id bar_user_id message_id =
    let state = env#state in
    if env#is_admin then [`RemoveMessage message_id; `KickUser bar_user_id]
    else if IntSet.mem user_id state.trusted_users then
      match IntMap.find_opt bar_user_id state.users_reg_time with
      | Some reg_date ->
          let reg_duration = env#now -. reg_date in
          if reg_duration < 2.0 *. 24.0 *. 3600.0 then
            [`RemoveMessage message_id; `KickUser bar_user_id]
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
  | { from= Some {id= user_id; _}
    ; reply_to_message= Some {from= Some {id= repl_user_id; _}; message_id; _}
    ; _ } ->
      try_ban user_id repl_user_id message_id
  | _ ->
      []

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
