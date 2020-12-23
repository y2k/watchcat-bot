module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type state = {trusted_users: IntSet.t; user_registerd: float IntMap.t}

let empty_state = {trusted_users= IntSet.empty; user_registerd= IntMap.empty}

let new_chat_member env user_id =
  let state = env#state in
  [ `UpdateState
      { state with
        user_registerd= IntMap.add user_id env#now state.user_registerd } ]

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
  match find_user_in_message entities with
  | Some user_id ->
      let state = env#state in
      [ `UpdateState
          {state with trusted_users= IntSet.add user_id state.trusted_users} ]
  | None ->
      [`SendMessage "Пользователь не указан"]

let remove_trusted_user env {entities; _} =
  match find_user_in_message entities with
  | Some user_id ->
      let state = env#state in
      [ `UpdateState
          {state with trusted_users= IntSet.remove user_id state.trusted_users}
      ]
  | None ->
      [`SendMessage "Пользователь не указан"]

let try_ban env msg =
  let try_ban user_id bar_user_id message_id =
    let state = env#state in
    if IntSet.mem user_id state.trusted_users then
      match IntMap.find_opt bar_user_id state.user_registerd with
      | Some reg_date ->
          let reg_duration = env#now -. reg_date in
          if reg_duration < 2.0 *. 24.0 *. 3600.0 then
            [`RemoveMessage message_id; `KickUser bar_user_id]
          else
            [ `SendMessage
                (Printf.sprintf
                   {|Пользователь добавился слишком давно (%f сек).|}
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
      failwith "???"

type ('env, 'a) user_command =
  {name: string; description: string; auth: bool; run: 'env -> message -> 'a}

let user_commands =
  [ { name= "ban"
    ; description= "Забанить пользователя"
    ; auth= false
    ; run= try_ban }
  ; { name= "add"
    ; description=
        "Добавить доверенного пользователя"
    ; auth= true
    ; run= add_trusted_user }
  ; { name= "remove"
    ; description=
        "Удалить доверенного пользователя"
    ; auth= true
    ; run= remove_trusted_user } ]
