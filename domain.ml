module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type state = {trusted_users: IntSet.t; user_registerd: float IntMap.t}

let empty_state = {trusted_users= IntSet.empty; user_registerd= IntMap.empty}

let try_ban env user_id bar_user_id message_id =
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
                 "Пользователь добавился слишком \
                  давно (%f сек)."
                 reg_duration) ]
    | _ ->
        [ `SendMessage
            "Пользователь добавился слишком \
             давно или ненайден." ]
  else
    [ `SendMessage
        "Извините, но вас нет в списке \
         доверенных пользователей.\n\
         Обратитесь пожалуйста к \
         администраторам, что бы вас добавили."
    ]

let add_trusted_user env user_id =
  match user_id with
  | Some user_id ->
      let state = env#state in
      `UpdateState
        {state with trusted_users= IntSet.add user_id state.trusted_users}
  | None ->
      `SendMessage "Пользователь не указан"

let remove_trusted_user env user_id =
  match user_id with
  | Some user_id ->
      let state = env#state in
      `UpdateState
        {state with trusted_users= IntSet.remove user_id state.trusted_users}
  | None ->
      `SendMessage "Пользователь не указан"

let new_chat_member env user_id =
  let state = env#state in
  `UpdateState
    {state with user_registerd= IntMap.add user_id env#now state.user_registerd}
