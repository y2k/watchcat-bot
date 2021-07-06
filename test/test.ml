open Lib.Domain
open TelegramApi

type ('f, 'eff) env =
  { func: 'f -> Message.message -> 'eff list
  ; is_admin: bool
  ; state: state
  ; mention_user_id: int option
  ; message_date: int
  ; is_reply: bool }

let empty_env =
  { func= (fun _ _ -> [])
  ; is_admin= false
  ; state= {trusted_users= UserMap.empty}
  ; mention_user_id= None
  ; message_date= 0
  ; is_reply= false }

let run_test env expected =
  env.func
    (object
       method is_admin = env.is_admin

       method state = env.state
    end )
    (Message.create ~message_id:100 ~date:env.message_date
       ~from:(Some (User.create ~id:400 ~first_name:"" ()))
       ~entities:
         ( match env.mention_user_id with
         | Some user_id ->
             Some
               [ MessageEntity.create
                   ~entity_type:
                     (TextMention
                        (User.create ~id:user_id ~first_name:"mention_user" ())
                     )
                   ~offset:0 ~length:0 () ]
         | None ->
             None )
       ~reply_to:
         ( if env.is_reply then
           Some
             (Message.create ~message_id:200 ~date:0 ~photo:(Some [])
                ~from:(Some (User.create ~id:300 ~first_name:"" ()))
                ~chat:(Chat.create ~id:500 ~chat_type:Chat.Supergroup ())
                () )
         else None )
       ~chat:(Chat.create ~id:500 ~chat_type:Chat.Supergroup ())
       () )
  = expected

let%test "call try_ban by trusted user to late" =
  run_test
    { empty_env with
      func= try_ban
    ; message_date= 91
    ; state=
        { trusted_users=
            UserMap.singleton {chat_id= 500; user_id= 400} {name= ""} } }
    [`DeleteMessage 100]

let%test "call try_ban by trusted user" =
  run_test
    { empty_env with
      func= try_ban
    ; is_reply= true
    ; message_date= 90
    ; state=
        { trusted_users=
            UserMap.singleton {chat_id= 500; user_id= 400} {name= ""} } }
    [`DeleteMessage 200; `KickUser 300; `DeleteMessage 100]

let%test "call try_ban by admin" =
  run_test
    {empty_env with func= try_ban; is_admin= true; is_reply= true}
    [`DeleteMessage 200; `KickUser 300; `DeleteMessage 100]

let%test "call try_ban with no permission" =
  run_test {empty_env with func= try_ban} [`DeleteMessage 100]

let%test "add_trusted_user not admin" =
  run_test {empty_env with func= add_trusted_user} [`DeleteMessage 100]

let%test "add_trusted_user no reply" =
  run_test
    {empty_env with func= add_trusted_user; is_admin= true}
    [ `DeleteMessage 100
    ; `SendMessage "Пользователь не указан" ]

let%test "add_trusted_user" =
  run_test
    { empty_env with
      func= add_trusted_user
    ; is_admin= true
    ; mention_user_id= Some 200 }
    [ `UpdateState
        [ StateEvents.TrustedUserAdded
            {chat_id= 500; user_id= 200; name= "mention_user"} ]
    ; `DeleteMessage 100 ]

let%test "remove_trusted_user not admin" =
  run_test {empty_env with func= remove_trusted_user} [`DeleteMessage 100]

let%test "remove_trusted_user no reply" =
  run_test
    {empty_env with func= remove_trusted_user; is_admin= true}
    [ `DeleteMessage 100
    ; `SendMessage "Пользователь не указан" ]

let%test "remove_trusted_user" =
  run_test
    { empty_env with
      func= remove_trusted_user
    ; mention_user_id= Some 200
    ; is_admin= true }
    [ `UpdateState [StateEvents.TrustedUserDeleted {chat_id= 500; user_id= 200}]
    ; `DeleteMessage 100 ]
