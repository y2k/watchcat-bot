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
  let actual =
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
  in
  Alcotest.(check bool) "" true (actual = expected)

let call_try_ban_by_trusted_user_to_late () =
  run_test
    { empty_env with
      func= try_ban
    ; message_date= 91
    ; state=
        { trusted_users=
            UserMap.singleton {chat_id= 500; user_id= 400} {name= ""} } }
    [`DeleteMessage 100]

let call_try_ban_by_trusted_user () =
  run_test
    { empty_env with
      func= try_ban
    ; is_reply= true
    ; message_date= 90
    ; state=
        { trusted_users=
            UserMap.singleton {chat_id= 500; user_id= 400} {name= ""} } }
    [`DeleteMessage 200; `KickUser 300; `DeleteMessage 100]

let call_try_ban_by_admin () =
  run_test
    {empty_env with func= try_ban; is_admin= true; is_reply= true}
    [`DeleteMessage 200; `KickUser 300; `DeleteMessage 100]

let call_try_ban_with_no_permission () =
  run_test {empty_env with func= try_ban} [`DeleteMessage 100]

let add_trusted_user_not_admin () =
  run_test {empty_env with func= add_trusted_user} [`DeleteMessage 100]

let add_trusted_user_no_reply () =
  run_test
    {empty_env with func= add_trusted_user; is_admin= true}
    [`DeleteMessage 100; `SendMessage "Пользователь не указан"]

let add_trusted_user () =
  run_test
    { empty_env with
      func= add_trusted_user
    ; is_admin= true
    ; mention_user_id= Some 200 }
    [ `DeleteMessage 100
    ; `UpdateState
        [ StateEvents.TrustedUserAdded
            {chat_id= 500; user_id= 200; name= "mention_user"} ] ]

let remove_trusted_user_not_admin () =
  run_test {empty_env with func= remove_trusted_user} [`DeleteMessage 100]

let remove_trusted_user_no_reply () =
  run_test
    {empty_env with func= remove_trusted_user; is_admin= true}
    [`DeleteMessage 100; `SendMessage "Пользователь не указан"]

let remove_trusted_user () =
  run_test
    { empty_env with
      func= remove_trusted_user
    ; mention_user_id= Some 200
    ; is_admin= true }
    [ `DeleteMessage 100
    ; `UpdateState [StateEvents.TrustedUserDeleted {chat_id= 500; user_id= 200}]
    ]

let () =
  let open Alcotest in
  run "E2E"
    [ ( ""
      , [ test_case "call_try_ban_by_trusted_user_to_late" `Quick
            call_try_ban_by_trusted_user_to_late
        ; test_case "call_try_ban_by_trusted_user" `Quick
            call_try_ban_by_trusted_user
        ; test_case "call_try_ban_by_admin" `Quick call_try_ban_by_admin
        ; test_case "call_try_ban_with_no_permission" `Quick
            call_try_ban_with_no_permission
        ; test_case "add_trusted_user_not_admin" `Quick
            add_trusted_user_not_admin
        ; test_case "add_trusted_user_no_reply" `Quick add_trusted_user_no_reply
        ; test_case "add_trusted_user" `Quick add_trusted_user
        ; test_case "remove_trusted_user_not_admin" `Quick
            remove_trusted_user_not_admin
        ; test_case "remove_trusted_user_no_reply" `Quick
            remove_trusted_user_no_reply
        ; test_case "remove_trusted_user" `Quick remove_trusted_user ] ) ]
