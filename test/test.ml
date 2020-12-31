open Lib.Domain
open TelegramApi

let%test "call try_ban by trusted user" =
  try_ban
    (object
       method is_admin = false

       method state =
         { trusted_users=
             UserMap.singleton {chat_id= 500; user_id= 400} {name= ""} }
    end)
    (Message.create ~message_id:100 ~date:0
       ~from:(Some (User.create ~id:400 ~first_name:"" ()))
       ~reply_to:
         (Some
            (Message.create ~message_id:200 ~date:0
               ~from:(Some (User.create ~id:300 ~first_name:"" ()))
               ~chat:(Chat.create ~id:500 ~chat_type:Chat.Supergroup ())
               ()))
       ~chat:(Chat.create ~id:500 ~chat_type:Chat.Supergroup ())
       ())
  = [`DeleteMessage 200; `KickUser 300; `DeleteMessage 100]

let%test "call try_ban by admin" =
  try_ban
    (object
       method is_admin = true

       method state = {trusted_users= UserMap.empty}
    end)
    (Message.create ~message_id:100 ~date:0
       ~from:(Some (User.create ~id:400 ~first_name:"" ()))
       ~reply_to:
         (Some
            (Message.create ~message_id:200 ~date:0
               ~from:(Some (User.create ~id:300 ~first_name:"" ()))
               ~chat:(Chat.create ~id:500 ~chat_type:Chat.Supergroup ())
               ()))
       ~chat:(Chat.create ~id:500 ~chat_type:Chat.Supergroup ())
       ())
  = [`DeleteMessage 200; `KickUser 300; `DeleteMessage 100]

let%test "call try_ban with no permission" =
  try_ban
    (object
       method is_admin = false

       method state = {trusted_users= UserMap.empty}
    end)
    (Message.create ~message_id:42 ~date:0
       ~chat:(Chat.create ~id:0 ~chat_type:Chat.Supergroup ())
       ())
  = [`DeleteMessage 42]

let%test "add_trusted_user not admin" =
  add_trusted_user
    (object
       method is_admin = false
    end)
    (Message.create ~message_id:42 ~date:0
       ~chat:(Chat.create ~id:0 ~chat_type:Chat.Supergroup ())
       ())
  = [`DeleteMessage 42]

let%test "add_trusted_user no reply" =
  add_trusted_user
    (object
       method is_admin = true
    end)
    (Message.create ~message_id:42 ~date:0
       ~chat:(Chat.create ~id:0 ~chat_type:Chat.Supergroup ())
       ())
  = [ `DeleteMessage 42
    ; `SendMessage "Пользователь не указан" ]

let%test "add_trusted_user" =
  add_trusted_user
    (object
       method is_admin = true
    end)
    (Message.create
       ~entities:
         (Some
            [ MessageEntity.create
                ~entity_type:
                  (TextMention (User.create ~id:200 ~first_name:"user" ()))
                ~offset:0 ~length:0 () ])
       ~message_id:42 ~date:0
       ~chat:(Chat.create ~id:100 ~chat_type:Chat.Supergroup ())
       ())
  = [ `UpdateState
        [StateEvents.TrustedUserAdded {chat_id= 100; user_id= 200; name= "user"}]
    ; `DeleteMessage 42 ]

let%test "remove_trusted_user not admin" =
  remove_trusted_user
    (object
       method is_admin = false
    end)
    (Message.create ~message_id:42 ~date:0
       ~chat:(Chat.create ~id:0 ~chat_type:Chat.Supergroup ())
       ())
  = [`DeleteMessage 42]

let%test "remove_trusted_user no reply" =
  remove_trusted_user
    (object
       method is_admin = true
    end)
    (Message.create ~message_id:42 ~date:0
       ~chat:(Chat.create ~id:0 ~chat_type:Chat.Supergroup ())
       ())
  = [ `DeleteMessage 42
    ; `SendMessage "Пользователь не указан" ]

let%test "remove_trusted_user" =
  remove_trusted_user
    (object
       method is_admin = true
    end)
    (Message.create
       ~entities:
         (Some
            [ MessageEntity.create
                ~entity_type:
                  (TextMention (User.create ~id:200 ~first_name:"user" ()))
                ~offset:0 ~length:0 () ])
       ~message_id:42 ~date:0
       ~chat:(Chat.create ~id:100 ~chat_type:Chat.Supergroup ())
       ())
  = [ `UpdateState [StateEvents.TrustedUserDeleted {chat_id= 100; user_id= 200}]
    ; `DeleteMessage 42 ]
