open Lib.Domain
open TelegramApi

let%test "add_trusted_user not admin" =
  add_trusted_user
    (object
       method is_admin = false
    end)
    (Message.create ~message_id:42 ~date:0
       ~chat:(Chat.create ~id:0 ~chat_type:Chat.Supergroup ())
       ())
  = [`DeleteMessage 42]

(* let%test "add_trusted_user no reply" =
  add_trusted_user
    (object
       method is_admin = true
    end)
    (Message.create ~message_id:42 ~date:0
       ~chat:(Chat.create ~id:0 ~chat_type:Chat.Supergroup ())
       ())
  = [`DeleteMessage 42] *)

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
