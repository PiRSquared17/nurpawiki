open Lwt
open XHTML.M
open Eliomservices
open Eliomparameters
open Eliomsessions
open Eliompredefmod.Xhtml

let login_table = Eliomsessions.create_volatile_table ()

let connect_action = 
  Eliomservices.new_post_coservice'
    ~post_params:(Eliomparameters.string "login")
    ()
    
(* As the handler is very simple, we register it now: *)
let disconnect_action = 
  Eliompredefmod.Actions.register_new_post_coservice'
    ~post_params:Eliomparameters.unit 
    (fun sp () () -> 
      Eliomsessions.close_session  ~sp () >>= fun () -> 
      Lwt.return [])


let disconnect_box sp s = 
  Eliompredefmod.Xhtml.post_form disconnect_action sp 
    (fun _ -> [p [Eliompredefmod.Xhtml.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let login_box sp = 
  Eliompredefmod.Xhtml.post_form connect_action sp
    (fun loginname ->
      [p 
         (let login = 
            [pcdata "Enter your login name: "; 
             Eliompredefmod.Xhtml.string_input
               ~input_type:`Text ~name:loginname ()]
          in login)
     ])
    ()

(* Get logged in user as an option *)
let get_login_user sp =
  Lwt.return (Eliomsessions.get_volatile_session_data login_table sp ()) >>=
    fun session_data ->
      match session_data with
        Eliomsessions.Data user -> Lwt.return (Some user)
      | Eliomsessions.No_data 
      | Eliomsessions.Data_session_expired -> Lwt.return None

(** Wrap page service calls inside with_user_login to have them
    automatically check for user login and redirect to login screen if
    not logged in. *)
let with_user_login sp f =
  get_login_user sp >>= fun maybe_user ->
    match maybe_user with
      Some username ->
        f username sp
    | None ->
        Html_util.html_stub sp [login_box sp]

let connect_action_handler sp () login =
  Eliomsessions.close_session  ~sp () >>= fun () -> 
  Eliomsessions.set_volatile_session_data ~table:login_table ~sp login;
  return []

let () =
  Eliompredefmod.Actions.register ~service:connect_action connect_action_handler

