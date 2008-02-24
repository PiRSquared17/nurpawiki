(* Copyright (c) 2006, 2007 Janne Hellsten <jjhellst@gmail.com> *)

(* 
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 2 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.  You should have received
 * a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>. 
 *)

open Lwt
open XHTML.M
open Eliomservices
open Eliomparameters
open Eliomsessions
open Eliompredefmod.Xhtml

open Services
open Types

open Config

module Db = Database
module Dbu = Database_upgrade

let seconds_in_day = 60.0 *. 60.0 *. 24.0

let login_table = Eliomsessions.create_volatile_table ()

(* Set password & login into session.  We set the cookie expiration
   into 24h from now so that the user can even close his browser
   window, re-open it and still retain his logged in status. *)
let set_password_in_session sp login_info =
  ignore (set_service_session_timeout ~sp None);
  ignore (set_volatile_session_timeout ~sp None);
  ignore (set_service_session_timeout ~sp None);

  set_volatile_data_session_cookie_exp_date ~sp 
    (Some 3153600000.0);
  set_volatile_session_data ~table:login_table ~sp login_info


let upgrade_page = new_service ["upgrade"] unit ()

let connect_action = 
  Eliomservices.new_post_coservice'
    ~post_params:((string "login") ** (string "passwd"))
    ()
    

let link_to_nurpawiki_main sp = 
  a ~sp ~service:wiki_view_page 
    [pcdata "Take me to Nurpawiki"] 
    (Config.site.cfg_homepage,(None,(None,None)))

(* Get logged in user as an option *)
let get_login_user sp =
  Lwt.return (Eliomsessions.get_volatile_session_data login_table sp ()) >>=
    fun session_data ->
      match session_data with
        Eliomsessions.Data user -> Lwt.return (Some user)
      | Eliomsessions.No_data 
      | Eliomsessions.Data_session_expired -> Lwt.return None

let db_upgrade_warning sp = 
  [h1 [pcdata "Database Upgrade Warning!"];
   p
     [pcdata "An error occured when Nurpawiki was trying to access database.";
      br ();
      strong [
        pcdata "You might be seeing this for a couple of reasons:";
        br ()];
      br ();
      pcdata "1) You just installed Nurpawiki and this is the first time you're running Nurpawiki on your database!"; br ();
      pcdata "2) You have upgraded an existing Nurpawiki installation and this is the first time you're running it since upgrade."; br ();
      br ();
      pcdata "In order to continue, your DB needs to be upgraded. ";
      pcdata "If you have valuable data in your DB, please take a backup of it before proceeding!";
      br ();
      br ();
      a ~service:upgrade_page ~sp [pcdata "Upgrade now!"] ()]]

let db_installation_error sp = 
  [div
     [h1 [pcdata "Database not configured or database connection down!"];
      br ();
      p [pcdata "When trying to access the Nurpawiki database an error occurred.";
         br (); br ();
         pcdata "The two most probable reasons for this are:"];
      ul (li [pcdata "The Nurpawiki schema has not been installed on the database or the database settings are misconfigured.  Please see Nurpawiki documentation on database installation."])
        [li [pcdata "The database connection was somehow disconnected.  This may happen due to network failures, SQL server restarts, etc."]];
      p [pcdata "Try going back to the wiki: "; link_to_nurpawiki_main sp;
         br (); br ();
         pcdata "If the problem persists, talk to the site administrator."]]]
     

let login_html sp ~err =
  Html_util.html_stub sp 
    [div ~a:[a_id "login_outer"]
       [div ~a:[a_id "login_align_middle"]
          [Eliompredefmod.Xhtml.post_form connect_action sp
             (fun (loginname,passwd) ->
                [table ~a:[a_class ["login_box"]]
                   (tr (td ~a:[a_class ["login_text"]] 
                          [pcdata "Welcome to Nurpawiki!"]) [])
                   [tr (td [pcdata ""]) [];
                    tr (td ~a:[a_class ["login_text_descr"]] 
                          [pcdata "Username:"]) [];
                    tr (td [string_input ~input_type:`Text ~name:loginname ()]) [];
                    tr (td ~a:[a_class ["login_text_descr"]] 
                          [pcdata "Password:"]) [];
                    tr (td [string_input ~input_type:`Password ~name:passwd ()]) [];
                    tr (td [string_input ~input_type:`Submit ~value:"Login" ()]) [];
                   ];
                 p err]) ()]]]


let with_db_installed sp f =
  (* Check if the DB is installed.  If so, check that it doesn't need
     an upgrade. *)
  let r = 
    Db.with_conn
      (fun conn ->
         if Dbu.is_schema_installed ~conn then
           Some (Html_util.html_stub sp (db_installation_error sp))
         else if Dbu.db_schema_version ~conn < Db.nurpawiki_schema_version then
           Some (Html_util.html_stub sp (db_upgrade_warning sp))
         else None) in
  match r with
    Some x -> x
  | None -> f ()
      
(** Wrap page service calls inside with_user_login to have them
    automatically check for user login and redirect to login screen if
    not logged in. *)
let with_user_login ?(allow_read_only=false) sp f =
  let login () =
    get_login_user sp >>= fun maybe_user ->
      match maybe_user with
        Some (login,passwd) ->
          begin
            let u = 
              Db.with_conn
                (fun conn -> Db.query_user ~conn login) in
            match u with
              Some user ->
                let passwd_md5 = Digest.to_hex (Digest.string passwd) in
                (* Autheticate user against his password *)
                if passwd_md5 <> user.user_passwd then
                  login_html sp
                    [Html_util.error ("Wrong password given for user '"^login^"'")]
                else 
                  f user sp
            | None ->
                login_html sp 
                  [Html_util.error ("Unknown user '"^login^"'")]
          end
      | None ->
          if allow_read_only && Config.site.cfg_allow_ro_guests then
            let guest_user = 
              {
                user_id = 0;
                user_login = "guest";
                user_passwd = "";
                user_real_name = "Guest";
                user_email = "";
              } in
            f guest_user sp
          else 
            login_html sp [] in
  with_db_installed sp login

(* Either pretend to be logged in as 'guest' (if allowed by config
   options) or require a proper login.
   
   If logging in as 'guest', we setup a dummy user 'guest' that is not
   a real user.  It won't have access to write to any tables. *)
let with_guest_login sp f =
 with_user_login ~allow_read_only:true sp f

(* Same as with_user_login except that we can't generate HTML for any
   errors here.  Neither can we present the user with a login box.  If
   there are any errors, just bail out without doing anything
   harmful. *)
let action_with_user_login sp f =
  let db_version = 
    Db.with_conn (fun conn -> Dbu.db_schema_version conn) in
  if db_version = Db.nurpawiki_schema_version then
    get_login_user sp >>= fun maybe_user ->
      (match maybe_user with
         Some (login,passwd) ->
           begin
             match (Db.with_conn (fun conn -> Db.query_user ~conn login)) with
               Some user ->
                 let passwd_md5 = Digest.to_hex (Digest.string passwd) in
                 (* Autheticate user against his password *)
                 if passwd_md5 = user.user_passwd then
                   return (f user)
                 else 
                   return []
             | None ->
                 return []
           end
       | None -> return [])
  else
    return []


let update_session_password sp login new_password =
  ignore
    (Eliomsessions.close_session  ~sp () >>= fun () -> 
       set_password_in_session sp (login,new_password);
       return [])
  

(* Check session to see what happened during page servicing.  If any
   actions were called, some of them might've set values into session
   that we want to use for rendering the current page. *)
let any_complete_undos sp =
  List.fold_left
    (fun acc e -> 
       match e with 
         Action_completed_task tid -> Some tid
       | _ -> acc)
    None (Eliomsessions.get_exn sp)

(* Same as any_complete_undos except we check for changed task
   priorities. *)
let any_task_priority_changes sp =
  List.fold_left
    (fun acc e -> 
       match e with 
         Action_task_priority_changed tid -> tid::acc
       | _ -> acc)
    [] (Eliomsessions.get_exn sp)

let connect_action_handler sp () login_nfo =
  Eliomsessions.close_session  ~sp () >>= fun () -> 
    set_password_in_session sp login_nfo;
    return []

let () =
  Eliompredefmod.Actions.register ~service:connect_action connect_action_handler

(* /upgrade upgrades the database schema (if needed) *)
let _ =
  register upgrade_page
    (fun sp () () ->
       let msg = Db.with_conn (fun conn -> Dbu.upgrade_schema ~conn) in
       Html_util.html_stub sp
         [h1 [pcdata "Upgrade DB schema"];
          (pre [pcdata msg]);
          p [br ();
             link_to_nurpawiki_main sp]])

let _ =
  register disconnect_page
    (fun sp () () ->
       (Eliomsessions.close_session  ~sp () >>= fun () ->
          Html_util.html_stub sp 
            [h1 [pcdata "Logged out!"];
             p [br ();
                link_to_nurpawiki_main sp]]))
