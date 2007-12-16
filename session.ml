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

open Types

let upgrade_page = new_service ["upgrade"] unit ()

(* /upgrade upgrades the database schema (if needed) *)
let _ =
  register upgrade_page
    (fun sp () () ->
       let msg = Database.upgrade_schema () in
       Html_util.html_stub sp
         [h1 [pcdata "Upgrade DB schema"];
          (pre [pcdata msg])])


let login_table = Eliomsessions.create_volatile_table ()

let connect_action = 
  Eliomservices.new_post_coservice'
    ~post_params:((string "login") ** (string "passwd"))
    ()
    
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
     [pcdata "Your database appears to be older than your current Nurpawiki server.";
      br ();
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

let db_installation_error  sp = 
  [h1 [pcdata "Database Not Properly Installed!"];
   p
     [pcdata "Your database does not seem to be installed as per installation instructions.  Please see database installation instructions in the documentation."]]

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


(** Wrap page service calls inside with_user_login to have them
    automatically check for user login and redirect to login screen if
    not logged in. *)
let with_user_login sp f =
  (* Check if the DB is installed.  If so, check that it doesn't need
     an upgrade. *)
  if Database.is_schema_installed then
    Html_util.html_stub sp (db_installation_error sp)
  else if Database.db_schema_version () < Database.nurpawiki_schema_version then
    Html_util.html_stub sp (db_upgrade_warning sp)
  else
    get_login_user sp >>= fun maybe_user ->
      match maybe_user with
        Some (login,passwd) ->
          begin
            match (Database.query_user login) with
              Some user ->
                let passwd_md5 = Digest.to_hex (Digest.string passwd) in
                (* Autheticate user against his password *)
                if passwd_md5 <> user.user_passwd then
                  login_html sp
                    [Html_util.error ("Wrong password given for user '"^login^"'")]
                else 
                  f user sp
            | None ->
                login_html sp [] 
          end
      | None ->
          login_html sp []


(* Same as with_user_login except that we can't generate HTML for any
   errors here.  Neither can we present the user with a login box.  If
   there are any errors, just bail out without doing anything
   harmful. *)
let action_with_user_login sp f =
  if Database.db_schema_version () = Database.nurpawiki_schema_version then
    get_login_user sp >>= fun maybe_user ->
      (match maybe_user with
         Some (login,passwd) ->
           begin
             match (Database.query_user login) with
               Some user ->
                 let passwd_md5 = Digest.to_hex (Digest.string passwd) in
                 (* Autheticate user against his password *)
                 if passwd_md5 = user.user_passwd then
                   f user
                 else 
                   ()
             | None ->
                 ()
           end
       | None -> ());
      return []
  else
    return []


let update_session_password sp login new_password =
  ignore
    (Eliomsessions.close_session  ~sp () >>= fun () -> 
       Eliomsessions.set_volatile_session_data ~table:login_table ~sp (login,new_password);
       return [])
  
let connect_action_handler sp () login_nfo =
  Eliomsessions.close_session  ~sp () >>= fun () -> 
    Eliomsessions.set_volatile_session_data ~table:login_table ~sp login_nfo;
    return []

let () =
  Eliompredefmod.Actions.register ~service:connect_action connect_action_handler

