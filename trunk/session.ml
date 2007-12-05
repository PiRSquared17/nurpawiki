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
    ~post_params:(Eliomparameters.string "login")
    ()
    
let login_box sp = 
  Eliompredefmod.Xhtml.post_form connect_action sp
    (fun loginname ->
      [p 
         (let login = 
            [pcdata "Enter your login name (must be admin or nobody for now): "; 
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

let db_upgrade_warning sp = 
  [h1 [pcdata "Database Upgrade Warning!"];
   p
     [pcdata "Your database appears to be older than your current Nurpawiki server.";
      br ();
      pcdata "In order to continue, you must upgrade your DB. ";
      pcdata "Before doing so, please take a backup of your database!";
      br ();
      br ();
      a ~service:upgrade_page ~sp [pcdata "Upgrade now!"] ()]]

(** Wrap page service calls inside with_user_login to have them
    automatically check for user login and redirect to login screen if
    not logged in. *)
let with_user_login sp f =
  (* First check for the need of DB upgrade: *)
  if Database.db_schema_version () < Database.nurpawiki_schema_version then
    begin
      (* TODO could take the user directly to main page here *)
      Html_util.html_stub sp (db_upgrade_warning sp)
    end
  else
    get_login_user sp >>= fun maybe_user ->
      match maybe_user with
        Some username ->
          begin
            match (Database.find_user_id username) with
              Some id ->
                f { user_id = id; user_login = username; } sp
            | None ->
                Html_util.html_stub sp [login_box sp]
          end
      | None ->
          Html_util.html_stub sp [login_box sp]

let connect_action_handler sp () login =
  Eliomsessions.close_session  ~sp () >>= fun () -> 
  Eliomsessions.set_volatile_session_data ~table:login_table ~sp login;
  return []

let () =
  Eliompredefmod.Actions.register ~service:connect_action connect_action_handler

