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

open XHTML.M
open Eliomsessions
open Eliomparameters
open Eliomservices
open Eliompredefmod.Xhtml

open Services
open Types

let service_create_new_user = 
  new_post_service
    ~fallback:user_admin_page
    ~post_params:((string "login") ** 
                    (string "passwd") ** 
                    (string "passwd2") **  (* re-type *)
                    (string "name") **
                    (string "email"))
    ()

let service_save_user_edit =
  new_post_service
    ~fallback:edit_user_page
    ~post_params:((string "passwd") ** 
                    (string "passwd2") **  (* re-type *)
                    (string "name") **
                    (string "email"))
    ()


let rec view_user_admin_page sp ~err ~credentials =
  let users = Database.query_users () in
  let users_table = 
    table 
      (tr 
         (th 
            [pcdata "Id"]) 
         [th [pcdata "Login"]; 
          th [pcdata "Real Name"];
          th [pcdata "E-mail"]])
      (List.map 
         (fun user ->
            tr 
              (td [pcdata (string_of_int user.user_id)])
              [td [pcdata user.user_login];
               td [pcdata user.user_real_name];
               td [pcdata user.user_email]])
         users) in

  Html_util.html_stub sp
    (Html_util.navbar_html sp ~credentials
       ([h1 [pcdata "Edit users"];
         users_table] @
          err @
         [post_form ~service:service_create_new_user ~sp
            (fun (login,(passwd,(passwd2,(name,email)))) ->
               [h2 [pcdata "Create a new user"];
                (table
                   (tr
                      (td [pcdata "Login:"])
                      [td [string_input ~input_type:`Text ~name:login ()]])
                   [tr
                      (td [pcdata "Password:"])
                      [td [string_input ~input_type:`Password ~name:passwd ()]];
                    
                    tr
                      (td [pcdata "Re-type password:"])
                      [td [string_input ~input_type:`Password ~name:passwd2 ()]];

                    tr 
                      (td [pcdata "Name:"])
                      [td [string_input ~input_type:`Text ~name:name ()]];

                    tr 
                      (td [pcdata "E-mail address:"])
                      [td [string_input ~input_type:`Text ~name:email ()]];

                    tr
                      (td [string_input ~input_type:`Submit ~value:"Add User" ()])
                      []])]) ()]))

(* Only allow certain types of login names to avoid surprises *)
let sanitize_login_name name =
  let rex = Pcre.regexp "^[a-zA-Z_][a-zA-Z0-9_]*$" in
  try Some (Pcre.extract ~rex name).(0) with Not_found -> None

let save_user ~update_user ~old_passwd ~login ~passwd ~passwd2 ~real_name ~email =
  let sanitized_login = sanitize_login_name login in
  match sanitized_login with
    None -> 
      [Html_util.error ("Only alphanumeric chars are allowed in login name!  Got '"^login^"'")]
  | Some login ->
      let old_user = 
        Database.query_user login in
      if not update_user && old_user <> None then
        [Html_util.error ("User '"^login^"' already exists!")]
      else if passwd <> passwd2 then
        [Html_util.error "Re-typed password doesn't match your password!"]
      else 
        begin
          let passwd_md5 = Digest.to_hex (Digest.string passwd) in
          if update_user then
            begin
              match old_user with
                Some u ->
                  (* If no password was entered, set it to old value: *)
                  let new_passwd_md5 = 
                    if passwd = "" then old_passwd else passwd_md5 in
                  Database.update_user 
                    ~user_id:u.user_id ~passwd:new_passwd_md5 ~real_name ~email;
              | None ->
                  assert false 
            end
          else
            Database.add_user ~login ~passwd:passwd_md5 ~real_name ~email;
          []
        end

let _ =
  register service_create_new_user
    (fun sp () (login,(passwd,(passwd2,(real_name, email))))  ->
     Session.with_user_login sp
       (fun credentials sp ->
          let err = 
            save_user ~update_user:false ~old_passwd:"" ~login ~passwd ~passwd2 ~real_name ~email in
          view_user_admin_page sp ~err ~credentials))


let _ =
  register user_admin_page
    (fun sp _ () -> 
       Session.with_user_login sp
         (fun credentials sp ->
            view_user_admin_page sp ~err:[] ~credentials))


let rec view_edit_user_page sp ~err ~credentials =
  Html_util.html_stub sp
    (Html_util.navbar_html sp ~credentials
       ([h1 [pcdata "Edit User"]] @
          err @
          [post_form ~service:service_save_user_edit ~sp
             (fun (passwd,(passwd2,(name,email))) ->
                [h2 [pcdata ("Edit User '"^credentials.user_login^"'")];
                 (table
                    (tr
                       (td [pcdata "Password:"])
                       [td [string_input ~input_type:`Password ~name:passwd ()]])
                    [tr
                       (td [pcdata "Re-type password:"])
                       [td [string_input ~input_type:`Password ~name:passwd2 ()]];

                     tr 
                       (td [pcdata "Name:"])
                       [td [string_input ~input_type:`Text ~name:name 
                              ~value:credentials.user_real_name ()]];

                     tr 
                       (td [pcdata "E-mail address:"])
                       [td [string_input ~input_type:`Text ~name:email 
                              ~value:credentials.user_email ()]];

                     tr
                       (td [string_input ~input_type:`Submit ~value:"Save User" ()])
                       []])]) ()]))


let _ =
  register service_save_user_edit
    (fun sp () (passwd,(passwd2,(real_name, email)))  ->
       Session.with_user_login sp
         (fun credentials sp ->
            let err = 
              save_user 
                ~update_user:true 
                ~old_passwd:credentials.user_passwd 
                ~login:credentials.user_login
                ~passwd ~passwd2 ~real_name ~email in
            (* Update password in the session and reload user
               credentials: *)
            if passwd <> "" then
              Session.update_session_password sp credentials.user_login passwd;
            Session.with_user_login sp
              (fun credentials sp ->
                 view_edit_user_page sp ~err ~credentials)))

let _ =
  register edit_user_page
    (fun sp _ () -> 
       Session.with_user_login sp
         (fun credentials sp ->
            view_edit_user_page sp ~err:[] ~credentials))
