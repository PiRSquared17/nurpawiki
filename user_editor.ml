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
               td [pcdata user.user_email];
               td [a ~service:edit_user_page ~sp [pcdata "Edit"] user.user_login]])
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

let save_user ~update_user ~login ~passwd ~passwd2 ~real_name ~email =
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
                    if passwd = "" then None else Some passwd_md5 in
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
            save_user ~update_user:false ~login ~passwd ~passwd2 ~real_name ~email in
          view_user_admin_page sp ~err ~credentials))


let save_user_prefs c_passwd c_passwd2 (c_name,old_name) (c_email,old_email) =
  (table
     (tr
        (td [pcdata "Password:"])
        [td [string_input ~input_type:`Password ~name:c_passwd ()];
         ])
     [tr
        (td [pcdata "Re-type password:"])
        [td [string_input ~input_type:`Password ~name:c_passwd2 ()]];

      tr 
        (td [pcdata "Name:"])
        [td [string_input ~input_type:`Text ~name:c_name 
               ~value:old_name ()]];

      tr 
        (td [pcdata "E-mail address:"])
        [td [string_input ~input_type:`Text ~name:c_email 
               ~value:old_email ()]];

      tr
        (td [string_input ~input_type:`Submit ~value:"Save User" ()])
        []])

let _ =
  register user_admin_page
    (fun sp _ () -> 
       Session.with_user_login sp
         (fun credentials sp ->
            view_user_admin_page sp ~err:[] ~credentials))


let rec view_edit_user_page sp ~err ~cur_user user_to_edit =
  Html_util.html_stub sp
    (Html_util.navbar_html sp ~credentials:cur_user
       ([h1 [pcdata "Edit User"]] @
          err @
          [post_form ~service:service_save_user_edit ~sp
             (fun (passwd,(passwd2,(name,email))) ->
                [h2 [pcdata ("Edit User '"^user_to_edit.user_login^"'")];
                 save_user_prefs passwd passwd2 
                   (name,user_to_edit.user_real_name) 
                   (email,user_to_edit.user_email)]) user_to_edit.user_login]))


let error_page sp msg =
  Html_util.html_stub sp 
    [p [Html_util.error msg]]

let _ =
  register service_save_user_edit
    (fun sp login (passwd,(passwd2,(real_name, email)))  ->
       Session.with_user_login sp
         (fun cur_user sp ->
            (* TODO match user privileges here! *)
            let err = 
              save_user 
                ~update_user:true 
                ~login:login
                ~passwd ~passwd2 ~real_name ~email in
            (* Update password in the session if we're editing current
               user: *)
            if passwd <> "" && cur_user.user_login = login then
              Session.update_session_password sp login passwd;
            match Database.query_user login with
              Some user ->
                Session.with_user_login sp
                  (fun cur_user sp ->
                     view_edit_user_page sp ~err ~cur_user user)
            | None ->
                error_page sp ("Trying to edit unknown user '"^login^"'")))

let _ =
  register edit_user_page
    (fun sp editing_login () -> 
       Session.with_user_login sp
         (fun cur_user sp ->
            match Database.query_user editing_login with
              Some login ->
                (* TODO need to match user privileges here! *)
                view_edit_user_page sp ~err:[] ~cur_user login
            | None ->
                error_page sp ("Unknown user '"^editing_login^"'")))
