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

let service_save_user = 
  new_post_service
    ~fallback:user_editor_page
    ~post_params:((string "login") ** 
                    (string "passwd") ** 
                    (string "passwd2") **  (* re-type *)
                    (string "name") **
                    (string "email"))
    ()


let rec view_user_editor_page sp ~err ~credentials =
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
         [post_form ~service:service_save_user ~sp
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

let _ =
  register service_save_user
    (fun sp () (login,(passwd,(passwd2,(real_name, email))))  ->
     Session.with_user_login sp
       (fun credentials sp ->
          let sanitized_login = sanitize_login_name login in
          let err = 
            match sanitized_login with
              None -> 
                [Html_util.error ("Only alphanumeric chars are allowed in login name!  Got '"^login^"'")]
            | Some login ->
                let old_user = 
                  Database.query_user login in
                if old_user <> None then
                  [Html_util.error ("User '"^login^"' already exists!")]
                else if passwd <> passwd2 then
                  [Html_util.error "Re-typed password doesn't match your password!"]
                else 
                  begin
                    let passwd = Digest.to_hex (Digest.string passwd) in
                    Database.add_user ~login ~passwd ~real_name ~email;
                    []
                  end in
          view_user_editor_page sp ~err ~credentials))


let _ =
  register user_editor_page
    (fun sp _ () -> 
       Session.with_user_login sp
         (fun credentials sp ->
            view_user_editor_page sp ~err:[] ~credentials))
