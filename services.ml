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

open Types

open Lwt

(* As the handler is very simple, we register it now: *)
let disconnect_action = 
  Eliompredefmod.Actions.register_new_post_coservice'
    ~post_params:Eliomparameters.unit 
    (fun sp () () -> 
      Eliomsessions.close_session  ~sp () >>= fun () -> 
      Lwt.return [])


let wiki_view_page = 
  new_service ["view"] ((string "p")
                        ** (opt (bool "printable"))) ()

let wiki_edit_page = new_service ["edit"] (string "p") ()

let scheduler_page = new_service ["scheduler"] unit ()

let edit_todo_get_page = new_service ["edit_todo"] 
  ((Eliomparameters.user_type 
      et_cont_of_string string_of_et_cont "src_service") **
     (opt (int "tid"))) ()

let edit_todo_page = 
  new_post_service
    ~fallback:edit_todo_get_page 
    ~post_params:any ()

let history_page = new_service ["history"] unit ()

let search_page = new_service ["search"] (string "q") ()

let benchmark_page = new_service ["benchmark"] (string "test") ()

let user_admin_page = new_service ["user_admin"] unit ()

let edit_user_page = new_service ["edit_user"] (string "user_to_edit") ()
