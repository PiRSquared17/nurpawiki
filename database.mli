(* Copyright (c) 2006-2008 Janne Hellsten <jjhellst@gmail.com> *)

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

type connection

val with_conn : (connection -> 'a) -> 'a
val guarded_exec : conn:connection -> string -> Postgresql.result
val insert_save_page_activity :
  conn:connection -> user_id:int -> int -> unit
val query_todos_by_ids : conn:connection -> int list -> Types.todo list
val query_todo : conn:connection -> int -> Types.todo option
val todo_exists : conn:connection -> int -> bool
val update_todo_activation_date :
  conn:connection -> int -> string -> unit
val update_todo_descr : conn:connection -> int -> string -> unit
val update_todo_owner_id : conn:connection -> int -> int option -> unit
val query_all_active_todos :
  conn:connection ->
  current_user_id:int option -> unit -> Types.todo list
val query_upcoming_todos :
  conn:connection ->
  current_user_id:int option -> int option * int option -> Types.todo list
val new_todo : conn:connection -> int -> int -> string -> string
val todos_in_pages :
  conn:connection -> int list -> Types.page list Types.IMap.t
val query_activity_in_pages :
  conn:connection ->
  min_id:int -> max_id:int -> Types.page list Types.IMap.t
val query_highest_activity_id : conn:connection -> int
val query_page_todos : conn:connection -> int -> Types.todo Types.IMap.t
val update_page_todos : conn:connection -> int -> int list -> unit
val complete_task :
  conn:connection -> user_id:int -> Types.IMap.key -> unit
val uncomplete_task :
  conn:connection -> user_id:int -> Types.IMap.key -> unit
val up_task_priority : int -> conn:connection -> unit
val down_task_priority : int -> conn:connection -> unit
val new_wiki_page : conn:connection -> user_id:int -> string -> int
val save_wiki_page :
  conn:connection -> int -> user_id:int -> string list -> unit
val find_page_id : conn:connection -> string -> int option
val page_id_of_page_name : conn:connection -> string -> int
val wiki_page_exists : conn:connection -> string -> bool
val load_wiki_page :
  conn:connection -> ?revision_id:int option -> int -> string
val query_page_revisions :
  conn:connection -> string -> Types.page_revision list
val query_past_activity :
  conn:connection -> min_id:int -> max_id:int -> Types.activity list
val search_wikipage :
  conn:connection -> string -> Types.search_result list
val query_users : conn:connection -> Types.user list
val query_user : conn:connection -> string -> Types.user option
val add_user :
  conn:connection ->
  login:string -> passwd:string -> real_name:string -> email:string -> unit
val update_user :
  conn:connection ->
  user_id:int ->
  passwd:string option -> real_name:string -> email:string -> unit
val nurpawiki_schema_version : int
