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

open Types
module Psql = Postgresql
module P = Printf
open XHTML.M

open Eliomsessions

type db_config = 
    {
      db_name : string;
      db_user : string;
      db_port : string option;
      db_pass : string option;
    }

open Simplexmlparser

let (>>) f g = g f

let dbcfg =
  let get_attr_opt attr attrs = 
    try Some (List.assoc attr attrs)
    with Not_found -> None in

  let get_attr_with_err attr attrs =
    try (List.assoc attr attrs)
    with Not_found -> 
      raise (Extensions.Error_in_config_file 
               ("Expecting database."^attr^" attribute in Nurpawiki config")) in

  let rec find_dbcfg = function
      [Element ("database", attrs, _)] ->
        let dbname = get_attr_with_err "name" attrs in
        let dbuser = get_attr_with_err "user" attrs in
        let dbport = get_attr_opt "port" attrs in
        let dbpass = get_attr_opt "password" attrs in
        (dbname,dbuser,dbport,dbpass)
    | _ -> 
        raise (Extensions.Error_in_config_file ("Unexpected content inside Nurpawiki config")) in
  let (dbname,dbuser,dbport,dbpass) = find_dbcfg (get_config ()) in
  { 
    db_name = dbname;
    db_user = dbuser;
    db_port = dbport;
    db_pass = dbpass;
  }

let db_conn =
  try
    Messages.errlog (P.sprintf "connecting to DB '%s' as user '%s'" 
                       dbcfg.db_name dbcfg.db_user);
    new Psql.connection ~host:"localhost"
      ~dbname:dbcfg.db_name ~user:dbcfg.db_user 
      ~port:(Option.default "" dbcfg.db_port)
      ~password:(Option.default "" dbcfg.db_pass)
      ()
  with
    (Psql.Error e) as ex ->
      (match e with
         Psql.Connection_failure msg -> 
           P.eprintf "psql failed : %s\n" msg;
           raise ex
       | _ -> 
           P.eprintf "psql failed : %s\n" (Psql.string_of_error e);
           raise ex)
  | _ -> assert false

(* Escape a string for SQL query *)
let escape s =
  let b = Buffer.create (String.length s) in
  String.iter 
    (function
         '\\' -> Buffer.add_string b "\\\\"
       | '\'' -> Buffer.add_string b "''"
       | '"' -> Buffer.add_string b "\""
       | c -> Buffer.add_char b c) s;
  Buffer.contents b
    
let todos_user_login_join = "FROM todos LEFT OUTER JOIN users ON todos.user_id = users.id"

(* Use this tuple format when querying TODOs to be parsed by
   parse_todo_result *)
let todo_tuple_format = "todos.id,descr,completed,priority,activation_date,user_id,users.login"

let todo_of_row row = 
  let id = int_of_string (List.nth row 0) in
  let descr = List.nth row 1 in
  let completed = (List.nth row 2) = "t" in
  let owner_id = List.nth row 5 in
  let owner = 
    if owner_id = "" then
      None
    else
      Some {
        owner_id = int_of_string owner_id;
        owner_login = List.nth row 6;
      } in
    
  let pri = List.nth row 3 in
  {
    t_id = id;
    t_descr = descr; 
    t_completed = completed;
    t_priority = int_of_string pri;
    t_activation_date =  List.nth row 4;
    t_owner = owner;
  }
    
let parse_todo_result res = 
  List.fold_left 
    (fun acc row ->
       let id = int_of_string (List.nth row 0) in
       IMap.add id (todo_of_row row) acc)
    IMap.empty res#get_all_lst

let guarded_exec query =
  try
    db_conn#exec query
  with
    (Psql.Error e) as ex ->
      (match e with
         Psql.Connection_failure msg -> 
           P.eprintf "psql failed : %s\n" msg;
           raise ex
       | _ -> 
           P.eprintf "psql failed : %s\n" (Psql.string_of_error e);
           raise ex)

let insert_todo_activity ~user_id todo_id ?(page_ids=None) activity =
  let user_id_s = string_of_int user_id in
  match page_ids with
    None ->
      "INSERT INTO activity_log(activity_id,user_id,todo_id) VALUES ("^
        (string_of_int (int_of_activity_type activity))^", "^user_id_s^
        ", "^todo_id^")"
  | Some pages ->
      let insert_pages = 
        List.map
          (fun page_id -> 
             "INSERT INTO activity_in_pages(activity_log_id,page_id) "^
               "VALUES (CURRVAL('activity_log_id_seq'), "^string_of_int page_id^")")
          pages in
      let page_act_insert = String.concat "; " insert_pages in
      "INSERT INTO activity_log(activity_id,user_id,todo_id) VALUES ("^
        (string_of_int (int_of_activity_type activity))^", "^
        user_id_s^", "^todo_id^"); "^
        page_act_insert

let insert_save_page_activity ~user_id (page_id : int) =
  let sql = "BEGIN;
INSERT INTO activity_log(activity_id, user_id) 
       VALUES ("^(string_of_int (int_of_activity_type AT_edit_page))^
    " ,"^(string_of_int user_id)^");
INSERT INTO activity_in_pages(activity_log_id,page_id) 
       VALUES (CURRVAL('activity_log_id_seq'), "^string_of_int page_id^");
COMMIT" in
  ignore (guarded_exec sql)

let query_todos_by_ids todo_ids = 
  if todo_ids <> [] then
    let ids = String.concat "," (List.map string_of_int todo_ids) in
    let r = guarded_exec ("SELECT "^todo_tuple_format^" "^todos_user_login_join^" WHERE todos.id IN ("^ids^")") in
    List.map todo_of_row (r#get_all_lst)
  else
    []

let query_todo id = 
  match query_todos_by_ids [id] with
    [task] -> Some task
  | [] -> None
  | _ -> None

let update_todo_activation_date todo_id new_date =
  let sql = 
    "UPDATE todos SET activation_date = '"^new_date^"' WHERE id = "^
      (string_of_int todo_id) in
  ignore (guarded_exec sql)


let update_todo_descr todo_id new_descr =
  let sql = 
    "UPDATE todos SET descr = '"^escape new_descr^"' WHERE id = "^
      (string_of_int todo_id) in
  ignore (guarded_exec sql)


let update_todo_owner_id todo_id owner_id =
  let owner_id_s = 
    match owner_id with
      Some id -> string_of_int id 
    | None -> "NULL" in
  let sql = 
    "UPDATE todos SET user_id = "^owner_id_s^" WHERE id = "^
      (string_of_int todo_id) in
  ignore (guarded_exec sql)


let select_current_user id = 
  (match id with
     None -> ""
   | Some user_id -> 
       " AND (user_id = "^string_of_int user_id^" OR user_id IS NULL) ")

(* Query TODOs and sort by priority & completeness *)
let query_all_active_todos ~current_user_id () =
  let r = guarded_exec
    ("SELECT "^todo_tuple_format^" "^todos_user_login_join^" "^
       "WHERE activation_date <= current_date AND completed = 'f' "^
       select_current_user current_user_id^
       "ORDER BY completed,priority,id") in
  List.map todo_of_row r#get_all_lst

let query_upcoming_todos ~current_user_id date_criterion =
  let date_comparison =
    let dayify d = 
      "'"^string_of_int d^" days'" in
    match date_criterion with
      (None,Some days) -> 
        "(activation_date > now()) AND (now()+interval "^dayify days^
          " >= activation_date)"
    | (Some d1,Some d2) ->
        let sd1 = dayify d1 in
        let sd2 = dayify d2 in
        "(activation_date > now()+interval "^sd1^") AND (now()+interval "^sd2^
          " >= activation_date)"
    | (Some d1,None) ->
        let sd1 = dayify d1 in
        "(activation_date > now()+interval "^sd1^")"
    | (None,None) -> 
        "activation_date <= now()" in
  let r = guarded_exec
    ("SELECT "^todo_tuple_format^" "^todos_user_login_join^" "^
       "WHERE "^date_comparison^
       select_current_user current_user_id^
       " AND completed='f' ORDER BY activation_date,priority,id") in
  List.map todo_of_row r#get_all_lst
    
let new_todo page_id user_id descr =
  (* TODO: could wrap this into BEGIN .. COMMIT if I knew how to
     return the data from the query! *)
  let sql = 
    "INSERT INTO todos(user_id,descr) values('"^(string_of_int user_id)^"','"^escape descr^"'); 
 INSERT INTO todos_in_pages(todo_id,page_id) values(CURRVAL('todos_id_seq'), "
    ^string_of_int page_id^");"^
      (insert_todo_activity ~user_id
         "(SELECT CURRVAL('todos_id_seq'))" ~page_ids:(Some [page_id]) 
         AT_create_todo)^";
 SELECT CURRVAL('todos_id_seq')" in
  let r = guarded_exec sql in
  (* Get ID of the inserted item: *)
  (r#get_tuple 0).(0)

(* Mapping from a todo_id to page list *)
let todos_in_pages todo_ids =
  (* Don't query if the list is empty: *)
  if todo_ids = [] then
    IMap.empty
  else 
    let ids = String.concat "," (List.map string_of_int todo_ids) in
    let sql = 
      "SELECT todo_id,page_id,page_descr "^
        "FROM todos_in_pages,pages WHERE todo_id IN ("^ids^") AND page_id = pages.id" in
    let r = guarded_exec sql in
    let rows = r#get_all_lst in
    List.fold_left
      (fun acc row ->
         let todo_id = int_of_string (List.nth row 0) in
         let page_id = int_of_string (List.nth row 1) in
         let page_descr = List.nth row 2 in
         let lst = try IMap.find todo_id acc with Not_found -> [] in
         IMap.add todo_id ({ p_id = page_id; p_descr = page_descr }::lst) acc)
      IMap.empty rows

(* TODO must not query ALL activities.  Later we only want to
   currently visible activities => pages available. *)
let query_activity_in_pages () =
  let sql = "SELECT activity_log_id,page_id,page_descr FROM activity_in_pages,pages WHERE page_id = pages.id" in
  let r = guarded_exec sql in
  List.fold_left
    (fun acc row ->
       let act_id = int_of_string (List.nth row 0) in
       let page_id = int_of_string (List.nth row 1) in
       let page_descr = List.nth row 2 in
       let lst = try IMap.find act_id acc with Not_found -> [] in
       IMap.add act_id ({ p_id = page_id; p_descr = page_descr }::lst) acc) 
    IMap.empty (r#get_all_lst)
    
(* Collect todos in the current page *)
let query_page_todos page_id =
  let sql = "SELECT "^todo_tuple_format^" "^todos_user_login_join^" WHERE todos.id in "^
    "(SELECT todo_id FROM todos_in_pages WHERE page_id = "^string_of_int page_id^")" in
  let r = guarded_exec sql in
  parse_todo_result r

(* Make sure todos are assigned to correct pages and that pages
   don't contain old todos moved to other pages or removed. *)
let update_page_todos page_id todos =
  let page_id' = string_of_int page_id in
  let sql = 
    "BEGIN;
 DELETE FROM todos_in_pages WHERE page_id = "^page_id'^";"^
      (String.concat "" 
         (List.map 
            (fun todo_id ->
               "INSERT INTO todos_in_pages(todo_id,page_id)"^
                 " values("^(string_of_int todo_id)^", "^page_id'^");")
            todos)) ^
      "COMMIT" in
  ignore (guarded_exec sql)                        

(* Mark task as complete and set completion date for today *)
let complete_task_generic ~user_id id op =
  let (activity,task_complete_flag) =
    match op with
      `Complete_task -> (AT_complete_todo, "t")
    | `Resurrect_task -> (AT_uncomplete_todo, "f") in
  let page_ids =
    try 
      Some (List.map (fun p -> p.p_id) (IMap.find id (todos_in_pages [id])))
    with Not_found -> None in
  let ids = string_of_int id in
  let sql = "BEGIN;
UPDATE todos SET completed = '"^task_complete_flag^"' where id="^ids^";"^
    (insert_todo_activity ~user_id ids ~page_ids activity)^"; COMMIT" in
  ignore (guarded_exec sql)

(* Mark task as complete and set completion date for today *)
let complete_task ~user_id id =
  complete_task_generic ~user_id id `Complete_task

let uncomplete_task ~user_id id =
  complete_task_generic ~user_id id `Resurrect_task

let task_priority id = 
  let sql = "SELECT priority FROM todos WHERE id = "^string_of_int id in
  let r = guarded_exec sql in
  int_of_string (r#get_tuple 0).(0)

(* TODO offset_task_priority can probably be written in one
   query instead of two (i.e., first one SELECT and then UPDATE
   based on that. *)
let offset_task_priority id incr =
  let pri = min (max (task_priority id + incr) 1) 3 in
  let sql = 
    "UPDATE todos SET priority = '"^(string_of_int pri)^
      "' where id="^string_of_int id in
  ignore (guarded_exec sql)

let up_task_priority id =
  offset_task_priority id (-1)

let down_task_priority id =
  offset_task_priority id 1

let new_wiki_page page =
  let sql = 
    "INSERT INTO pages (page_descr) VALUES ('"^escape page^"');"^
      "INSERT INTO wikitext (page_id,page_text) "^
      "       VALUES ((SELECT CURRVAL('pages_id_seq')), ''); "^
      "SELECT CURRVAL('pages_id_seq')" in
  let r = guarded_exec sql in
  int_of_string ((r#get_tuple 0).(0))

let find_page_id descr =
  let sql = 
    "SELECT id FROM pages WHERE page_descr = '"^escape descr^"' LIMIT 1" in
  let r = guarded_exec sql in
  if r#ntuples = 0 then None else Some (int_of_string (r#get_tuple 0).(0))

let page_id_of_page_name descr =
  Option.get (find_page_id descr)

let wiki_page_exists page_descr =
  find_page_id page_descr <> None

let load_wiki_page page_id = 
  let sql = "SELECT page_text FROM wikitext WHERE page_id="^string_of_int page_id^" LIMIT 1" in
  let r = guarded_exec sql in
  (r#get_tuple 0).(0)

let save_wiki_page page_id lines =
  let escaped = escape (String.concat "\n" lines) in
  (* E in query is escape string constant *)
  let sql =
    "UPDATE wikitext SET page_text = E'"^escaped^"' WHERE page_id = "
    ^string_of_int page_id in
  ignore (guarded_exec sql)

let query_past_activity () =
  let sql =
    "SELECT activity_log.id,activity_id,activity_timestamp,todos.descr,users.login "^
      "FROM activity_log
       LEFT OUTER JOIN todos ON activity_log.todo_id = todos.id
       LEFT OUTER JOIN users ON activity_log.user_id = users.id
       AND activity_log.activity_timestamp < now()
       ORDER BY activity_timestamp DESC" in
  let r = guarded_exec sql in
  r#get_all_lst >>
    List.map
    (fun row ->
       let id = int_of_string (List.nth row 0) in
       let act_id = List.nth row 1 in
       let time = List.nth row 2 in
       let descr = List.nth row 3 in
       let user = List.nth row 4 in
       { a_id = id;
         a_activity = activity_type_of_int (int_of_string act_id);
         a_date = time;
         a_todo_descr = if descr = "" then None else Some descr;
         a_changed_by = if user = "" then None else Some user
       })

(* Search features *)
let search_wikipage str =
  let escaped_ss = escape str in
  let sql = 
    "SELECT page_id,headline,page_descr FROM findwikipage('"^escaped_ss^"') "^
      "LEFT OUTER JOIN pages on page_id = pages.id ORDER BY rank DESC" in
  let r = guarded_exec sql in
  r#get_all_lst >>
    List.map
    (fun row ->
       let id = int_of_string (List.nth row 0) in
       let hl = List.nth row 1 in
       { sr_id = id; 
         sr_headline = hl; 
         sr_page_descr = Some (List.nth row 2);
         sr_result_type = SR_page })


let user_query_string = 
  "SELECT id,login,passwd,real_name,email FROM users"

let user_of_sql_row row =
  let id = int_of_string (List.nth row 0) in
  { 
    user_id = id;
    user_login = (List.nth row 1);
    user_passwd = (List.nth row 2); 
    user_real_name = (List.nth row 3); 
    user_email = (List.nth row 4); 
  }

let query_users () =
  let sql = user_query_string ^ " ORDER BY id" in
  let r = guarded_exec sql in
  r#get_all_lst >> List.map user_of_sql_row


let query_user username =
  let sql = 
    user_query_string ^" WHERE login = '"^escape username^"' LIMIT 1" in
  let r = guarded_exec sql in
  if r#ntuples = 0 then 
    None 
  else
    Some (user_of_sql_row (r#get_tuple_lst 0))

let add_user ~login ~passwd ~real_name ~email =
  let sql =
    "INSERT INTO users (login,passwd,real_name,email) "^
      "VALUES ("^(String.concat "," 
                    (List.map (fun s -> "'"^escape s^"'")
                       [login; passwd; real_name; email]))^")" in
  ignore (guarded_exec sql)

let update_user ~user_id ~passwd ~real_name ~email =
  let sql =
    "UPDATE users SET "^
      (match passwd with
         None -> ""
       | Some passwd -> "passwd = '"^escape passwd^"',")^
      "real_name = '"^escape real_name^"',
          email = '"^escape email^"' 
       WHERE id = "^(string_of_int user_id) in
  ignore (guarded_exec sql)


let upgrade_schema_from_0 logmsg =
  Buffer.add_string logmsg "Upgrading from schema version 0\n";
  (* Create version table and set version to 1: *)
  let sql = 
    "CREATE TABLE version (schema_version integer NOT NULL);
     INSERT INTO version (schema_version) VALUES('1')" in
  ignore (guarded_exec sql);

  let empty_passwd = (Digest.to_hex (Digest.string "")) in
  let sql = 
    "CREATE TABLE users (id SERIAL, 
                         login text NOT NULL,
                         passwd varchar(64) NOT NULL,
                         real_name text,
                         email varchar(64));
     INSERT INTO users (login,passwd) VALUES('admin', '"^empty_passwd^"')" in
  Buffer.add_string logmsg "  Create users table\n";
  ignore (guarded_exec sql);

  (* Migrate all tables to version 1 from schema v0: *)

  (* Todos are now owned by user_id=0 *)
  let sql =
    "ALTER TABLE todos ADD COLUMN user_id integer" in
  Buffer.add_string logmsg "  Add user_id column to todos table\n";
  ignore (guarded_exec sql);

  (* Add user_id field to activity log table *)
  let sql =
    "ALTER TABLE activity_log ADD COLUMN user_id integer" in
  Buffer.add_string logmsg "  Add user_id column to activity_log table\n";
  ignore (guarded_exec sql);
  ()

(* Highest upgrade schema below must match this version *)
let nurpawiki_schema_version = 1

let upgrade_schema () =
  (* First find out schema version.. *)
  let logmsg = Buffer.create 0 in
  let sql = 
    "SELECT * from pg_tables WHERE schemaname = 'public' AND "^
      "tablename = 'version'" in
  let r = guarded_exec sql in
  if r#ntuples = 0 then
    begin
      Buffer.add_string logmsg "Schema is at version 0 (no version found)\n";
      upgrade_schema_from_0 logmsg
    end;
  Buffer.contents logmsg

let db_schema_version () =
  let sql = 
    "SELECT * from pg_tables WHERE schemaname = 'public' AND "^
      "tablename = 'version'" in
  let r = guarded_exec sql in
  if r#ntuples = 0 then
    0
  else 
    let r = guarded_exec "SELECT (version.schema_version) FROM version" in
    int_of_string (r#get_tuple 0).(0)


(** Check whether the nurpawiki schema is properly installed on Psql *)
let is_schema_installed =
  let sql = 
    "SELECT * from pg_tables WHERE schemaname = 'public' AND "^
      "tablename = 'todos'" in
  let r = guarded_exec sql in
  r#ntuples = 0

