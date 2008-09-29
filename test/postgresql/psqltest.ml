
module Psql = Postgresql
module P = Printf

let guarded_exec ~(conn : Psql.connection) query =
  try
    conn#exec query
  with
    (Psql.Error e) as ex ->
      (match e with
         Psql.Connection_failure msg -> 
           P.eprintf "psql failed : %s\n" msg;
           raise ex
       | _ -> 
           P.eprintf "psql failed : %s\n" (Psql.string_of_error e);
           raise ex)

let _ = 
  Printf.printf "Postgresql <-> Nurpawiki DB installation test.\n";
  let db_user = "postgres" in
  let db_name = "nurpawiki" in
  let db_passwd = "barfoo" in
  let conn =
    new Psql.connection ~host:"localhost"
      ~dbname:db_name ~user:db_user
(*      ~port:(Option.default "" dbcfg.db_port)*)
      ~password:db_passwd
      () in

  let sql = 
    "SELECT schemaname,tablename from pg_tables WHERE (schemaname = 'public' OR schemaname = 'nw') AND tablename = 'todos'" in
  let r = guarded_exec ~conn sql in
  let row_str = String.concat "." (List.hd r#get_all_lst) in
  Printf.printf "DB result: '%s'\n" row_str

