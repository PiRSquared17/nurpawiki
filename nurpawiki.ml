
open XHTML.M
open Eliom
open Eliom.Xhtml
open Lwt
open ExtList
open ExtString

module Psql = Postgresql
module P = Printf

module OrdInt = struct type t = int let compare a b = compare a b end
module IMap = Map.Make (OrdInt)

type todo = 
    {
      t_id : int;
      t_descr : string;
      t_completed : bool;
      t_priority : int;
      t_activation_date : string;
    }

type page = 
    {
      p_id : int;
      p_descr : string;
    }

type activity_type = 
    AT_create_todo
  | AT_complete_todo
  | AT_work_on_todo
  | AT_create_page
  | AT_edit_page

type activity =
    {
      a_id : int;
      a_activity : activity_type;
      a_date : string;
      a_todo_descr : string option;
    }

type search_result_type = SR_page | SR_todo

type search_result =
    {
      sr_id : int;
      sr_headline : string;
      sr_result_type : search_result_type;
      sr_page_descr : string option;
    }


let activity_type_of_string = function
    "create_todo" -> AT_create_todo
  | "complete_todo" -> AT_complete_todo
  | "work_on_todo" -> AT_work_on_todo
  | "create_page" -> AT_create_page
  | "edit_page" -> AT_edit_page
  | _ -> assert false

let (>>) f g = g f

let newline_re = Str.regexp "\n"

let wiki_view_page = 
  new_service ["view"] ((string "p")
                        ** (opt (int "completed_todo_id"))
                        ** (opt (int "up_pri"))
                        ** (opt (int "down_pri"))
                        ** (opt (bool "printable"))) ()

let wiki_edit_page = new_service ["edit"] (string "p") ()

let scheduler_page = new_service ["scheduler"] unit ()

let edit_todo_get_page = new_service ["edit_todo"] (opt (int "tid")) ()

let edit_todo_page = 
  new_post_service
    ~fallback:edit_todo_get_page 
    ~post_params:any () (*(list "tid" (bool "b" ** string "n")) ()*)

let history_page = new_service ["history"] unit ()

let search_page = new_service ["search"] (string "q") ()

let benchmark_page = new_service ["benchmark"] (string "test") ()

module WikiDB =
  struct
    let db_conn =
      try
        new Psql.connection ~host:"localhost" ~dbname:"nurpawiki" ~user:"jhellsten" (*~password:"jhellsten"*) ()
      with
        (Psql.Error e) as ex ->
          (match e with
             Psql.Connection_failure msg -> 
               P.eprintf "psql failed : %s\n" msg;
               raise ex
           | _ -> 
               P.eprintf "psql failed : %s\n" "unknown";
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
        
    (* Use this tuple format when querying TODOs to be parsed by
       parse_todo_result *)
    let todo_tuple_format = "id,descr,completed,priority,activation_date" 

    let todo_of_row row = 
      let id = int_of_string (List.nth row 0) in
      let descr = List.nth row 1 in
      let completed = (List.nth row 2) = "t" in
      let pri = List.nth row 3 in
      {
        t_id = id;
        t_descr = descr; 
        t_completed = completed;
        t_priority = int_of_string pri;
        t_activation_date =  List.nth row 4;
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
               P.eprintf "psql failed : %s\n" "unknown";
               raise ex)

    (* Sub-query for mapping activity strings into integers *)
    let activity_id_of_activity activity = 
      "(SELECT id FROM activities WHERE activity = '"^activity^"')"

    let insert_todo_activity todo_id ?(page_ids=None) activity =
      match page_ids with
        None ->
          "INSERT INTO activity_log(activity_id,todo_id) VALUES ("^
            (activity_id_of_activity activity)^", "^todo_id^")"
      | Some pages ->
          let insert_pages = 
            List.map
              (fun page_id -> 
                 "INSERT INTO activity_in_pages(activity_log_id,page_id) "^
                   "VALUES (CURRVAL('activity_log_id_seq'), "^string_of_int page_id^")")
              pages in
          let page_act_insert = String.concat "; " insert_pages in
          "INSERT INTO activity_log(activity_id,todo_id) VALUES ("^
            (activity_id_of_activity activity)^", "^todo_id^"); "^
            page_act_insert

    let insert_save_page_activity (page_id : int) =
      let sql = "BEGIN;
INSERT INTO activity_log(activity_id) 
       VALUES ("^activity_id_of_activity "edit_page"^");
INSERT INTO activity_in_pages(activity_log_id,page_id) 
       VALUES (CURRVAL('activity_log_id_seq'), "^string_of_int page_id^");
COMMIT" in
      ignore (guarded_exec sql)

    let query_todos_by_ids todo_ids = 
      if todo_ids <> [] then
        let ids = String.concat "," (List.map string_of_int todo_ids) in
        let r = guarded_exec ("SELECT "^todo_tuple_format^" from todos WHERE id IN ("^ids^")") in
        List.map todo_of_row (r#get_all_lst)
      else
        []

    let update_activation_date_for_todos todo_ids new_date =
      if todo_ids <> [] then
        let ids = String.concat "," (List.map string_of_int todo_ids) in
        let sql = 
          "UPDATE todos SET activation_date = '"^new_date^"' WHERE id IN ("^
            ids^")" in
        ignore (guarded_exec sql)


    (* Query TODOs and sort by priority & completeness *)
    let query_all_active_todos () =
      let r = guarded_exec
        ("SELECT "^todo_tuple_format^" FROM todos "^
           "WHERE activation_date <= current_date AND completed = 'f' "^
           "ORDER BY completed,priority,id") in
      List.map todo_of_row r#get_all_lst

    let query_upcoming_todos date_criterion =
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
        ("SELECT "^todo_tuple_format^" FROM todos "^
           "WHERE "^date_comparison^" AND completed='f' ORDER BY activation_date,priority,id") in
      List.map todo_of_row r#get_all_lst
      
    let new_todo page_id descr =
      (* TODO: could wrap this into BEGIN .. COMMIT if I knew how to
         return the data from the query! *)
      let sql = 
"INSERT INTO todos(descr) values('"^escape descr^"'); 
 INSERT INTO todos_in_pages(todo_id,page_id) values(CURRVAL('todos_id_seq'), "
        ^string_of_int page_id^");"^
        (insert_todo_activity 
           "(SELECT CURRVAL('todos_id_seq'))" ~page_ids:(Some [page_id]) "create_todo")^";
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
      let sql = "SELECT "^todo_tuple_format^" FROM todos where id in "^
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
    let complete_task id =
      let page_ids =
        try 
          Some (List.map (fun p -> p.p_id) (IMap.find id (todos_in_pages [id])))
        with Not_found -> None in
      let ids = string_of_int id in
      let sql = "BEGIN;
UPDATE todos SET completed = 't' where id="^ids^";"^
        (insert_todo_activity ids ~page_ids "complete_todo")^"; COMMIT" in
      ignore (guarded_exec sql)

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

    let query_activities () = 
      guarded_exec "SELECT id,activity FROM activities"
      
    let activity_id_to_string =
      let r = query_activities () in
      List.fold_left
        (fun acc row ->
           let id = int_of_string (List.nth row 0) in
           let activity = List.nth row 1 in
           IMap.add id (activity_type_of_string activity) acc)
        IMap.empty r#get_all_lst

    let query_past_activity () =
      let sql =
        "SELECT activity_log.id,activity_id,activity_timestamp,todos.descr "^
          "FROM activity_log LEFT OUTER JOIN todos "^
          "ON activity_log.todo_id = todos.id AND activity_log.activity_timestamp < now() "^
          "ORDER BY activity_timestamp DESC" in
      let r = guarded_exec sql in
      r#get_all_lst >>
        List.map
          (fun row ->
             let id = int_of_string (List.nth row 0) in
             let act_id = List.nth row 1 in
             let time = List.nth row 2 in
             let descr = List.nth row 3 in
             { a_id = id;
               a_activity = IMap.find (int_of_string act_id) activity_id_to_string;
               a_date = time;
               a_todo_descr = if descr = "" then None else Some descr; })

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

  end

let iso_date_re = Str.regexp "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)"

let date_of_string s = 
  if Str.string_match iso_date_re s 0 then
    let year = int_of_string (Str.matched_group 1 s) in
    let month = int_of_string (Str.matched_group 2 s) in
    let day = int_of_string (Str.matched_group 3 s) in
    Date.make year month day
  else 
    assert false

let iso_date_time_re = Str.regexp "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\) .*"

let date_of_date_time_string s = 
  if Str.string_match iso_date_time_re s 0 or Str.string_match iso_date_re s 0 then
    let year = int_of_string (Str.matched_group 1 s) in
    let month = int_of_string (Str.matched_group 2 s) in
    let day = int_of_string (Str.matched_group 3 s) in
    Date.make year month day
  else
    begin
      P.eprintf "invalid date '%s'\n" s;
      assert false
    end

(* Deal with Wiki markup *)
module WikiML =
  struct
    type preproc_line = 
        [ `Wiki of string
        | `NoWiki of string list
        ]

    let ws_or_empty_re = Str.regexp "^\\([ \t\n\r]*\\)$"

    let h1_re = Str.regexp "^=\\(.*\\)=\\([ \n\r]*\\)?$"
    let h2_re = Str.regexp "^==\\(.*\\)==\\([ \n\r]*\\)?$"
    let h3_re = Str.regexp "^===\\(.*\\)===\\([ \n\r]*\\)?$"
    let list_re = Str.regexp "^[ ]?\\([*]+\\) \\(.*\\)\\([ \n\r]*\\)?$"

    let is_list = function 
        `Wiki line ->
          Str.string_match list_re line 0
      | `NoWiki _ -> false

    let is_list_or_empty = function 
        `Wiki line ->
          Str.string_match list_re line 0 || Str.string_match ws_or_empty_re line 0
      | `NoWiki _ -> false

    let take_while pred lines =
      let rec loop acc = function 
          (x::xs) as lst -> 
            if pred x then
              loop (x::acc) xs
            else 
              (lst, List.rev acc)
        | [] ->
            ([], List.rev acc) in
      loop [] lines

    let string_of_priority = function
        3 -> "lo"
      | 2 -> "med"
      | 1 -> "hi"
      | _ -> "INTERNAL ERROR: PRIORITY OUT OF RANGE"

    let accepted_chars_ = "a-zA-Z\128-\2550-9_!\"§°#%&/()=?+.,;:{}'@\\$\\^\\*`´<>"
    let accepted_chars_sans_ws = "["^accepted_chars_^"-]+"
    let accepted_chars = "["^accepted_chars_^" -]+"
    let text_re = Str.regexp ("\\("^accepted_chars_sans_ws^"\\)")
    let wikilink_re = Str.regexp "\\([A-Z][a-z]+\\([A-Z][a-z]+\\)+\\)"

    let todo_re = 
      Str.regexp ("\\[todo:\\([0-9]+\\)\\("^accepted_chars^"\\)?\\]")

    let wikilinkanum_re = 
      Str.regexp ("\\(\\[\\(wiki\\|file\\|http\\):\\("^accepted_chars_sans_ws^
                    "\\)[ ]+\\("^accepted_chars^"\\)\\]\\)")

    let wikilinkanum_no_text_re = 
      Str.regexp ("\\(\\[\\(wiki\\|file\\|http\\):\\("^accepted_chars_sans_ws^"\\)\\]\\)")

    let ws_re = Str.regexp "\\([ \t]+\\).*"

    let priority_css_class = function
        3 -> "todo_pri_lo"
      | 2 -> "todo_pri_med"
      | 1 -> "todo_pri_hi"
      | _ -> "internal_error"

    let open_pre_re = Str.regexp "^\\(<pre>\\|8<\\)[ \r\n]*$"
    let close_pre_re = Str.regexp "^\\(</pre>\\|8<\\)[ \r\n]*$"

    (* WikiML preprocessor: translate a list of lines into
       normal lines and blocks of PRE blocks that contain
       verbatim lines. *)
    let preprocess lines =
      let rec loop acc = function
          x::xs ->
            if Str.string_match open_pre_re x 0 then
              begin
                (* Handle <pre>..</pre>, {{{..}}} *)
                let (after_pre,contents) =
                  take_while 
                    (fun x -> not (Str.string_match close_pre_re x 0)) xs in
                let next = 
                  match after_pre with [] -> [] | _::next -> next in
                loop (`NoWiki contents :: acc) next
              end
            else 
              loop (`Wiki x::acc) xs
        | [] ->
            List.rev acc in
      loop [] lines

    let wikitext_of_preprocessed_lines preproc_lines =
      List.flatten
        (List.map
           (function
                `Wiki text -> [text]
              | `NoWiki lines -> ("<pre>" :: lines) @ ["</pre>"])
           preproc_lines)

    (* Todo item manipulation HTML *)
    let complete_todo sp page completed id =
      if completed then
        pcdata "" (* TODO redundant *)
      else 
        a ~a:[a_title "Mark as completed!"]
          ~service:wiki_view_page ~sp:sp
          [pcdata ""]
          (*img ~alt:"Mark complete" 
             ~src:(make_uri (static_dir sp) sp ["mark_complete.png"]) ()] *)
          (page,(Some id,(None,(None, None))))

    let priority_arrow sp page id up_or_down =
      let (title,arrow_img,params) = 
        if up_or_down then 
          ("Raise priority!", "arrow_up.png", (Some id, (None, None)))
        else 
          ("Lower priority!", "arrow_down.png", (None, (Some id, None))) in
      let arrow_img =
        img ~alt:"Logo" ~src:(make_uri (static_dir sp) sp [arrow_img]) () in
      a ~a:[a_title title] ~service:wiki_view_page ~sp:sp 
        [arrow_img] (page,(None,params))

    let up_arrow sp page id = priority_arrow sp page id true

    let down_arrow sp page id = priority_arrow sp page id false

    let mod_priorities sp page completed pri id =
      if completed then 
        []
      else 
        [up_arrow sp page id; down_arrow sp page id]

    let todo_modify_buttons sp page todo_id todo =
      let completed = todo.t_completed in
      span ~a:[a_class ["no_break"]]
        ((mod_priorities sp page completed todo.t_priority todo_id) @
           (if not completed then
              []
            else []) @
           [complete_todo sp page completed todo_id])
    
    let translate_list items =
      let add_ul t lst = 
        t @ [ul (List.hd lst) (List.tl lst)] in
      let rec loop = function
          ((nesting1,text1)::(nesting2,text2)::xs) as lst ->
            if nesting1 = nesting2 then
              (li text1)::loop (List.tl lst)
            else if nesting1 < nesting2 then (* enter *)
              let (next_same_level,same_or_higher) = 
                take_while (fun (n,_) -> n >= nesting2) (List.tl lst) in
              (li (add_ul text1 (loop same_or_higher)))::loop next_same_level
            else (* leave *)
              loop (List.tl lst)
        | (nesting,text)::[] ->
            [(li text)]
        | [] -> [] in
      let list_items = loop items in
      ul (List.hd list_items) (List.tl list_items)

    let parse_lines sp cur_page (todo_data : todo IMap.t) preprocessed_lines =

      let wikilink scheme page text = 
        let ext_img = 
          img ~alt:"External link" 
            ~src:(make_uri (static_dir sp) sp ["external_link.png"]) () in
        if scheme = "wiki" || scheme = "" then
          let t = if text = "" then page else text in
          if WikiDB.wiki_page_exists page then
            a wiki_view_page sp [pcdata t] (page,(None,(None,(None,None))))
          else 
            a ~a:[a_class ["missing_page"]] 
              ~service:wiki_view_page ~sp:sp [pcdata t] 
              (page,(None,(None,(None,None))))
        else (* External link *)
          let url = scheme^":"^page in
          let t = if text = "" then url else text in
          a (new_external_service 
               ~url:[url]
               ~get_params:unit
               ~post_params:unit ()) sp [pcdata t; ext_img] () in

      let add_html html_acc html =
        html::html_acc in

      let add_todo acc todo =
        let todo_id = int_of_string todo in
        let html = 
          try 
            let todo = IMap.find todo_id todo_data in
            let completed = todo.t_completed in
            let style = 
              if completed then 
                ["todo_descr_completed"]
              else 
                ["todo_descr"; priority_css_class todo.t_priority] in
            span 
              [todo_modify_buttons sp cur_page todo_id todo;
               span ~a:[a_class style] [pcdata todo.t_descr]]
          with Not_found -> 
            (pcdata "UNKNOWN TODO ID!") in
        add_html acc html in

      let rec parse_text acc s =
        let len = String.length s in
        let rec loop acc charpos =
          if charpos >= len then
            acc
          else 
            if s.[charpos] = '\t' then 
              let m = "\t" in
              loop (add_html acc (pcdata m)) (charpos+1)
            else if s.[charpos] = ' ' then 
              let m = " " in
              loop (add_html acc (pcdata m)) (charpos+1)
            else if s.[charpos] = '\r' || s.[charpos] = '\n' then
              acc
            else if Str.string_partial_match todo_re s charpos then
              let fm_len = String.length (Str.matched_group 0 s) in
              let todo_id = Str.matched_group 1 s in
              loop (add_todo acc todo_id) (charpos+fm_len)
            else if Str.string_match wikilink_re s charpos then
              let m = Str.matched_group 1 s in
              loop (add_html acc (wikilink "" m m)) (charpos+(String.length m))
            else if Str.string_match wikilinkanum_re s charpos then
              let scheme = Str.matched_group 2 s in
              let page = Str.matched_group 3 s in
              let text = Str.matched_group 4 s in
              let fm_len = String.length (Str.matched_group 1 s) in
              loop (add_html acc (wikilink scheme page text)) (charpos+fm_len)
            else if Str.string_match wikilinkanum_no_text_re s charpos then
              let scheme = Str.matched_group 2 s in
              let page = Str.matched_group 3 s in
              let text = "" in
              let fm_len = String.length (Str.matched_group 1 s) in
              loop (add_html acc (wikilink scheme page text)) (charpos+fm_len)
            else if Str.string_match text_re s charpos then
              let m = Str.matched_group 1 s in
              loop (add_html acc (pcdata m)) (charpos+(String.length m))
            else
              begin
                let s = (String.sub s charpos ((String.length s)-charpos)) in
                let len = String.length s in
                P.eprintf "parsing: '";
                for i = 0 to len-1 do
                  P.eprintf "%i " (int_of_char s.[i])
                done;
                P.eprintf "'\n";
                add_html acc (pcdata ("WIKI SYNTAX ERROR on line: '"^s^"'"))
              end
        in
        List.rev (loop acc 0) in
      
      let rec loop acc = function
          ((`Wiki x) as wx::xs) as lst ->
            if Str.string_match h3_re x 0 then
              loop ((h3 [pcdata (Str.matched_group 1 x)])::acc) xs
            else if Str.string_match h2_re x 0 then
              loop ((h2 [pcdata (Str.matched_group 1 x)])::acc) xs
            else if Str.string_match h1_re x 0 then
              loop ((h1 [pcdata (Str.matched_group 1 x)])::acc) xs
            else if is_list wx then
              (* Grab all lines starting with '*': *)
              let (after_bullets,bullets) =
                take_while is_list_or_empty lst in
              let list_items = 
                List.filter_map
                  (function 
                       (`Wiki e) as wl ->
                         if Str.string_match ws_or_empty_re e 0 then
                           (* Empty line, ignore *)
                           None
                         else if is_list wl then
                           let n_stars = String.length (Str.matched_group 1 e) in
                           Some (n_stars, parse_text [] (Str.matched_group 2 e))
                         else 
                           assert false
                     | `NoWiki _ -> assert false) bullets in
              loop ((translate_list list_items)::acc) after_bullets
            else if Str.string_match ws_or_empty_re x 0 then
              loop acc xs
            else 
              loop ((p (parse_text [] x))::acc) xs
        | (`NoWiki x::xs) ->
            loop (pre [pcdata (String.concat "\n" x)]::acc) xs
        | [] -> List.rev acc in

      loop [] preprocessed_lines

  end

let load_wiki_page page_id =
  let lines = 
    WikiDB.load_wiki_page page_id >> Str.split newline_re in
  let preprocd = WikiML.preprocess lines in
  preprocd

let wikiml_to_html sp (page_id:int) (page_name:string) todo_data =
  load_wiki_page page_id >> WikiML.parse_lines sp page_name todo_data

(* Use this as the basis for all pages.  Includes CSS etc. *)
let html_stub sp ?(javascript=[]) body_html =
  let script src = 
    js_script ~a:[a_defer `Defer] ~uri:(make_uri (static_dir sp) sp [src]) in
  let scripts = 
    script "nurpawiki.js" :: (List.map script javascript) in
  return 
    (html 
       (head (title (pcdata "")) 
          (scripts @ [css_link (make_uri (static_dir sp) sp ["style.css"])]))
       (body 
          body_html))

(* TODO need to mangle the string to be suitable as an URL *)
let urlify_wiki_page_descr s =
  s

(* Hash page description to a CSS palette entry.  Used to syntax
   highlight wiki page links based on their names. *)
let css_palette_ndx_of_wikipage page_id = 
  "palette"^(string_of_int (page_id mod 12))

let todo_page_links_of_pages sp ?(colorize=false) ?(link_css_class=None) ?(insert_parens=true) pages =
  let attrs page = 
    let color_css = 
      if colorize then [css_palette_ndx_of_wikipage page.p_id] else [] in
    match link_css_class with
      Some c -> [a_class ([c] @ color_css)]
    | None -> [a_class color_css] in
  let link page = 
    a ~a:(attrs page) ~service:wiki_view_page ~sp:sp [pcdata page.p_descr]
      (urlify_wiki_page_descr page.p_descr,(None,(None,(None,None)))) in
  let rec insert_commas acc = function
      (x::_::xs) as lst ->
        insert_commas (pcdata ", "::x::acc) (List.tl lst)
    | x::[] ->
        insert_commas (x::acc) []
    | [] -> List.rev acc in
  let insert_parens_html lst = 
    pcdata " ("::lst @ [pcdata ")"] in
  if pages <> [] then
    let lst = insert_commas [] (List.map link pages) in
    if insert_parens then 
      insert_parens_html lst
    else 
      lst
  else
    []

let todo_page_links sp todo_in_pages ?(colorize=false) ?(link_css_class=None) ?(insert_parens=true) id =
  let pages = try IMap.find id todo_in_pages with Not_found -> [] in
  todo_page_links_of_pages ~colorize sp pages

let todo_list_table_html sp cur_page todos =
  (* Which pages contain TODOs, mapping from todo_id -> {pages} *)
  let todo_in_pages =
    WikiDB.todos_in_pages (List.map (fun todo -> todo.t_id) todos) in
  let todo_page_link todo =
    let descr = todo.t_descr in
    let page_links =
      let c = "wiki_pri_"^WikiML.string_of_priority todo.t_priority in
      todo_page_links sp todo_in_pages ~link_css_class:(Some c) (todo.t_id) in
    pcdata descr :: page_links in

  table ~a:[a_class ["todo_table"]]
    (tr 
       (th [pcdata "Id"]) [th [pcdata "Description"]])
    (List.map
       (fun todo ->
          let id = todo.t_id in
          let completed = todo.t_completed in
          let row_pri_style = 
            if completed then
              "todo_completed_row" 
            else 
              WikiML.priority_css_class todo.t_priority in
          let row_class = 
            (row_pri_style::(if completed then ["todo_table_completed"] else [])) in
          (tr 
             (td ~a:[a_class row_class] [pcdata (string_of_int id)])
             [td ~a:[a_class row_class] (todo_page_link todo);
              td [(WikiML.todo_modify_buttons sp cur_page id todo)]]))
       todos)

let navbar_html sp ?(wiki_page_links=[]) ?(todo_list_table=[]) content =
  let home_link link_text =
    a ~service:wiki_view_page 
      ~a:[a_accesskey 'h'; a_class ["ak"]] ~sp:sp link_text 
      ("WikiStart", (None, (None,(None,None)))) in
  let scheduler_link =
    a ~service:scheduler_page
      ~a:[a_accesskey 'r'; a_class ["ak"]] ~sp:sp 
      [img ~alt:"Scheduler" ~src:(make_uri (static_dir sp) sp ["calendar.png"]) ();
       pcdata "Scheduler"] () in
  let history_link =
    a ~service:history_page
      ~a:[a_accesskey 'r'; a_class ["ak"]] ~sp:sp 
      [img ~alt:"History" ~src:(make_uri (static_dir sp) sp ["home.png"]) ();
       pcdata "History"] () in

(*  let search_input =
    [get_form search_page sp
       (fun (chain : ([`One of string] Eliom.param_name)) -> 
          [p [(string_input ~a:[a_id "q"] chain);
              submit_input "Search"]])] in*)

  let search_input =
    [get_form search_page sp
       (fun (chain : ([`One of string] Eliom.param_name)) -> 
          [p [string_input ~input_type:`Submit ~value:"Search" ();
              textarea ~name:chain ~rows:1 ~cols:50
                ~value:(pcdata "") ()]])] in

  let space = [pcdata " "] in
  [div ~a:[a_id "navbar"]
     ([home_link 
         [img ~alt:"Home" ~src:(make_uri (static_dir sp) sp ["home.png"]) ();
          pcdata "Home"]] @ 
        space @ [scheduler_link] @ 
         space @ [history_link] @ space @ wiki_page_links @
         [br ()] @ search_input @ [br ()] @ todo_list_table);
   div ~a:[a_id "content"]
     content]

let wiki_page_menu_html sp page content =
  let edit_link = 
    [a ~service:wiki_edit_page ~sp:sp ~a:[a_accesskey '1'; a_class ["ak"]]
       [img ~alt:"Edit" ~src:(make_uri (static_dir sp) sp ["edit.png"]) ();
        pcdata "Edit page"] page] in
  let printable_link =
    [a ~service:wiki_view_page ~sp:sp
       ~a:[a_accesskey 'p'; a_class ["ak"]] [pcdata "Print"]
       (page, (None, (None,(None,Some true))))] in
  let todo_list = 
    todo_list_table_html sp page (WikiDB.query_all_active_todos ()) in
  navbar_html sp ~wiki_page_links:(edit_link @ [br ()] @ printable_link)
        ~todo_list_table:[todo_list] content

let wiki_page_contents_html sp page_id page_name todo_data ?(content=[]) () =
  wiki_page_menu_html sp page_name
    (content @ wikiml_to_html sp page_id page_name todo_data)

let view_page sp page_id page_name ~printable =
  let todos = WikiDB.query_page_todos page_id in
  if printable <> None && Option.get printable = true then
    let page_content = wikiml_to_html sp page_id page_name todos in
    html_stub sp page_content
  else 
    html_stub sp
      (wiki_page_contents_html sp page_id page_name todos ())
      
let new_todo_re = 
  Str.regexp ("\\[todo \\("^WikiML. accepted_chars^"\\)\\]")

let check_new_and_removed_todos page_id lines =
  (* Figure out which TODOs are mentioned on the wiki page: *)
  let page_todos = 
    List.fold_left
      (fun acc -> function
           `Wiki line ->
             let rec loop acc n =
               try 
                 let beg = Str.search_forward WikiML.todo_re line n in
                 loop (Str.matched_group 1 line::acc)
                   (beg+(String.length (Str.matched_group 0 line)))
               with 
                 Not_found -> acc in
             loop acc 0
         | `NoWiki _ -> acc) [] lines in
  WikiDB.update_page_todos page_id (List.map int_of_string page_todos)

(* Insert new TODOs from the wiki ML into DB and replace [todo descr]
   by [todo:ID] *)
let convert_new_todo_items page =
  List.map
    (function
         `Wiki line -> 
           `Wiki (Str.global_substitute new_todo_re
                    (fun str -> 
                       let descr = Str.matched_group 1 str in
                       let id = WikiDB.new_todo page descr in
                       "[todo:"^id^" "^descr^"]") line)
       | (`NoWiki _) as x -> x)

(* Convert [todo:ID] into [todo:ID 'Description'] before going into
   Wiki page edit textarea. *)
let annotate_old_todo_items page page_todos (lines : WikiML.preproc_line list) =
  List.map
    (function
         `Wiki line ->
           `Wiki 
             (Str.global_substitute WikiML.todo_re
                (fun str -> 
                   let id = Str.matched_group 1 str in
                   let (descr,completed) = 
                     try 
                       let todo = IMap.find (int_of_string id) page_todos in
                       (todo.t_descr,if todo.t_completed then "(x) " else "")
                     with 
                       Not_found -> ("UNKNOWN TODO","") in
                   "[todo:"^id^" "^completed^descr^"]") line)
       | (`NoWiki line) as x ->
           x) lines

(* Save page as a result of /edit?p=Page *)
let service_save_page_post =
  register_new_post_service
    ~fallback:wiki_view_page
    ~post_params:(string "value")
    (fun sp (page,(_,(_,_))) value -> 
       (* Check if there are any new or removed [todo:#id] tags and
          updated DB page mappings accordingly: *)
       let wikitext = Str.split newline_re value >> WikiML.preprocess in
       let page_id = WikiDB.page_id_of_page_name page in
       check_new_and_removed_todos page_id wikitext;
       (* Convert [todo Description] items into [todo:ID] format, save
          descriptions to database and save the wiki page contents. *)
       let edited_wiki_page =
         convert_new_todo_items page_id wikitext in
       let wiki_plaintext = 
         WikiML.wikitext_of_preprocessed_lines edited_wiki_page in
       (* Log activity: *)
       WikiDB.insert_save_page_activity page_id;
       WikiDB.save_wiki_page page_id wiki_plaintext;
       view_page sp page_id page ~printable:(Some false))

(* Use to create a "cancel" button for user submits *)
let cancel_link service sp params =
  a ~a:[a_class ["cancel_edit"]] ~service:service ~sp:sp 
    [pcdata "Cancel"] 
    params

(* /edit?p=Page *)
let _ =
  register wiki_edit_page
    (fun sp page_name () -> 
       let (page_id,page_todos,preproc_wikitext) = 
         if WikiDB.wiki_page_exists page_name then
           let page_id = WikiDB.page_id_of_page_name page_name in
           let current_page_todos = WikiDB.query_page_todos page_id in
           (page_id,
            current_page_todos,
            load_wiki_page page_id >> annotate_old_todo_items page_name current_page_todos)
         else
           begin
             (WikiDB.new_wiki_page page_name, IMap.empty, [])
           end in
       let wikitext = 
         String.concat "\n" (WikiML.wikitext_of_preprocessed_lines preproc_wikitext) in
       let f =
         post_form service_save_page_post sp
           (fun chain -> 
              [(p [string_input ~input_type:`Submit ~value:"Save" (); 
                   cancel_link wiki_view_page sp
                     (page_name,(None, (None,(None,None))));
                   br ();
                   textarea ~name:chain ~rows:30 ~cols:80 
                     ~value:(pcdata wikitext) ()])])
           (page_name,(None, (None,(None,None)))) in
       html_stub sp
         (wiki_page_contents_html sp page_id page_name page_todos 
            ~content:[f] ()))

(* /view?p=Page *)
let _ = 
  register wiki_view_page
    (fun sp (page_name,(completed_todo_id, (up_pri,(down_pri,printable)))) () ->
       (* If user requested task/TODO completion, complete the task in
          DB: *)
       Option.may WikiDB.complete_task completed_todo_id;
       Option.may WikiDB.up_task_priority up_pri;
       Option.may WikiDB.down_task_priority down_pri;
       match WikiDB.find_page_id page_name with
         Some page_id ->
           view_page sp page_id page_name ~printable
       | None ->
           let f = 
             a wiki_edit_page sp [pcdata "Create new page"] page_name in
           html_stub sp
             (wiki_page_menu_html sp page_name [f]))

let clamp_date_to_today date =
  let today = Date.today () in
  let d = date_of_string date in
  begin
    match Date.compare today d with
      -1 -> d
    | 0 | 1 -> today
    | _ -> assert false
  end

let wiki_page_links sp todo_in_pages todo = 
  let id = todo.t_id in
  let c = "wiki_pri_"^WikiML.string_of_priority todo.t_priority in
  todo_page_links sp todo_in_pages ~link_css_class:(Some c) id

let view_scheduler_page sp =
  let today = Date.today () in
  let prettify_activation_date d =
    let d = date_of_string d in
    (* Clamp & prettify activation date *)
    begin 
      match Date.compare today d with
        -1 -> Printer.DatePrinter.sprint "%a %b %d, %Y" d
      | 0 | 1 -> "today"
      | _ -> assert false
    end in
    
  let todo_table_html sp todos f =
    let todo_in_pages =
      WikiDB.todos_in_pages (List.map (fun (_,todo) -> todo.t_id) todos) in

    let todo_choices = 
      List.map
        (fun (heading,todo) ->
           let todo_id_s = string_of_int todo.t_id in
           (*[tr (td ~a:[a_class ["rm_table_heading"]] [pcdata heading]) []]*)
           let pri_style = WikiML.priority_css_class todo.t_priority in
           tr
             (td ~a:[a_class ["rm_edit"]]
                [a ~a:[a_title "Edit"] ~service:edit_todo_get_page ~sp:sp
                   [img ~alt:"Edit" 
                      ~src:(make_uri (static_dir sp) sp ["edit_small.png"]) ()]
                   (Some todo.t_id)])
             [td [any_checkbox ~name:("e-"^ todo_id_s) ~value:"0" ()];
              (td ~a:[a_class ["no_break"; pri_style]] 
                 [pcdata (prettify_activation_date todo.t_activation_date)]);
              td ~a:[a_class [pri_style]] 
                ([pcdata todo.t_descr] @ wiki_page_links sp todo_in_pages todo)])
        todos in

    todo_choices in

  let todo_section sp todos f =
    (todo_table_html sp todos f) in

  let upcoming_pending =
    WikiDB.query_upcoming_todos (None,None) in
  let upcoming_tomorrow =
    WikiDB.query_upcoming_todos (None,Some 1) in
  let upcoming_todos_7_days =
    WikiDB.query_upcoming_todos (Some 1,Some 7) in
  let upcoming_todos_14_days =
    WikiDB.query_upcoming_todos (Some 7, Some 14) in
  let upcoming_all = 
    WikiDB.query_upcoming_todos (Some 14, None) in

  let mark_todo_hdr h = List.map (fun e -> (h, e)) in
  let merged_todos = 
    (mark_todo_hdr "Today" upcoming_pending) @ 
      (mark_todo_hdr "Tomorrow" upcoming_tomorrow) @
      (mark_todo_hdr "Next 7 days" upcoming_todos_7_days) @
      (mark_todo_hdr "Next 2 weeks" upcoming_todos_14_days) @
      (mark_todo_hdr "Everything else" upcoming_all) in

  (* TODO merge this HTML generation with other pages.  PROBLEM:
     don't know how to easily do that without duplicating the
     parameter passing of pages. *)
  let table f = 
    [p [any_input ~name:"tid" ~input_type:`Submit ~value:"Mass edit" ()];
     table
       (tr (th []) [th []; th [pcdata "Activates on"]; th [pcdata "Todo"]])
       (todo_section sp merged_todos f)] in

  let table' = 
    post_form edit_todo_page sp table None in

  html_stub sp
    (navbar_html sp 
       ([h1 [pcdata "Road ahead"]] @ [table']))

let scheduler_page_discard_todo_id = 
  register_new_service
    ~url:["scheduler"] ~get_params:(list "todo_ids" (int "tid"))
    (fun sp _ () -> view_scheduler_page sp)

(* Save page as a result of /edit_todo?todo_id=ID *)
let service_save_todo_item =
  register_new_post_service
    ~fallback:scheduler_page_discard_todo_id
    ~post_params:(string "activation_date")
    (fun sp todo_ids new_date ->
       WikiDB.update_activation_date_for_todos todo_ids new_date;
       view_scheduler_page sp)

let rec render_todo_editor sp todo_ids =

  let todos_to_edit = 
    List.filter_map
      (fun (bool_id,id) -> 
         None) todo_ids in
(* TODO
         if b then Some (int_of_string id) else None) todo_ids in
*)

  let todos_str = String.concat "," (List.map string_of_int todos_to_edit) in
  let todos = WikiDB.query_todos_by_ids todos_to_edit in

  let today_str = 
    Printer.DatePrinter.sprint "%i"(Date.today ()) in
    
  let smallest_date =
    List.fold_left 
      (fun min_date e -> min e.t_activation_date min_date) today_str todos in

  let smallest_date_str = 
    Printer.DatePrinter.sprint "%i"
      (clamp_date_to_today smallest_date) in

  let f =
    let todo_in_pages =
      WikiDB.todos_in_pages (List.map (fun todo -> todo.t_id) todos) in

    post_form service_save_todo_item sp
      (fun chain -> 
         [table 
            (tr (th [pcdata "ID"]) 
               [th [pcdata "Description"]; th [pcdata "Activates on"]])
            ((List.map
                (fun todo ->
                   let pri_style = WikiML.priority_css_class todo.t_priority in
                   let act_date = 
                     Printer.DatePrinter.sprint "%i"
                       (clamp_date_to_today todo.t_activation_date) in
                   tr ~a:[a_class [pri_style]]
                     (td [pcdata (string_of_int todo.t_id)])
                     [td (pcdata todo.t_descr :: wiki_page_links sp todo_in_pages todo); 
                      td ~a:[a_class ["no_break"]] [pcdata act_date]]) todos)
             @
                [tr
                   (td [])
                   [td ~a:[a_id "act_date"]
                      [string_input ~input_type:`Submit ~value:"Save" ();
                       string_input ~input_type:`Text ~name:chain 
                         ~a:[a_id "activation_date"; 
                             a_class ["act_date_input"];
                             a_value smallest_date_str] ~value:"" ();
                       cancel_link scheduler_page sp ()]]])]) todos_to_edit in
  let heading = [pcdata ("Edit TODOs "^todos_str)] in
  let help_str = 
    pcdata "NOTE: Below activation date will be assigned for all the items" in

  let calendar_js = 
    ["CalendarPopup.js";
     "date.js";
     "AnchorPosition.js";
     "PopupWindow.js";
     "nurpawiki_calendar.js"] in

  html_stub sp ~javascript:calendar_js
    (navbar_html sp 
       ((h1 heading)::[help_str; br(); f]))

let error_page sp msg =
  html_stub sp [h1 [pcdata ("ERROR: "^msg)]]

let render_todo_get_page sp = function
    Some todo_id ->
      render_todo_editor sp [(true, string_of_int todo_id)]
  | None ->
      error_page sp "edit_todo_fallback with no 'tid'"
  
let _ =
  register edit_todo_get_page
    (fun sp todo_id () -> render_todo_get_page sp todo_id)

let _ =
  register edit_todo_page
    (fun sp single_tid (todo_ids : (string * string) list) ->
       if todo_ids = [] then
         render_todo_get_page sp single_tid
       else 
         render_todo_editor sp todo_ids)


(* /scheduler *)
let _ =
  register scheduler_page
    (fun sp todo_id () ->
       view_scheduler_page sp)

let descr_of_activity_type = function
    AT_create_todo -> "Created"
  | AT_complete_todo -> "Completed"
  | AT_work_on_todo -> "Worked on"
  | AT_create_page -> "Created"
  | AT_edit_page -> "Edited"

module ReverseOrdString = 
  struct
    type t = String.t
    let compare a b = String.compare b a
  end

module RSMap = Map.Make (ReverseOrdString)

type act_group = 
    {
      ag_created_todos : (string * page list) list;
      ag_completed_todos : (string * page list) list;
      ag_edited_pages : page list;
    }

let empty_act_group = 
  {
      ag_created_todos = [];
      ag_completed_todos = [];
      ag_edited_pages = [];
  }

let group_activities activities activity_in_pages =
  List.fold_left
    (fun acc a ->
        let date = date_of_date_time_string a.a_date in
        let d = Printer.DatePrinter.sprint "%Y-%m-%d" date in
        let ag = try RSMap.find d acc with Not_found -> empty_act_group in
        let pages = try IMap.find a.a_id activity_in_pages with Not_found -> [] in
        let ag' = 
          match a.a_activity with
            AT_create_todo ->
              (match a.a_todo_descr with
                 Some descr ->
                   let e = (Option.get a.a_todo_descr, pages)::ag.ag_created_todos in
                   { ag with ag_created_todos = e }
               | None -> P.eprintf "no descr in activity_log %i\n" a.a_id; ag)
          | AT_complete_todo ->
              (match a.a_todo_descr with
                 Some descr ->
                   let e = (Option.get a.a_todo_descr, pages)::ag.ag_completed_todos in
                   { ag with ag_completed_todos = e }
               | None -> P.eprintf "no descr in activity_log %i\n" a.a_id; ag)
          | AT_create_page | AT_edit_page ->
              { ag with ag_edited_pages = pages @ ag.ag_edited_pages }
          | AT_work_on_todo -> ag in
        RSMap.add d ag' acc)
    RSMap.empty activities

let remove_duplicates strs = 
  let module PSet = 
    Set.Make (struct
                type t = page 
                let compare a b =  compare a.p_descr b.p_descr
              end) in
  let s = 
    List.fold_left (fun acc e -> PSet.add e acc) PSet.empty strs in
  PSet.fold (fun e acc -> e::acc) s []

let view_history_page sp =

  let activity = WikiDB.query_past_activity () in
  let activity_in_pages = WikiDB.query_activity_in_pages () in

  let prettify_date d =
    let d = date_of_date_time_string d in
    Printer.DatePrinter.sprint "%a %b %d, %Y" d in

  let activity_groups = group_activities activity activity_in_pages in

  let act_table = 
    table ~a:[a_class ["todo_table"]]
      (tr (th []) [th [pcdata "Activity"]; th [pcdata "Details"]])
      (List.rev
         (fst 
            (RSMap.fold
               (fun date e (lst_acc,prev_date) ->
                  let prettified_date = prettify_date date in
                  let date_text =
                    if prev_date = prettified_date then
                      []
                    else 
                      [pcdata prettified_date] in

                  let todo_html ty lst = 
                    List.rev
                      (List.mapi
                         (fun ndx (todo,pages) ->
                            (tr (td [])
                               [td (if ndx = 0 then [pcdata ty] else []);
                                td ([pcdata todo] @ 
                                      (todo_page_links_of_pages 
                                         ~colorize:true sp pages))]))
                         lst) in

                  let created_todos = todo_html "Created" e.ag_created_todos in
                  let completed_todos = todo_html "Completed" e.ag_completed_todos in
                  let pages_html = 
                    if e.ag_edited_pages <> [] then
                      [tr (td [])
                         [td [pcdata "Edited"];
                          td (todo_page_links_of_pages sp 
                                ~colorize:true ~insert_parens:false 
                                (remove_duplicates e.ag_edited_pages))]]
                    else 
                      [] in
                  
                  (* NOTE: 'tr' comes last as we're building the page
                     in reverse order *)
                  (pages_html @ created_todos @ completed_todos @ 
                     [tr (td ~a:[a_class ["no_break"; "h_date_heading"]] date_text) []] @ lst_acc,
                   prettified_date))
               activity_groups ([],"")))) in
  html_stub sp
    (navbar_html sp 
       ([h1 [pcdata "Blast from the past"]] @ [act_table]))

(* /history *)
let _ =
  register history_page
    (fun sp todo_id () ->
       view_history_page sp)

(* /benchmark?test=empty,one_db *)
let _ =
  let gen_html sp = function
      "empty" ->
        (html 
           (head (title (pcdata "")) [])
           (body [p [pcdata "Empty page"]]))
    | "db1" ->
        ignore (WikiDB.query_activities ());
        (html 
           (head (title (pcdata "")) [])
           (body [p [pcdata "Test one DB query"]]))
    | _ ->
        (html 
           (head (title (pcdata "")) [])
           (body [p [pcdata "invalid 'test' param!"]])) in
  
  register benchmark_page
    (fun sp test_id () ->
       return (gen_html sp test_id))

(* /search?q=[keyword list] *)
let _ =
  (* Parse <b></b> tags from headline and convert to b tags. *)
  let html_of_headline h = 
    let rec html_of_elem = function
        Nethtml.Element ("b",_,c) ->
          let c = 
            List.flatten 
              (List.rev (List.fold_left (fun acc e -> (html_of_elem e)::acc) [] c)) in
          [(span ~a:[a_class ["sr_hilite"]] c)]
      | Nethtml.Element (_,_,_) -> []
      | Nethtml.Data s -> [pcdata s] in

    let ch = new Netchannels.input_string h in
    let doc = Nethtml.parse ch in
    List.flatten
      (List.rev
         (List.fold_left (fun acc e -> (html_of_elem e)::acc) [] doc) )in

  let render_results sp search_results = 
    List.flatten
      (List.map 
         (fun sr ->
            match sr.sr_result_type with
              SR_page ->
                let link descr = 
                  a ~a:[a_class ["sr_link"]] ~service:wiki_view_page ~sp:sp 
                    [pcdata descr]
                    (urlify_wiki_page_descr descr,(None,(None,(None,None)))) in
                [p ([link (Option.get sr.sr_page_descr); br ()] @ 
                      html_of_headline sr.sr_headline)]
            | SR_todo -> assert false) search_results) in
  let gen_search_page sp search_str =
    let search_results = WikiDB.search_wikipage search_str in
    html_stub sp
      (navbar_html sp 
         ([h1 [pcdata "Search results"]] @ (render_results sp search_results))) in
    
  register search_page
    (fun sp search_str () ->
       gen_search_page sp search_str)

  
