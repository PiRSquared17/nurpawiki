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

open Lwt
open ExtList
open ExtString

open Services
open Types
open Util

module Psql = Postgresql
module P = Printf

(* TODO no need to extract here *)
let matches_pcre rex s =
  try ignore (Pcre.extract ~rex s); true with Not_found -> false

let (>>) f g = g f

let newline_re = Pcre.regexp "\n"

let task_side_effect_complete sp task_id () =
  Session.action_with_user_login sp
    (fun user ->
       if Privileges.can_complete_task task_id user then
         begin
           Database.complete_task user.user_id task_id;
           [Action_completed_task task_id]
         end
       else
         [])


let task_side_effect_undo_complete sp task_id () =
  Session.action_with_user_login sp
    (fun user ->
       if Privileges.can_complete_task task_id user then
         Database.uncomplete_task user.user_id task_id;
       [])

let task_side_effect_mod_priority sp (task_id, dir) () =
  Session.action_with_user_login sp
    (fun user ->
       if Privileges.can_modify_task_priority task_id user then
         if dir = false then 
           Database.down_task_priority task_id
         else 
           Database.up_task_priority task_id;
       [])
             


let () =
  Eliompredefmod.Actions.register 
    ~service:task_side_effect_complete_action task_side_effect_complete;
  Eliompredefmod.Actions.register 
    ~service:task_side_effect_undo_complete_action task_side_effect_undo_complete;
  Eliompredefmod.Actions.register 
    ~service:task_side_effect_mod_priority_action task_side_effect_mod_priority

let make_static_uri = Html_util.make_static_uri

(* Deal with Wiki markup *)
module WikiML =
  struct
    type preproc_line = 
        [ `Wiki of string
        | `NoWiki of string list
        ]

    let ws_or_empty_re = Pcre.regexp "^([ \t\n\r]*)$"

    let h1_re = Pcre.regexp "^=(.*)=([ \n\r]*)?$"
    let h2_re = Pcre.regexp "^==(.*)==([ \n\r]*)?$"
    let h3_re = Pcre.regexp "^===(.*)===([ \n\r]*)?$"
    let list_re = Pcre.regexp "^[ ]?([*]+) (.*)([ \n\r]*)?$"
      
    let is_list = function 
        `Wiki line ->
          match_pcre_option list_re line
      | `NoWiki _ -> 
          None
          
    let is_list_or_empty = function 
        `Wiki line ->
          matches_pcre list_re line || matches_pcre ws_or_empty_re line
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

    let accepted_chars_ = "a-zA-Z\128-\2550-9_!\"§°#%&/()=?+.,;:{}'@\\$\\^\\*`´<>"
    let accepted_chars_sans_ws = "["^accepted_chars_^"-]+"
    let accepted_chars = "["^accepted_chars_^" -]+"

    let italic_re = 
      Str.regexp ("\\(_\\("^(del_substring accepted_chars "_")^"\\)_\\)")

    let bold_re = 
      Str.regexp ("\\(\\*\\("^del_substring accepted_chars "\\*" ^"\\)\\*\\)")

    let code_re = 
      Str.regexp ("\\(`\\("^del_substring accepted_chars "`" ^"\\)`\\)")

    let text_re = Str.regexp ("\\("^accepted_chars_sans_ws^"\\)")
    let wikilink_re = Str.regexp "\\([!]?[A-Z][a-z]+\\([A-Z][a-z]+\\)+\\)"
      
    let todo_re = 
      Str.regexp ("\\[todo:\\([0-9]+\\)\\( "^accepted_chars^"\\)?\\]")

    let wikilinkanum_re = 
      Str.regexp ("\\(\\[\\(wiki\\|file\\|http\\):\\("^accepted_chars_sans_ws^
                    "\\)[ ]+\\("^accepted_chars^"\\)\\]\\)")

    let wikilinkanum_no_text_re = 
      Str.regexp ("\\(\\[\\(wiki\\|file\\|http\\):\\("^accepted_chars_sans_ws^"\\)\\]\\)")

    let open_pre_re = Pcre.regexp "^(<pre>|8<)\\s*$"
    let close_pre_re = Pcre.regexp "^(</pre>|8<)\\s*$"

    (* WikiML preprocessor: translate a list of lines into
       normal lines and blocks of PRE blocks that contain
       verbatim lines. *)
    let preprocess lines =
      let rec loop acc = function
          x::xs ->
            (match match_pcre_option open_pre_re x with
               Some m ->
                 begin
                   (* Handle <pre>..</pre> *)
                   let (after_pre,contents) =
                     take_while 
                       (fun x -> match_pcre_option close_pre_re x = None) xs in
                   let next = 
                     match after_pre with [] -> [] | _::next -> next in
                   loop (`NoWiki contents :: acc) next
                 end
             | None ->
                 loop (`Wiki x::acc) xs)
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
    let complete_todo sp id =
      [Html_util.complete_task_img_link sp id]
          
    let priority_arrow sp id up_or_down =
      let (title,arrow_img,dir) = 
        if up_or_down then 
          ("Raise priority!", "arrow_up.png", true)
        else 
          ("Lower priority!", "arrow_down.png", false) in
      let arrow_img =
        img ~alt:"Logo" ~src:(make_static_uri sp [arrow_img]) () in
      Eliompredefmod.Xhtml.a
        ~a:[a_title title] ~service:task_side_effect_mod_priority_action
        ~sp [arrow_img] (id,dir)


    let mod_priorities sp pri id =
      [priority_arrow sp id true; 
       priority_arrow sp id false]

    let todo_editor_link sp todo_id page =
      Html_util.todo_edit_img_link sp (ET_view page) todo_id
        
    let todo_modify_buttons sp page todo_id todo =
      let completed = todo.t_completed in
      span ~a:[a_class ["no_break"]]
        (if completed then
           []
         else 
           (todo_editor_link sp todo_id page @
              mod_priorities sp todo.t_priority todo_id @
              complete_todo sp todo_id))

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
            ~src:(make_static_uri sp ["external_link.png"]) () in
        if scheme = "wiki" || scheme = "" then
          let t = if text = "" then page else text in
          if Database.wiki_page_exists page then
            a wiki_view_page sp [pcdata t] (page,None)
          else 
            a ~a:[a_class ["missing_page"]] 
              ~service:wiki_view_page ~sp:sp [pcdata t] 
              (page,None)
        else (* External link *)
          let url = scheme^":"^page in
          let t = if text = "" then url else text in
          a (new_external_service 
               ~prefix:url
               ~path:[]
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
                ["todo_descr"; Html_util.priority_css_class todo.t_priority] in
            span 
              [todo_modify_buttons sp cur_page todo_id todo;
               span ~a:[a_class style] (Html_util.todo_descr_html todo.t_descr todo.t_owner)]
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
              (* If the WikiLink starts with a bang (!), don't create
                 a link but leave it as text. *)
              if m.[0] = '!' then
                let s = String.sub m 1 (String.length m - 1) in
                loop (add_html acc (pcdata s)) (charpos+(String.length m))
              else
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
            else if Str.string_match italic_re s charpos then
              let m = Str.matched_group 1 s in
              let inner_m = Str.matched_group 2 s in
              let h = em [pcdata inner_m] in
              loop (add_html acc h) (charpos+(String.length m))
            else if Str.string_match bold_re s charpos then
              let m = Str.matched_group 1 s in
              let inner_m = Str.matched_group 2 s in
              let h = strong [pcdata inner_m] in
              loop (add_html acc h) (charpos+(String.length m))
            else if Str.string_match code_re s charpos then
              let m = Str.matched_group 1 s in
              let inner_m = Str.matched_group 2 s in
              let h = code [pcdata inner_m] in
              loop (add_html acc h) (charpos+(String.length m))
            else if Str.string_match text_re s charpos then
              let m = Str.matched_group 1 s in
              loop (add_html acc (pcdata m)) (charpos+(String.length m))
            else
              begin
                let s = (String.sub s charpos ((String.length s)-charpos)) in
                add_html acc 
                  (Html_util.error 
                     ("WIKI SYNTAX ERROR on line: '"^s^"'"))
              end
        in
        List.rev (loop acc 0) in
      
      let rec pcre_first_match str pos =
        let rec loop = function
            (rex,f)::xs ->
              (try Some (Pcre.extract ~rex ~pos str, f) with Not_found -> loop xs)
          | [] -> None in
        loop in

      let rec loop acc = function
          ((`Wiki x)::xs) as lst ->

            let parse_list r = 
              (* Grab all lines starting with '*': *)
              let (after_bullets,bullets) =
                take_while is_list_or_empty lst in
              let list_items = 
                List.filter_map
                  (function 
                       (`Wiki e) as wl ->
                         if matches_pcre ws_or_empty_re e then
                           (* Empty line, ignore *)
                           None
                         else 
                           begin
                             match is_list wl with
                               Some r ->
                                 let n_stars = String.length r.(1) in
                                 Some (n_stars, parse_text [] r.(2))
                             | None ->
                                 assert false
                           end
                     | `NoWiki _ -> assert false) bullets in
              loop ((translate_list list_items)::acc) after_bullets in
            
            let wiki_pats =
              [(h3_re, (fun r -> loop ((h3 [pcdata r.(1)])::acc) xs));
               (h2_re, (fun r -> loop ((h2 [pcdata r.(1)])::acc) xs));
               (h1_re, (fun r -> loop ((h1 [pcdata r.(1)])::acc) xs));
               (ws_or_empty_re, (fun r -> loop acc xs));
               (list_re, (fun r -> parse_list r))] in

            begin
              match pcre_first_match x 0 wiki_pats with
                Some (res, action) -> action res
              | None ->
                  loop ((p (parse_text [] x))::acc) xs
            end
        | (`NoWiki x::xs) ->
            loop (pre [pcdata (String.concat "\n" x)]::acc) xs
        | [] -> List.rev acc in
      
      loop [] preprocessed_lines

  end

let load_wiki_page page_id =
  Database.load_wiki_page page_id >> Pcre.split ~rex:newline_re >> WikiML.preprocess

let wikiml_to_html sp (page_id:int) (page_name:string) todo_data =
  load_wiki_page page_id >> WikiML.parse_lines sp page_name todo_data

let todo_list_table_html sp cur_page todos =
  (* Which pages contain TODOs, mapping from todo_id -> {pages} *)
  let todo_in_pages =
    Database.todos_in_pages (List.map (fun todo -> todo.t_id) todos) in
  let todo_page_link todo =
    let descr = todo.t_descr in
    let page_links =
      let c = "wiki_pri_"^Html_util.string_of_priority todo.t_priority in
      Html_util.todo_page_links sp todo_in_pages 
        ~link_css_class:(Some c) (todo.t_id) in
    Html_util.todo_descr_html descr todo.t_owner @ page_links in

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
              Html_util.priority_css_class todo.t_priority in
          let row_class = 
            (row_pri_style::(if completed then ["todo_table_completed"] else [])) in
          (tr 
             (td ~a:[a_class row_class] [pcdata (string_of_int id)])
             [td ~a:[a_class row_class] (todo_page_link todo);
              td [(WikiML.todo_modify_buttons sp cur_page id todo)]]))
       todos)

let wiki_page_menu_html sp ~credentials ~undo_task_id page content =
  let edit_link = 
    [a ~service:wiki_edit_page ~sp:sp ~a:[a_accesskey '1'; a_class ["ak"]]
       [img ~alt:"Edit" ~src:(make_static_uri sp ["edit.png"]) ();
        pcdata "Edit page"] page] in
  let printable_link =
    [a ~service:wiki_view_page ~sp:sp
       ~a:[a_accesskey 'p'; a_class ["ak"]] [pcdata "Print"]
       (page, Some true)] in
  let current_user_id = Some credentials.user_id in
  let todo_list = 
    todo_list_table_html sp page 
      (Database.query_all_active_todos ~current_user_id ()) in
  Html_util.navbar_html sp ~credentials 
    ~wiki_page_links:(edit_link @ [pcdata " "] @  printable_link)
    ~undo_task_id
    ~todo_list_table:[todo_list] content

let wiki_page_contents_html sp ~undo_task_id page_id page_name todo_data ?(content=[]) () =
  wiki_page_menu_html sp ~undo_task_id page_name
    (content @ wikiml_to_html sp page_id page_name todo_data)

let view_page sp ~credentials page_id page_name ~printable =
  let undo_task_id = Session.any_complete_undos sp in
  
  let todos = Database.query_page_todos page_id in
  if printable <> None && Option.get printable = true then
    let page_content = wikiml_to_html sp page_id page_name todos in
    Html_util.html_stub sp page_content
  else 
    Html_util.html_stub sp
      (wiki_page_contents_html 
         sp 
         ~credentials 
         ~undo_task_id page_id page_name todos ())
      
let new_todo_re = 
  Str.regexp ("\\[todo \\("^WikiML. accepted_chars^"\\)\\]")

(* Parse existing todo's from the current to-be-saved wiki page and
   update the DB relation on what todos are on the page. 

   Todo descriptions are inspected and if they've been changed, modify
   them in the DB.  It's also possible to resurrect completed tasks
   here by removing the '(x)' part from a task description. *)
let check_new_and_removed_todos ~cur_user page_id lines =
  (* Figure out which TODOs are mentioned on the wiki page: *)
  let page_todos = 
    List.fold_left
      (fun acc -> function
           `Wiki line ->
             let rec loop acc n =
               try 
                 let beg = Str.search_forward WikiML.todo_re line n in
                 let m = 
                   try 
                     Some (Str.matched_group 2 line) 
                   with 
                     Not_found -> None in
                 loop ((Str.matched_group 1 line, m)::acc)
                   (beg+(String.length (Str.matched_group 0 line)))
               with 
                 Not_found -> acc in
             loop acc 0
         | `NoWiki _ -> acc) [] lines in

  (* Query todos that reside on this page.  Don't update DB for todos
     that did NOT change *)
  let todos_on_page = Database.query_page_todos page_id in

  let completed_re = Pcre.regexp "^\\s*\\(x\\) (.*)$" in
  let remove_ws_re = Pcre.regexp "^\\s*(.*)$" in
  (* Update todo descriptions & resurrect completed tasks *)
  List.iter
    (fun (id_s,descr) ->
       match descr with
         Some descr ->
           (match match_pcre_option completed_re descr with
              Some _ -> 
                (* Task has already been completed, do nothing: *)
                ()
            | None ->
                let id = int_of_string id_s in
                (* Update task description (if not empty): *)
                (match match_pcre_option remove_ws_re descr with
                   Some r ->
                     begin
                       try
                         let new_descr = r.(1) in
                         (* Only modify task description in DB if it's
                            changed from its previous value: *)
                         let todo = IMap.find id todos_on_page in
                         (* Resurrect completed task *)
                         if todo.t_completed then
                           Database.uncomplete_task cur_user.user_id id;
                         if todo.t_descr <> new_descr then
                           Database.update_todo_descr id new_descr
                       with 
                         Not_found -> 
                           (* Internal inconsistency, should not happen. *)
                           ()
                     end
                 | None -> ()))
       | None -> ())  page_todos;

  (* Update DB "todos in pages" relation *)
  Database.update_page_todos page_id 
    (List.map (fun e -> (int_of_string (fst e))) page_todos)


(* Insert new TODOs from the wiki ML into DB and replace [todo descr]
   by [todo:ID] *)
let convert_new_todo_items credentials page =
  let owner_id = credentials.user_id in
  List.map
    (function
         `Wiki line -> 
           `Wiki (Str.global_substitute new_todo_re
                    (fun str -> 
                       let descr = Str.matched_group 1 str in
                       let id = Database.new_todo page owner_id descr in
                       "[todo:"^id^" "^descr^"]") line)
       | (`NoWiki _) as x -> x)

(* Save page as a result of /edit?p=Page *)
let service_save_page_post =
  register_new_post_service
    ~fallback:wiki_view_page
    ~post_params:(string "value")
    (fun sp (page,_) value -> 
       Session.with_user_login sp
         (fun credentials sp ->
            (* Check if there are any new or removed [todo:#id] tags and
               updated DB page mappings accordingly: *)
            let wikitext = Pcre.split ~rex:newline_re value >> WikiML.preprocess in
            let page_id = Database.page_id_of_page_name page in
            check_new_and_removed_todos ~cur_user:credentials page_id wikitext;
            (* Convert [todo Description] items into [todo:ID] format, save
               descriptions to database and save the wiki page contents. *)
            let wiki_plaintext = 
              convert_new_todo_items credentials page_id wikitext >>
                WikiML.wikitext_of_preprocessed_lines in
            (* Log activity: *)
            Database.insert_save_page_activity credentials.user_id page_id;
            Database.save_wiki_page page_id wiki_plaintext;
            view_page sp ~credentials page_id page ~printable:(Some false)))

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

(* /edit?p=Page *)
let _ =
  register wiki_edit_page
    (fun sp page_name () -> 
       Session.with_user_login sp
         (fun credentials sp ->
            let (page_id,page_todos,preproc_wikitext) = 
              if Database.wiki_page_exists page_name then
                let page_id = Database.page_id_of_page_name page_name in
                let current_page_todos = Database.query_page_todos page_id in
                (page_id,
                 current_page_todos,
                 load_wiki_page page_id >> 
                   annotate_old_todo_items page_name current_page_todos)
              else
                begin
                  (Database.new_wiki_page page_name, IMap.empty, [])
                end in
            let wikitext = 
              String.concat "\n" (WikiML.wikitext_of_preprocessed_lines preproc_wikitext) in
            let f =
              post_form service_save_page_post sp
                (fun chain -> 
                   [(p [string_input ~input_type:`Submit ~value:"Save" (); 
                        Html_util.cancel_link wiki_view_page sp
                          (page_name,None);
                        br ();
                        textarea ~name:chain ~rows:30 ~cols:80 
                          ~value:(pcdata wikitext) ()])])
                (page_name,None) in
            Html_util.html_stub sp
              (wiki_page_contents_html sp ~credentials ~undo_task_id:None
                 page_id page_name page_todos ~content:[f] ())))

let view_wiki_page sp ~credentials (page_name,printable) =
  match Database.find_page_id page_name with
    Some page_id ->
      view_page sp ~credentials page_id page_name ~printable
  | None ->
      let f = 
        a wiki_edit_page sp [pcdata "Create new page"] page_name in
      Html_util.html_stub sp
        (wiki_page_menu_html sp ~credentials ~undo_task_id:None page_name [f])

(* /view?p=Page *)
let _ = 
  register wiki_view_page
    (fun sp params () ->
       Session.with_user_login sp
         (fun credentials sp -> 
            view_wiki_page sp ~credentials params))


(* /benchmark?test=empty,one_db *)
let _ =
  let gen_html sp = function
      "empty" ->
        (html 
           (head (title (pcdata "")) [])
           (body [p [pcdata "Empty page"]]))
    | "db1" ->
        (* TODO TODO add simple SQL query here *)
(*        ignore (Database.query_activities ());*)
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
                    (descr,None) in
                [p ([link (Option.get sr.sr_page_descr); br ()] @ 
                      html_of_headline sr.sr_headline)]
            | SR_todo -> assert false) search_results) in
  let gen_search_page sp ~credentials search_str =
    let search_results = Database.search_wikipage search_str in
    Html_util.html_stub sp
      (Html_util.navbar_html sp ~credentials ~undo_task_id:None
         ([h1 [pcdata "Search results"]] @ (render_results sp search_results))) in
    
  register search_page
    (fun sp search_str () ->
       Session.with_user_login sp
         (fun credentials sp ->
            gen_search_page sp credentials search_str))

