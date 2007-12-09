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

module Psql = Postgresql
module P = Printf

let match_pcre_option rex s =
  try Some (Pcre.extract ~rex s) with Not_found -> None

(* TODO no need to extract here *)
let matches_pcre rex s =
  try ignore (Pcre.extract ~rex s); true with Not_found -> false

let (>>) f g = g f

let newline_re = Pcre.regexp "\n"

let iso_date_re = Pcre.regexp "([0-9]+)-([0-9]+)-([0-9]+)"

let date_of_string s = 
  match match_pcre_option iso_date_re s with
    Some r ->
      let year = int_of_string r.(1) in
      let month = int_of_string r.(2) in
      let day = int_of_string r.(3) in
      Date.make year month day
  | None -> assert false


let iso_date_time_re = Pcre.regexp "([0-9]+)-([0-9]+)-([0-9]+) .*"

let date_of_date_time_string s =
  match (match_pcre_option iso_date_time_re s,
         match_pcre_option iso_date_re s) with
    (Some r,_)|(_,Some r) ->
      let year = int_of_string r.(1) in
      let month = int_of_string r.(2) in
      let day = int_of_string r.(3) in
      Date.make year month day
  | _ -> 
      Messages.errlog ("invalid date '"^s^"'");
      assert false

let task_side_effect_complete sp task_id () =
  (* TODO error handling! (should not break anything though even on
     errors) *)
  ignore 
    (Session.action_with_user_login sp 
       (fun user ->
          Database.complete_task user.user_id task_id));
  return []


let task_side_effect_mod_priority sp (task_id, dir) () =
  if dir = false then 
    Database.down_task_priority task_id
  else 
    Database.up_task_priority task_id;
  return []


let task_side_effect_complete_action = 
  Eliomservices.new_coservice' ~get_params:(Eliomparameters.int "task_id") ()

let task_side_effect_mod_priority_action = 
  Eliomservices.new_coservice' ~get_params:((Eliomparameters.int "task_id") **
                                              Eliomparameters.bool "dir") ()

let () =
  Eliompredefmod.Actions.register 
    ~service:task_side_effect_complete_action task_side_effect_complete;
  Eliompredefmod.Actions.register 
    ~service:task_side_effect_mod_priority_action task_side_effect_mod_priority

let make_static_uri = Html_util.make_static_uri

let todo_edit_img_link sp page_cont task_id =
  [a ~a:[a_title "Edit"] ~service:edit_todo_get_page ~sp:sp
     [img ~alt:"Edit" 
        ~src:(make_static_uri sp ["edit_small.png"]) ()]
     (page_cont, Some task_id)]

let complete_task_img_link sp task_id =
  let img_html = 
    [img ~alt:"Mark complete" 
       ~src:(make_static_uri sp ["mark_complete.png"]) ()] in
  Eliompredefmod.Xhtml.a ~service:task_side_effect_complete_action
    ~a:[a_title "Mark as completed!"] ~sp img_html task_id

let todo_descr_html descr owner = 
  match owner with
    None -> [pcdata descr]
  | Some o ->
      [pcdata descr; span ~a:[a_class ["todo_owner"]] [pcdata (" ["^o.owner_login^"] ")]]


(* Deal with Wiki markup *)
module WikiML =
  struct
    type preproc_line = 
        [ `Wiki of string
        | `NoWiki of string list
        ]

    let ws_or_empty_re = Pcre.regexp "^\\([ \t\n\r]*\\)$"

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
      Str.regexp ("\\[todo:\\([0-9]+\\)\\( "^accepted_chars^"\\)?\\]")

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
    let complete_todo sp id =
      [complete_task_img_link sp id]
          
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
      todo_edit_img_link sp (ET_view page) todo_id
        
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
                ["todo_descr"; priority_css_class todo.t_priority] in
            span 
              [todo_modify_buttons sp cur_page todo_id todo;
               span ~a:[a_class style] (todo_descr_html todo.t_descr todo.t_owner)]
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
               (list_re, (fun r -> parse_list r));
               (ws_or_empty_re, (fun r -> loop acc xs))] in

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
      (page.p_descr,None) in
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
    Database.todos_in_pages (List.map (fun todo -> todo.t_id) todos) in
  let todo_page_link todo =
    let descr = todo.t_descr in
    let page_links =
      let c = "wiki_pri_"^WikiML.string_of_priority todo.t_priority in
      todo_page_links sp todo_in_pages ~link_css_class:(Some c) (todo.t_id) in
    todo_descr_html descr todo.t_owner @ page_links in

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

let wiki_page_menu_html sp ~credentials page content =
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
    ~todo_list_table:[todo_list] content

let wiki_page_contents_html sp page_id page_name todo_data ?(content=[]) () =
  wiki_page_menu_html sp page_name
    (content @ wikiml_to_html sp page_id page_name todo_data)

let view_page sp ~credentials page_id page_name ~printable =
  let todos = Database.query_page_todos page_id in
  if printable <> None && Option.get printable = true then
    let page_content = wikiml_to_html sp page_id page_name todos in
    Html_util.html_stub sp page_content
  else 
    Html_util.html_stub sp
      (wiki_page_contents_html sp ~credentials page_id page_name todos ())
      
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

(* Use to create a "cancel" button for user submits *)
let cancel_link service sp params =
  a ~a:[a_class ["cancel_edit"]] ~service:service ~sp:sp 
    [pcdata "Cancel"] 
    params

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
                        cancel_link wiki_view_page sp
                          (page_name,None);
                        br ();
                        textarea ~name:chain ~rows:30 ~cols:80 
                          ~value:(pcdata wikitext) ()])])
                (page_name,None) in
            Html_util.html_stub sp
              (wiki_page_contents_html sp ~credentials page_id page_name page_todos 
                 ~content:[f] ())))

let view_wiki_page sp ~credentials (page_name,printable) =
  match Database.find_page_id page_name with
    Some page_id ->
      view_page sp ~credentials page_id page_name ~printable
  | None ->
      let f = 
        a wiki_edit_page sp [pcdata "Create new page"] page_name in
      Html_util.html_stub sp
        (wiki_page_menu_html sp ~credentials page_name [f])

(* /view?p=Page *)
let _ = 
  register wiki_view_page
    (fun sp (page_name,printable) () ->
       Session.with_user_login sp
         (fun credentials sp -> view_wiki_page sp ~credentials (page_name,printable)))


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

  let scheduler_page_internal sp ~credentials =
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
    
    let todo_table_html sp todos =
      let todo_in_pages =
        Database.todos_in_pages (List.map (fun (_,todo) -> todo.t_id) todos) in

      let prev_heading = ref "" in
      let todo_rows = 
        List.map
          (fun (heading,todo) ->
             let todo_id_s = string_of_int todo.t_id in
             let heading_row =
               if !prev_heading <> heading then
                 begin
                   prev_heading := heading;
                   [tr (td ~a:[a_class ["rm_table_heading"]] [pcdata heading]) []]
                 end
               else
                 [] in
             let pri_style = WikiML.priority_css_class todo.t_priority in
             let todo_row =
               tr
                 (td ~a:[a_class ["rm_edit"]]
                    (todo_edit_img_link sp ET_scheduler todo.t_id))
                 [td [raw_checkbox ~name:("t-"^ todo_id_s) ~value:"0" ()];
                  td [complete_task_img_link sp todo.t_id];
                  (td ~a:[a_class ["no_break"; pri_style]] 
                     [pcdata (prettify_activation_date todo.t_activation_date)]);
                  td ~a:[a_class [pri_style]] 
                    (todo_descr_html todo.t_descr todo.t_owner @ wiki_page_links sp todo_in_pages todo)] in
             heading_row @ [todo_row]) todos in
      List.flatten todo_rows in

    let todo_section sp todos =
      (todo_table_html sp todos) in

    let current_user_id = Some credentials.user_id in
    let upcoming_pending =
      Database.query_upcoming_todos ~current_user_id (None,None) in
    let upcoming_tomorrow =
      Database.query_upcoming_todos ~current_user_id (None,Some 1) in
    let upcoming_todos_7_days =
      Database.query_upcoming_todos ~current_user_id (Some 1,Some 7) in
    let upcoming_todos_14_days =
      Database.query_upcoming_todos ~current_user_id (Some 7, Some 14) in
    let upcoming_all = 
      Database.query_upcoming_todos ~current_user_id (Some 14, None) in

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
    let table () = 
      [p [raw_input ~input_type:`Submit ~value:"Mass edit" ()];
       table
         (tr (th []) [th []; th []; th [pcdata "Activates on"]; th [pcdata "Todo"]])
         (todo_section sp merged_todos)] in

    let table' = 
      post_form edit_todo_page sp table (ET_scheduler, None) in
    
    Html_util.html_stub sp
      (Html_util.navbar_html sp ~credentials
         ([h1 [pcdata "Road ahead"]] @ [table'])) in
  Session.with_user_login sp
    (fun credentials sp -> 
       scheduler_page_internal sp credentials)
  

let render_edit_todo_cont_page sp ~credentials = function
    ET_scheduler -> 
      view_scheduler_page sp
  | ET_view wiki_page ->
      view_wiki_page sp ~credentials (wiki_page,None)

let scheduler_page_discard_todo_id = 
  register_new_service
    ~path:["scheduler"] 
    ~get_params:((Eliomparameters.user_type 
                   et_cont_of_string string_of_et_cont "src_service"))
    (fun sp (src_page_cont) () -> 
       Session.with_user_login sp
         (fun credentials sp ->
            render_edit_todo_cont_page sp ~credentials src_page_cont))
         
(* Save page as a result of /edit_todo?todo_id=ID *)
let service_save_todo_item =
  register_new_post_service
    ~fallback:scheduler_page_discard_todo_id
    ~post_params:(list "todos"
                    ((int "todo_id") **
                       (string "activation_date") ** 
                       (string "descr") ** 
                       (int "owner_id")))
    (fun sp src_page_cont todos ->
     Session.with_user_login sp
       (fun credentials sp ->
          List.iter
            (fun (todo_id,(activation_date,(descr,owner_id))) ->
               Database.update_todo_descr todo_id descr;
               Database.update_todo_owner_id todo_id owner_id;
               Database.update_todo_activation_date todo_id activation_date)
            todos;
          render_edit_todo_cont_page sp ~credentials src_page_cont))

let rec render_todo_editor sp ~credentials (src_page_cont, todos_to_edit) =
  let todos_str = String.concat "," (List.map string_of_int todos_to_edit) in
  let todos = Database.query_todos_by_ids todos_to_edit in
  
  let f =
    let todo_in_pages =
      Database.todos_in_pages (List.map (fun todo -> todo.t_id) todos) in

    let cancel_page cont = 
      match cont with
        ET_scheduler -> cancel_link scheduler_page sp ()
      | ET_view wiki -> cancel_link wiki_view_page sp (wiki,None) in

    let owner_selection chain todo =
      (* TODO don't do this per each todo *)
      let users = Database.query_users () in

      let match_owner u = function
          Some o -> o.owner_id = u.user_id
        | None -> false in
        
      let options = 
        List.map 
          (fun u -> 
             Option ([], u.user_id, Some (pcdata u.user_login),
                     match_owner u todo.t_owner))
          users in
      int_select ~name:chain (List.hd options) (List.tl options) in

    let todo_descr chain v =
      string_input ~input_type:`Text ~name:chain ~value:v () in

    (* See nurpawiki_calendar.js for JavaScript calendar binding
       details. *)
    let create_listform f = 
      [table 
         (tr (th [pcdata "ID"]) 
            [th [pcdata "Description"]; th [pcdata "Activates on"]])
         (f.it
            (fun (tv_id,(tv_act_date,(tv_descr,tv_owner_id))) todo ->
               let pri_style = WikiML.priority_css_class todo.t_priority in
               [tr ~a:[a_class [pri_style]]
                  (td [pcdata (string_of_int todo.t_id)])
                  [td (todo_descr tv_descr todo.t_descr :: 
                         wiki_page_links sp todo_in_pages todo);
                   td ~a:[a_class ["no_break"]] 
                     [string_input ~a:[a_id ("calendar_"^(string_of_int todo.t_id))]
                        ~input_type:`Text ~name:tv_act_date 
                        ~value:todo.t_activation_date ();
                      button ~a:[a_id ("button_"^(string_of_int todo.t_id)); a_name "cal_trigger"] 
                        ~button_type:`Button [pcdata "..."]];
                   td [owner_selection tv_owner_id todo;
                       int_input ~name:tv_id ~input_type:`Hidden ~value:todo.t_id ()]]])
            todos
            [tr (td [string_input ~input_type:`Submit ~value:"Save" ();
                     cancel_page src_page_cont]) []])] in

    post_form ~service:service_save_todo_item ~sp create_listform src_page_cont in

  let heading = [pcdata ("Edit TODOs "^todos_str)] in
  let help_str = 
    pcdata "NOTE: Below activation date will be assigned for all the items" in

  let calendar_js = 
    [["jscalendar"; "calendar.js"];
     ["jscalendar"; "lang"; "calendar-en.js"];
     ["jscalendar"; "calendar-setup.js"];
     ["nurpawiki_calendar.js"]] in


  Html_util.html_stub sp ~javascript:calendar_js
    (Html_util.navbar_html sp ~credentials
       ((h1 heading)::[help_str; br(); f]))

let error_page sp msg =
  Html_util.html_stub sp [h1 [pcdata ("ERROR: "^msg)]]

let render_todo_get_page sp (src_page_cont, todo) = 
  match todo with
    Some todo_id ->
      render_todo_editor sp (src_page_cont, [todo_id])
  | None ->
      (* Bogus input as we didn't get any todos to edit..  But let's
         just take the user back to where he came from rather than
         issueing an error message. *)
      render_edit_todo_cont_page sp src_page_cont
        
let _ =
  register edit_todo_get_page
    (fun sp get_params () ->
       Session.with_user_login sp
         (fun credentials sp ->
            render_todo_get_page sp ~credentials get_params))

let todo_id_re = Pcre.regexp "^t-([0-9]+)$"

let parse_todo_ids todo_ids = 
  try
    List.map
      (fun (todo_id_str,b) ->
         match match_pcre_option todo_id_re todo_id_str with
           Some r ->
             int_of_string r.(1)
         | None ->
             raise Not_found) todo_ids
  with
    Not_found ->
      []
        

let _ =
  register edit_todo_page
    (fun sp (src_page_cont, single_tid) (todo_ids : (string * string) list) ->
       Session.with_user_login sp
         (fun credentials sp ->
            if todo_ids = [] then
              render_todo_get_page sp ~credentials 
                (src_page_cont, single_tid)
            else 
              render_todo_editor sp ~credentials
                (src_page_cont, (parse_todo_ids todo_ids))))


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
  | AT_uncomplete_todo -> "Resurrected"

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
      ag_resurrected_todos : (string * page list) list;
      ag_edited_pages : page list;
    }

let empty_act_group = 
  {
      ag_created_todos = [];
      ag_completed_todos = [];
      ag_resurrected_todos = [];
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
          | AT_uncomplete_todo ->
              (match a.a_todo_descr with
                 Some descr ->
                   let e = 
                     (Option.get a.a_todo_descr, pages)::ag.ag_resurrected_todos in
                   { ag with ag_resurrected_todos = e }
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

let view_history_page sp ~credentials =

  let activity = Database.query_past_activity () in
  let activity_in_pages = Database.query_activity_in_pages () in

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

                  let created_todos = 
                    todo_html "Created" e.ag_created_todos in
                  let completed_todos = 
                    todo_html "Completed" e.ag_completed_todos in
                  let resurrected_todos = 
                    todo_html "Resurrected" e.ag_resurrected_todos in
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
                  (pages_html @ created_todos @ completed_todos @ resurrected_todos @
                     [tr (td ~a:[a_class ["no_break"; "h_date_heading"]] date_text) []] @ lst_acc,
                   prettified_date))
               activity_groups ([],"")))) in
  Html_util.html_stub sp
    (Html_util.navbar_html sp ~credentials
       ([h1 [pcdata "Blast from the past"]] @ [act_table]))

(* /history *)
let _ =
  register history_page
    (fun sp todo_id () ->
       Session.with_user_login sp
         (fun credentials sp ->
            view_history_page sp ~credentials))

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
      (Html_util.navbar_html sp ~credentials
         ([h1 [pcdata "Search results"]] @ (render_results sp search_results))) in
    
  register search_page
    (fun sp search_str () ->
       Session.with_user_login sp
         (fun credentials sp ->
            gen_search_page sp credentials search_str))

