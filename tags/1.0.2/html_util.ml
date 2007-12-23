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

open Types
open Services

let make_static_uri sp name =
  make_uri (static_dir sp) sp name

let disconnect_box sp s = 
  Eliompredefmod.Xhtml.a ~service:disconnect_page ~sp [pcdata s] ()

(* Use this as the basis for all pages.  Includes CSS etc. *)
let html_stub sp ?(javascript=[]) body_html =
  let script src = 
    js_script ~a:[a_defer `Defer] ~uri:(make_static_uri sp src) () in
  let scripts  = 
    script ["nurpawiki.js"] :: (List.map script javascript) in
  return 
    (html ~a:[a_xmlns `W3_org_1999_xhtml]
       (head 
          (title (pcdata "")) 
          ((scripts) @ 
             [css_link ~a:[] ~uri:(make_uri ~service:(static_dir sp) ~sp 
                                     ["style.css"]) ();
              css_link ~a:[] ~uri:(make_uri ~service:(static_dir sp) ~sp 
                                     ["jscalendar"; "calendar-blue2.css"]) ()]))
       (body 
          body_html))

let navbar_html sp ~credentials ~undo_task_id ?(wiki_page_links=[]) ?(todo_list_table=[]) content =
  let home_link link_text =
    a ~service:wiki_view_page 
      ~a:[a_accesskey 'h'; a_class ["ak"]] ~sp:sp link_text 
      ("WikiStart", None) in
  let scheduler_link =
    a ~service:scheduler_page
      ~a:[a_accesskey 'r'; a_class ["ak"]] ~sp:sp 
      [img ~alt:"Scheduler" ~src:(make_static_uri sp ["calendar.png"]) ();
       pcdata "Scheduler"] () in
  let history_link =
    a ~service:history_page
      ~a:[a_accesskey 'r'; a_class ["ak"]] ~sp:sp 
      [img ~alt:"History" ~src:(make_static_uri sp ["home.png"]) ();
       pcdata "History"] () in

  let search_input =
    [get_form search_page sp
       (fun (chain : ([`One of string] Eliomparameters.param_name)) -> 
          [p [string_input ~input_type:`Submit ~value:"Search" ();
              string_input ~input_type:`Text ~name:chain ()]])] in

  let user_greeting = 
    [pcdata ("Howdy "^credentials.user_login^"!")] in

  let edit_users_link = 
    if Privileges.can_view_users credentials then
      [a ~service:user_admin_page ~sp [pcdata "Edit Users"] ()]
    else 
      [] in

  [div ~a:[a_id "topbar"]
     [table ~a:[a_class ["top_menu_size"]]
        (tr
           (td ~a:[a_class ["top_menu_left_align"]]
              [table
                 (tr (td [home_link 
                            [img ~alt:"Home" ~src:(make_static_uri sp ["home.png"]) ();
                             pcdata "Home"]])
                    [td [scheduler_link];
                     td [history_link];
                     td wiki_page_links])
                 []])
           [td ~a:[a_class ["top_menu_right_align"]]
              ([a ~service:about_page ~sp [pcdata "About"] ()] @
                 [pcdata " "] @
                 [a ~service:edit_user_page ~sp [pcdata "My Preferences"]
                    (None,credentials.user_login)] @
                 [pcdata " "] @
                 edit_users_link @
                 [pcdata " "] @
                 [disconnect_box sp "Logout"])]) []]] 
  @
    (match undo_task_id with
       None -> []
     | Some id ->
         [div ~a:[a_id "top_action_bar"]
           [a ~a:[a_class ["undo_link"]] ~service:task_side_effect_undo_complete_action 
              ~sp [pcdata "Undo Complete Task!"] id]])
  @
    [div ~a:[a_id "navbar"]
       (user_greeting @ [br ()] @ search_input @ todo_list_table);
     div ~a:[a_id "content"]
       content]

let error text = 
  span ~a:[a_class ["error"]] [pcdata text]

let error_page sp msg =
  html_stub sp 
    [p [error msg]]


let string_of_priority = function
    3 -> "lo"
  | 2 -> "med"
  | 1 -> "hi"
  | _ -> "INTERNAL ERROR: PRIORITY OUT OF RANGE"

let priority_css_class p =
  "todo_pri_"^(string_of_priority p)

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

let todo_edit_img_link sp page_cont task_id =
  [a ~a:[a_title "Edit"] ~service:edit_todo_get_page ~sp:sp
     [img ~alt:"Edit" 
        ~src:(make_static_uri sp ["edit_small.png"]) ()]
     (page_cont, Some task_id)]

let complete_task_img_link sp task_id =
  let img_html = 
    [img ~alt:"Mark complete" 
       ~src:(make_static_uri sp ["mark_complete.png"]) ()] in
  a ~service:task_side_effect_complete_action
    ~a:[a_title "Mark as completed!"] ~sp img_html task_id

let todo_descr_html descr owner = 
  match owner with
    None -> [pcdata descr]
  | Some o ->
      [pcdata descr; span ~a:[a_class ["todo_owner"]] [pcdata (" ["^o.owner_login^"] ")]]


(* Use to create a "cancel" button for user submits *)
let cancel_link service sp params =
  a ~a:[a_class ["cancel_edit"]] ~service:service ~sp:sp 
    [pcdata "Cancel"] 
    params
