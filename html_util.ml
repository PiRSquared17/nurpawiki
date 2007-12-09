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
  Eliompredefmod.Xhtml.post_form ~service:disconnect_action ~sp
    (fun _ -> [p [Eliompredefmod.Xhtml.string_input
                    ~input_type:`Submit ~value:s ()]]) ()


(* Use this as the basis for all pages.  Includes CSS etc. *)
let html_stub sp ?(javascript=[]) body_html =
  let script src = 
    js_script ~a:[a_defer `Defer] ~uri:(make_static_uri sp src) () in
  let scripts  = 
    script ["nurpawiki.js"] :: (List.map script javascript) in
  return 
    (html 
       (head (title (pcdata "")) 
          ((scripts) @ 
             [css_link ~a:[] ~uri:(make_uri ~service:(static_dir sp) ~sp 
                                     ["style.css"]) ();
              css_link ~a:[] ~uri:(make_uri ~service:(static_dir sp) ~sp 
                                     ["jscalendar"; "calendar-win2k-1.css"]) ()]))
       (body 
          body_html))

let navbar_html sp ~credentials ?(wiki_page_links=[]) ?(todo_list_table=[]) content =
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
           [td (*~a:[a_class ["top_menu_right_align"]]*)
              [table 
                 (tr 
                    (td search_input)
                    [td [disconnect_box sp "Logout"]])
                 []];
             td ~a:[a_class ["top_menu_right_align"]]
              [table 
                 (tr 
                    (td [a ~service:edit_user_page ~sp [pcdata "My Preferences"] 
                           (None,credentials.user_login)])
                    [td [a ~service:user_admin_page ~sp [pcdata "Edit Users"] ()]])
                 []]]) []];
   div ~a:[a_id "navbar"]
     (user_greeting @ [br ()] @ todo_list_table);
   div ~a:[a_id "content"]
     content]


let error text = 
  span ~a:[a_class ["error"]] [pcdata text]
