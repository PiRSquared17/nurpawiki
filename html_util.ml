
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
    js_script ~a:[a_defer `Defer] ~uri:(make_static_uri sp [src]) () in
  let scripts  = 
    script "nurpawiki.js" :: (List.map script javascript) in
  return 
    (html 
       (head (title (pcdata "")) 
          ((scripts) @ [css_link ~a:[] ~uri:(make_uri ~service:(static_dir sp) ~sp ["style.css"]) ()]))
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
              string_input ~input_type:`Text ~name:chain
                ~value:"" ()]])] in

  let user_greeting = 
    [pcdata ("Howdy "^credentials.user_login^"!")] in


  let space = [pcdata " "] in
  [div ~a:[a_id "navbar"]
     ([home_link 
         [img ~alt:"Home" ~src:(make_static_uri sp ["home.png"]) ();
          pcdata "Home"]] @ 
        space @ [scheduler_link] @ 
         space @ [history_link] @ space @ wiki_page_links @
         [disconnect_box sp "Logout"] @
         search_input @ user_greeting @ [br ()] @ todo_list_table);
   div ~a:[a_id "content"]
     content]


let error text = 
  span ~a:[a_class ["error"]] [pcdata text]
