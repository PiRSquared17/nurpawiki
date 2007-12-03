
open XHTML.M

open Eliomsessions
open Eliomparameters
open Eliomservices
open Eliompredefmod.Xhtml

open Lwt

let make_static_uri sp name =
  make_uri (static_dir sp) sp name

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

        
  
