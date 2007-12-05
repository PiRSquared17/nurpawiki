
open XHTML.M
open Eliomsessions
open Eliomparameters
open Eliomservices
open Eliompredefmod.Xhtml

open Types

open Lwt

(* As the handler is very simple, we register it now: *)
let disconnect_action = 
  Eliompredefmod.Actions.register_new_post_coservice'
    ~post_params:Eliomparameters.unit 
    (fun sp () () -> 
      Eliomsessions.close_session  ~sp () >>= fun () -> 
      Lwt.return [])


let wiki_view_page = 
  new_service ["view"] ((string "p")
                        ** (opt (bool "printable"))) ()

let wiki_edit_page = new_service ["edit"] (string "p") ()

let scheduler_page = new_service ["scheduler"] unit ()

let edit_todo_get_page = new_service ["edit_todo"] 
  ((Eliomparameters.user_type 
      et_cont_of_string string_of_et_cont "src_service") **
     (opt (int "tid"))) ()

let edit_todo_page = 
  new_post_service
    ~fallback:edit_todo_get_page 
    ~post_params:any ()

let history_page = new_service ["history"] unit ()

let search_page = new_service ["search"] (string "q") ()

let benchmark_page = new_service ["benchmark"] (string "test") ()

let user_editor_page = new_service ["user_editor"] unit ()

