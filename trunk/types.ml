
module OrdInt = struct type t = int let compare a b = compare a b end
module IMap = Map.Make (OrdInt)

type user = 
    {
      user_id : int;
      user_login : string;
    }

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

type et_cont = 
    ET_scheduler
  | ET_view of string

let et_cont_view_re = Pcre.regexp "^v_(.*)$"

let string_of_et_cont = function
    ET_scheduler -> "s"
  | ET_view src_page -> "v_"^src_page

let match_pcre_option rex s =
  try Some (Pcre.extract ~rex s) with Not_found -> None

let et_cont_of_string s = 
  if s = "s" then
    ET_scheduler
  else 
    begin
      match match_pcre_option et_cont_view_re s with
        None -> 
          raise (Failure "et_cont_of_string")
      | Some s ->
          ET_view s.(1)
    end

let int_of_activity_type = function
    AT_create_todo -> 1
  | AT_complete_todo -> 2
  | AT_work_on_todo -> 3
  | AT_create_page -> 4
  | AT_edit_page -> 5

let activity_type_of_int = function
    1 -> AT_create_todo
  | 2 -> AT_complete_todo
  | 3 -> AT_work_on_todo
  | 4 -> AT_create_page
  | 5 -> AT_edit_page
  | _ -> assert false

