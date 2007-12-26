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

let revision_table sp page_descr =
  let revisions = Database.query_page_revisions page_descr in

  let page_link descr (rev:int) = 
    a ~sp ~service:wiki_view_page [pcdata ("Revision "^(string_of_int rev))]
      (descr, (None, Some rev)) in

  let rows =
    List.map 
      (fun r ->
         tr (td [page_link page_descr r.pr_revision])
           [td [pcdata r.pr_created];
            td [pcdata (Option.default "" r.pr_owner_login)]])
      revisions in

  [table
     (tr (th [pcdata "Revision"]) [th [pcdata "When"]; th [pcdata "Changed by"]])
     rows]


let view_page_revisions sp page_descr =
  Session.with_user_login sp
    (fun credentials sp -> 
       Html_util.html_stub sp
         (Html_util.navbar_html sp ~credentials
            (h1 [pcdata (page_descr ^ " Revisions")] :: revision_table sp page_descr)))

(* /page_revisions?page_id=<id> *)
let _ =
  register page_revisions_page
    (fun sp page_descr () ->
       view_page_revisions sp page_descr)
