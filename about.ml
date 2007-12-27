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

open Services
open Types

let about_page_html =
  [h1 [pcdata "About Nurpawiki"];
   p 
     [pcdata ("Nurpawiki v"^Version.version^
                " Copyright (c) 2007 Janne Hellsten <jjhellst@gmail.com>");
      br ();
      br ();
      pcdata "See the ";
      XHTML.M.a ~a:[a_href (uri_of_string "http://code.google.com/p/nurpawiki")]
        [pcdata "project homepage"];
      pcdata "."]]

let _ =
  register about_page
    (fun sp () () ->
       Session.with_guest_login sp
         (fun cur_user sp ->
            Html_util.html_stub sp
              (Html_util.navbar_html sp ~cur_user about_page_html)))
