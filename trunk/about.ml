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
     [pcdata ("Nurpawiki v"^Config.version^
                " Copyright (c) 2007 Janne Hellsten <jjhellst@gmail.com>")]]

let _ =
  register about_page
    (fun sp () () ->
       Session.with_user_login sp
         (fun credentials sp ->
            Html_util.html_stub sp ~javascript:[]
              (Html_util.navbar_html sp ~credentials
                 about_page_html)))
