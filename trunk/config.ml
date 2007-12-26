(* Copyright (c) 2007 Janne Hellsten <jjhellst@gmail.com> *)

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

module P = Printf

open Eliomsessions
open Simplexmlparser

type db_config = 
    {
      db_name : string;
      db_user : string;
      db_port : string option;
      db_pass : string option;
    }

type site_config =
    {
      sitecfg_allow_ro_guests : bool;
    }

let get_attr_opt attr attrs = 
  try Some (List.assoc attr attrs)
  with Not_found -> None

let get_attr_with_err e attr attrs =
  try (List.assoc attr attrs)
  with Not_found -> 
    raise (Extensions.Error_in_config_file 
             ("Expecting "^e^"."^attr^" attribute in Nurpawiki config"))

let dbcfg =
  let rec find_dbcfg = function
      (Element ("database", attrs, _)::_) ->
        let dbname = get_attr_with_err "database" "name" attrs in
        let dbuser = get_attr_with_err "database" "user" attrs in
        let dbport = get_attr_opt "port" attrs in
        let dbpass = get_attr_opt "password" attrs in
        (dbname,dbuser,dbport,dbpass)
    | x::xs -> 
        find_dbcfg xs
    | [] -> 
        raise (Extensions.Error_in_config_file ("Couldn't find database element from config")) in
  let (dbname,dbuser,dbport,dbpass) = find_dbcfg (get_config ()) in
  { 
    db_name = dbname;
    db_user = dbuser;
    db_port = dbport;
    db_pass = dbpass;
  }

let site_config =
  let rec find_site_cfg = function
      (Element ("nurpawiki", attrs, _))::_ ->
        let allow_ro_guests = 
          (match get_attr_opt "allow_read_only_guests" attrs with
             Some s -> s = "yes"
           | None -> false) in
        allow_ro_guests
    | (Element (x,_,_))::xs -> 
        Messages.errlog x;
        find_site_cfg xs 
    | _ -> false in
  let allow_ro_guests = find_site_cfg (get_config ()) in
  Messages.warning (P.sprintf "read-only guests allowed %b" allow_ro_guests);
  {
    sitecfg_allow_ro_guests = allow_ro_guests;
  }

