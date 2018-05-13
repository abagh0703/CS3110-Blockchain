(* This file is in the public domain *)
open Base
open Cohttp_async
open Mutex
open Async_kernel
open Crypto
open Yojson
open Bs
open Core

(*this is for the routing table and the code is very similar to the following
 *site: https://dev.realworldocaml.org/records.html. We can talk about how to modify
 *this.
*)

type routing_info = {
  service_name: string;
  port: int;
  protocol: string
}
type 'a with_line_num = { item : 'a; line_num : int; }

(* rtmp is the routing table maintenence protocol, tcpmux is the tcpmux
 * tcp port service multiplexer. this will only work only when the access points
 * are up
*)
let parse_lines parse file_contents =
  let lines = String.split ~on:'\n' file_contents in
  List.mapi lines ~f:(fun line_num line ->
      { item = parse line;
        line_num = line_num + 1;
      })
