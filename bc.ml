open Cohttp
open Cohttp_lwt_unix
open Lwt
open Mutex

(* POST example: https://github.com/mirage/ocaml-cohttp *)

let get_value ((url:string),path) =
  let uri =
    Uri.make ~scheme:"http" ~host:url ~port:8081 ~path:path () in

  let b = Client.get (uri) >>=
             (fun (r,b) ->
               (Cohttp_lwt.Body.to_string b) >|=
                 (fun b ->
                   b)) in
  Lwt_main.run b


let post_value ((url:string),(key:string),(value:string),path) =
  let uri =
    Uri.make ~scheme:"http" ~host:url ~port:8081 ~query:[key,[value]] ~path:path () in
  let p = Client.post (uri) >>= (fun (r,b) ->
      (Cohttp_lwt.Body.to_string b) >|= (fun b -> b)) in
  Lwt_main.run p
