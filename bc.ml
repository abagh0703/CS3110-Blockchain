open Cohttp
open Cohttp_lwt_unix
open Lwt
open Mutex

(*

let f =
    let uri = Uri.make ~scheme:"http" ~host:"localhost" ~port:8081 ~query:["hi",[s]] () ~path:"chain" in
    (Client.get  (uri)) >>=
      (fun (r,b) ->

        (Cohttp_lwt.Body.to_string b) >|=
          (fun b ->
            print_endline b;
            v := b; b))
      *)


let get_value ((url:string),path) =
  let uri =
    Uri.make ~scheme:"http" ~host:url ~port:8081 ~path:path () in

  let b = Client.get (uri) >>=
             (fun (r,b) ->
               (Cohttp_lwt.Body.to_string b) >|=
                 (fun b ->
                   print_endline "hi";
                   b)) in
  Lwt_main.run b


let post_value ((url:string),(key:string),(value:string),path) =
  let uri =
    Uri.make ~scheme:"http" ~host:url ~port:8081 ~query:[key,[value]] ~path:path () in
  let p = Client.post (uri) >>= (fun (r,b) ->
      (Cohttp_lwt.Body.to_string b) >|= (fun b -> b)) in
  Lwt_main.run p

    (*
let uri = Uri.of_string "localhost:8080/hi?key=987&docID=123" in
(* let body1 = Cohttp_lwt_body.of_string "{\"inputs\":{\"key1\":\"val1\",\"key2\":\"val2\"}}" in *)
let body =
  Client.get (uri) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

in

let body2 = Lwt_main.run body in
let () =   print_endline ("Received body\n" ^ body2)

*)
