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
  try
    let uri =
      Uri.make ~scheme:"http" ~host:url ~port:8081 ~path:path () in
    
    let b = Client.get (uri) >>=
              (fun (r,b) ->
                (Cohttp_lwt.Body.to_string b) >|=
                  (fun b ->
                    b)) in
    Lwt_main.run b
  with
  | _ -> ""


let post_value ((url:string),(key:string),(value:string),path) =
  try
    let uri =
      Uri.make ~scheme:"http" ~host:url ~port:8081 ~query:[key,[value]] ~path:path () in
    let p = Client.post (uri) >>= (fun (r,b) ->
        (Cohttp_lwt.Body.to_string b) >|= (fun b -> b)) in
    Lwt_main.run p
  with
  | _ -> ""
  
    
                        

