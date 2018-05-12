open Async
open Cohttp
open Cohttp_async
open Mutex


let get_value (url:string) (v:string ref) (m:Mutex.t) =
  let _ = Client.get (Uri.of_string ("http://"^url)) >>>
             (fun (r,b) ->
               upon (Cohttp_async.Body.to_string b)
                 (fun b ->
                   Mutex.lock m;
                   v := b;
                   Mutex.unlock m;
                   shutdown 0)) in
  Scheduler.go ()


let post_value (url:string) (key:string) (value:string) host =
  let uri =
    Uri.make ~scheme:"http" ~host:host ~port:8081 ~query:[key,[value]] () in
  let _ = Client.post (uri) >>> (fun _ -> shutdown 0) in
  Scheduler.go ()
  
    
                        

