(*let () = print_endline "somethong"

let prog1 n =
  let result = ref 0 in
  let f i =
    for j = 1 to n do
      let v = !result in
      Thread.delay (Random.float 1.);
      result := v + i;
      Printf.printf "Value %d\n" !result;

      print_int !result
    done in
  ignore (Thread.create f 1);
  Thread.create f 2

let t2 = prog1 5

       
let () = Thread.join t2
 *)
  (*
let rec looper n =
  if n = 0 then () else looper (n-1)

let () = looper 5555555555

   *)



open Mutex
open Bs


let r = ref ["start"]
let m = Mutex.create ()

let point = Thread.create Bs.mk_server (r,m)

let rec f r m =
  Thread.delay 0.1;
  Mutex.lock m;
  let c = List.hd (!r) in
  Mutex.unlock m;
  print_endline (c);
  if c = "death" then () else f r m

let () = f r m
           
           
