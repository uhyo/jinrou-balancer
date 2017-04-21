open Batteries
open Job
open Village
open Simulate
open Manage


(* 初期設定 *)
  (* 中 *)
let makereqs () =
  let results = [] in
  let results = match Random.int 10 < 8 with
    | true -> Job.Diviner :: results
    | false -> results in
  let results = match Random.int 10 < 5 with
    | true -> Job.Guard :: results
    | false -> results in
    results
let () =
  begin
    Random.self_init ();
    let () = Manage.run 10 makereqs in
      ()
  end
