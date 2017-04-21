open Batteries
open Job
open Village
open Simulate
open Manage

let () =
  begin
    Random.self_init ();
    let _ = Manage.run 17 in
      ()
  end
