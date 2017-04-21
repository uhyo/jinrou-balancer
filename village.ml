open Batteries
open Job

module Village : sig
  type config = (Job.category, int) Map.t

  val initConfig : int -> config
end = struct

  type config = (Job.category, int) Map.t

  (* 割り振りを初期化 *)
  let initConfig n =
      (* 区切りを作る *)
    let seps =
      Enum.append
        (Enum.repeat ~times:n `Player)
        (Enum.repeat ~times:(List.length Job.categories - 1) `Sep) in
    let defaultwaff = Random.shuffle seps in
    let (mp, _) =
      Array.fold_left
        (fun (mp, teams) a ->
           match a with
             | `Player -> 
                 let mp' =
                   Map.modify_def
                     0
                     (List.hd teams)
                     (fun x -> x + 1)
                     mp in
                   (mp', teams)
             | `Sep ->
                 (mp, List.tl teams))
        (Map.empty, Job.categories)
        defaultwaff in
      List.fold_left
        (fun mp t ->
           Map.modify_def
             0
             t
             identity
             mp)
        mp
        Job.categories
end
