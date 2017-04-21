open Batteries
open Job
open Village
open Simulate

module Manage : sig

  val run : int -> (unit -> Job.t list) -> unit

end = struct

  let random_choose def l =
    let n = List.length l in
      if n > 0 then
        begin
          let r = Random.int n in
            List.at l r
        end
      else
        def

  (* 1 periodで何回村を回すか *)
  let period_num = 100
  (* 何回runするか *)
  let run_num = 100

  let period config makereqs =
    (* 勝率のカウント *)
    let init_count =
      List.fold_left
        (fun mp jd ->
           Map.add jd 0 mp)
        Map.empty
        Simulate.judgements in
    let rec one count = function
      | 0 -> count
      | remains -> 
          let simm = Simulate.initSimulation config (makereqs ()) in
          let j = Simulate.simulate simm in
          let count' =
            Map.modify
              j
              (fun x -> x + 1)
              count in
            one count' (remains - 1) in
      one init_count period_num

  (* configから *)
  let config_add t mp =
    Map.modify
      t
      (fun x -> x + 1)
      mp
  let config_del t mp =
    if Map.find t mp > 0 then
      Map.modify
        t
        (fun x -> x - 1)
        mp
    else
      (* 適当に減らす *)
      let (m1, _) = Map.partition
                       (fun _ v -> v > 0)
                       mp in
        if Map.cardinal m1 = 0 then
          failwith "Hey!"
        else
          let (t, _) = Map.choose m1 in
            Map.modify
              t
              (fun x -> x - 1)
              mp


  (* 勝率を受け取ってconfigを調整 *)
  let rec adjust wins config =
    if Map.find Simulate.JFox wins > 0.1 then
      (* 狐が勝ちすぎ *)
      begin
        let addu = random_choose Job.TVillage Job.categories in
          config_del Job.TFox @@
          config_add addu @@
          config
      end
    else if Map.find Simulate.JVillager wins < 0.45 then
      (* 村勝ちが少ない *)
      begin
        let delu = random_choose Job.TVillage @@ List.filter
                                      (fun c ->
                                         c <> Job.TVillage &&
                                         Map.find c config > 0)
                                      Job.categories in
          config_del delu @@
          config_add Job.TVillage @@
          config
      end
    else if Map.find Simulate.JWerewolf wins < 0.45 then
      (* 狼勝ちが少ない *)
      begin
        let delu = random_choose Job.TVillage @@ List.filter
                                      (fun c -> Map.find c config > 0)
                                      [Job.TVillage; Job.TMad; Job.TFox] in
        let addu = random_choose Job.TWerewolf [Job.TWerewolf; Job.TMad] in
          config_del delu @@
          config_add addu @@
          config
      end
    else
        config


  (* n人村に対して動作 *)
  let run n makereqs =
    let rec one config = function
      | 0 -> config
      | remains ->
          let wins = period config makereqs in
          (* 勝率に変換 *)
          let wins' =
            Map.map
              (fun cnt -> float_of_int cnt /. float_of_int period_num)
              wins in
            (* Log *)
            Printf.printf "----- current config -----\n";
            Map.iter
              (fun c v ->
                 Printf.printf "%s: %d\n" (Job.category_string c) v)
              config;
            Map.iter
              (fun j v ->
                 Printf.printf "%s: %f\n" (Simulate.judgement_string j) v)
              wins';
          let config' = adjust wins' config in
            one config' (remains-1) in
    let init_config = Village.initConfig n in
    let config = one init_config run_num in
      Printf.printf "===== result =====\n";
      Map.iter
        (fun c v ->
           Printf.printf "%s: %d\n" (Job.category_string c) v)
        config;
      ()
end
