open Batteries
open Base
open Job
open Village

module Simulate : sig
  type player = int * Job.t
  type judgement =
    | JVillager
    | JWerewolf
    | JFox
    | JDraw
  type game = {
    players: player list
  }

  val judgement_string : judgement -> string

  val judgements : judgement list

  val initSimulation : Village.config -> Job.t list -> game
  val simulate : game -> judgement
end  = struct
  type player = int * Job.t
  type judgement =
    | JVillager
    | JWerewolf
    | JFox
    | JDraw

  type game = {
    players: player list
  }

  let judgements = [
    JVillager;
    JWerewolf;
    JFox;
    JDraw
  ]

  let judgement_string = function
    | JVillager -> "JVillager"
    | JWerewolf -> "JWerewolf"
    | JFox -> "JFox"
    | JDraw -> "JDraw"

  let genPlayers config required =
    (* requiredをcategoryごとに分類 *)
    let reqsets =
      Map.of_enum @@
      Enum.map
        (fun x -> (x, [])) @@
      List.enum Job.categories in
    let reqsets =
      List.fold_left
        (fun mp j ->
           let c = Job.category_of_job j in
           Map.modify
             c
             (List.cons j)
             mp)
        reqsets
        required in
    let rec gen mp reqsets result id = function
      | [] -> result
      | (t::teams') as teams ->
          match Map.find t mp with
            | 0 ->
                gen mp reqsets result id teams'
            | i ->
                begin
                  let (reqsets', pl) =
                    match Map.find t reqsets with
                      | [] ->
                         (* 入れたいものはもうない *)
                          (reqsets, Job.generate t)
                      | j::js ->
                          (* jを入れたい *)
                          let reqsets' =
                            Map.add
                              t
                              js
                              reqsets in
                            (reqsets', j) in
                  let mp' = Map.add t (i-1) mp in
                  let result' = (id, pl)::result in
                    gen mp' reqsets' result' (id+1) teams
                end in
      gen config reqsets [] 0 Job.categories

  let initSimulation config required =
    (* プレイヤーたちを生成 *)
    let players = genPlayers config required in
      { players }

  let judge g =
    let players = g.players in
    (* 数を数える *)
    let vi = List.count (Job.is_human % snd) players in
    let wo = List.count (Job.is_werewolf % snd) players in
    let fx = List.count (Job.is_fox % snd) players in
      if wo = 0 then
        if fx > 0 then
          Some JFox
        else if vi > 0 then
          Some JVillager
        else
          Some JDraw
      else if vi <= wo then
        if fx > 0 then
          Some JFox
        else
          Some JWerewolf
      else
        None

  (* プレイヤーIDからプレイヤーを選ぶ *)
  let get_player id players = List.assoc id players
  (* 該当プレイヤーを1人ランダムに選ぶ *)
  let choose pred players =
    let preds = List.filter pred players in
      match preds with
        | [] -> None
        | _ ->
            let r = Random.int @@ List.length preds in
              Some (fst @@ List.at preds r)
  (* プレイヤーを除去 *)
  let kill_player_if id pred players =
    let rec go res = function
      | [] -> List.rev res
      | (n, job)::xs when n = id && pred job ->
          List.rev_append res xs
      | x::xs ->
          go (x::res) xs in
      go [] players

  let simulate_day config =
    (* 昼は1人を処刑 *)
    let players = config.players in

    (* 占いセンサー *)
    let div_cnt = List.count
                    (fun (_, job) -> job = Job.Diviner)
                    players in
    let mad_cnt = List.count
                    (fun (_, job) -> job = Job.Mad)
                    players in
    let divine_target =
      if div_cnt > 0 || mad_cnt > 0 then
        if Random.int (div_cnt + mad_cnt) < div_cnt then
          (* 真占い *)
          begin
            match choose (const true) players with
              | Some t ->
                  let job = get_player t players in
                    if Job.is_werewolf job then
                      (* 狼見つけた *)
                      Some t
                    else
                        None
              | None -> None
          end
        else
            None
      else
          None in
    (* 昼の処刑先 *)
    let execute_target = match divine_target with
      | Some t -> Some t
      | None -> choose (const true) players in

    let players' = 
      match execute_target with
        | Some target ->
            (* 1人抜ける *)
            let players' = kill_player_if target (const true) players in
              players'
        | None ->
            (* ??? *)
            players in
      { (* config with *) players = players' }
    (* 夜は1人を襲撃 *)
  let simulate_night config =
    let players = config.players in
    (* 占いの呪殺対象 *)
    let divined =
      List.fold_left
        (fun s (_, j) ->
           if j = Job.Diviner then
             begin
               match choose (const true) players with
                 | Some t -> Set.add t s
                 | None -> s
             end
           else
             s)
        Set.empty
        players in
    (* 狼以外から選択 *)
    let players =
      match choose (neg Job.is_werewolf % snd) players with
        | Some target ->
            (*
             let pl = get_player target players in
             Printf.printf "Night %s\n" (Job.job_string pl);
             *)
            (* 狩人が守る *)
            let guarded =
              List.fold_left
                (fun s (id, job) ->
                   if job = Job.Guard then
                     begin
                       match choose (fun (id',_) -> id <> id') players with
                         | None -> s
                         | Some id -> Set.add id s
                     end
                   else
                     s)
                Set.empty
                players in
            let players' =
              if Set.mem target guarded then
                (* 護衛成功 *)
                players
              else
                  kill_player_if
                    target
                    Job.is_edible
                    players in
              players'
        | None ->
            players in
    (* 占われた狐をどける *)
    let players = List.filter
                    (fun (id, j) -> (not @@ Job.is_fox j) || (not @@ Set.mem id divined))
                    players in
      { players = players }
  let simulate config =
    (* 昼と夜を交互に *)
    let rec turn b config =
      match judge config with
        | Some x -> x
        | None when b ->
            let config' = simulate_night config in
              turn (not b) config'
        | None ->
            let config' = simulate_day config in
              turn (not b) config'
    in
      turn true config



end
