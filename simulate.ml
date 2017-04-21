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

  val initSimulation : Village.config -> game
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

  let genPlayers config =
    let rec gen mp result id = function
      | [] -> result
      | (t::teams') as teams ->
          match Map.find t mp with
            | 0 ->
                gen mp result id teams'
            | i ->
                let mp' = Map.add t (i-1) mp in
                let pl = Job.generate t in
                let result' = (id, pl)::result in
                  gen mp' result' (id+1) teams in
      gen config [] 0 Job.categories

  let initSimulation config =
    (* プレイヤーたちを生成 *)
    let players = genPlayers config in
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
    let preds = List.filter (pred % snd) players in
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
    match choose (const true) players with
      | Some target ->
          (* 1人抜ける *)
          (*
          let pl = get_player target players in
          Printf.printf "Day %s\n" (Job.job_string pl);
           *)
          let players' = kill_player_if target (const true) players in
            { (* config with *) players = players'; }
      | None ->
          (* ??? *)
          { (* config with *) players }
    (* 夜は1人を襲撃 *)
  let simulate_night config =
    let players = config.players in
    (* 狼以外から選択 *)
    match choose (neg Job.is_werewolf) players with
      | Some target ->
          (*
          let pl = get_player target players in
          Printf.printf "Night %s\n" (Job.job_string pl);
           *)
          let players' = kill_player_if
                           target
                           Job.is_edible
                           players in
            { (* config with *) players = players'; }
      | None ->
          { players }
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
