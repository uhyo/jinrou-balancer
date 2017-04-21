open Batteries

module Job : sig
  type t =
      | Villager (* normal villager *)
      | Diviner (* divines *)
      | Saver of int (* saves n times *)
      | Guard (* guards from wolves *)
      | Werewolf (* wolves *)
      | Fox (* foxes *)
      | Mad (* mad players *)

  type category =
    | TVillage
    | TMad
    | TWerewolf
    | TFox

  (* boilerplate *)
  val job_string : t -> string
  val category_string : category -> string
  val random_category : unit -> category
  val category_of_job : t -> category

  (* 全てのカテゴリ *)
  val categories : category list
  (* カテゴリに該当する役職をランダムに生成 *)
  val generate : category -> t
  (* 勝利判定用 *)
  val is_human : t -> bool
  val is_werewolf : t -> bool
  val is_fox : t -> bool

  (* 能力判定用 *)
  (* 狼に食べられる *)
  val is_edible : t -> bool
end = struct
  type t =
      | Villager (* normal villager *)
      | Diviner (* divines *)
      | Saver of int (* saves n times *)
      | Guard (* guards from wolves *)
      | Werewolf (* wolves *)
      | Fox (* foxes *)
      | Mad (* mad players *)

  type category =
    | TVillage
    | TMad
    | TWerewolf
    | TFox

  let categories = [
    TVillage;
    TMad;
    TWerewolf;
    TFox
  ]

  let category_of_job = function
    | Villager
    | Diviner
    | Saver _
    | Guard -> TVillage
    | Mad -> TMad
    | Werewolf -> TWerewolf
    | Fox -> TFox

  let random_category () =
    List.at categories @@  Random.int @@ List.length categories 

  (* teamごとのjobを得る *)
  let get_jobs = function
    | TVillage ->
        Enum.map
          (fun i ->
             (* 48種類 *)
             match i with
               | 0 | 1 | 2 -> Diviner
               | 3 | 4 -> Guard
               | 5 -> Saver 1
               | _ -> Villager) @@
        (0 --^ 48)
    | TMad -> Enum.singleton Mad
    | TWerewolf -> Enum.singleton Werewolf
    | TFox -> Enum.singleton Fox

  let generate t =
    let enu = get_jobs t in
      Random.choice enu

  let is_human = function
    | Villager
    | Diviner
    | Saver _
    | Guard
    | Mad ->
        true
    | _ -> false
  let is_werewolf = function
    | Werewolf -> true
    | _ -> false
  let is_fox = function
    | Fox -> true
    | _ -> false

  let is_edible = function
    | Werewolf
    | Fox -> false
    | _ -> true

  let job_string = function
    | Villager -> "Villager"
    | Diviner -> "Diviner"
    | Saver _ -> "Saver"
    | Guard -> "Guard"
    | Werewolf -> "Werewolf"
    | Fox -> "Fox"
    | Mad -> "Mad"

  let category_string = function 
    | TVillage -> "Village"
    | TMad -> "Mad"
    | TWerewolf -> "Werewolf"
    | TFox -> "Fox"


end
