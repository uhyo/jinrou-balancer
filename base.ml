open Batteries

module List = struct
  include List

  let count pred =
    fold_left
      (fun cnt v ->
         if pred v then
           cnt + 1
         else
           cnt)
      0
  let kick i l =
    let rec kick' i res = function
      | [] -> []
      | x::xs when i <= 0 -> List.rev_append res xs
      | x::xs  -> kick' (i-1) (x::res) xs in
      kick' i [] l
end
