let rec range2 a b = if a > b then [] else a :: range2 (a + 1) b
(* here tailcall would fail... *)


let range a b =
  let rec f a b acc =
    if a > b then acc else (f [@tailcall]) a (b - 1) (b :: acc)
  in
  f a b []

let prime x =
  let rec f n i =
    if i == 1 then true
    else if n mod i = 0 then false
    else (f [@tailcall]) n (i - 1)
  in
  f x (x - 1)

(*
  let _ =
    for i = 2 to 30 do
      Printf.printf "%d -> %b\n" i (prime i)
    done
    *)
