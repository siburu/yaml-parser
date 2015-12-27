type value = [
  | `Mapping of (value * value) list
  | `Sequence of value list
  | `Scalar of string
]

type elem = [ value | `Comment of string ]

type doc = Doc of elem

open Core.Std
let rec output_value outc = function
        | `Mapping obj -> print_map outc obj
        | `Sequence l -> print_seq outc l
        | `Scalar s -> printf "\"%s\"" s

and print_map outc obj =
  output_string outc "{ ";
  let sep = ref "" in
  List.iter ~f:(fun (k, v) ->
      printf "%s%a : %a" !sep output_value k output_value v;
      sep := ",\n  ") obj;
  output_string outc " }"

and print_seq outc arr =
  output_string outc "[";
  List.iteri ~f:(fun i v ->
      if i > 0 then
        output_string outc ", ";
      output_value outc v) arr;
  output_string outc "]"

let output_elem outc = function
        | `Mapping obj -> print_map outc obj
        | `Sequence l -> print_seq outc l
        | `Scalar s -> printf "\"%s\"" s
        | `Comment c -> printf "#%s" c
