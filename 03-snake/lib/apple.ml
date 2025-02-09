open! Base

type t = { location : Position.t } [@@deriving sexp_of]

let location t = t.location

(* TODO: Implement [create].

   Make sure to inspect the mli to understand the signature of[create]. [create]
   will take in the height and width of the board area, as well as a list of
   locations where the apple cannot be generated, and create a [t] with a random
   location on the board.

   Hint: 
   - You can generate a random int up to [bound] via [Random.int bound].
   - You can pick a random element out of a list using [List.random_element_exn list]. 
*)
let create ~height ~width ~invalid_locations =
  let all_positions =
    List.init height ~f:(fun row ->
        List.init width ~f:(fun col -> { Position.row; col }))
    |> List.concat
  in
  let valid_positions =
    List.filter all_positions ~f:(fun pos ->
        not (List.mem invalid_locations pos ~equal:[%compare.equal: Position.t]))
  in
  match List.random_element valid_positions with
  | None -> None
  | Some location -> Some { location }
