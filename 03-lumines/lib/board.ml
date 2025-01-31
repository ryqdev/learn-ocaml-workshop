open Base

type t =
  { board : Filled_square.t option array array
  ; height : int
  ; width : int
  }

let create ~height ~width =
  { board = Array.make_matrix ~dimx:width ~dimy:height None; height; width }
;;

let get t { Point.col; row } = t.board.(col).(row)
let set t { Point.col; row } value = t.board.(col).(row) <- value

let mark_squares_that_are_sweepable t =
  (* TODO: at the end of this function, all filled_squares that are part of
     completed squares (i.e. four tiles in a square arrangement that are all of
     the same colors) should be in sweeper state [To_sweep], and all other
     squares should be [Unmarked].

     Note that, for example, a 2x3 rectangle of all the same color should also
     be marked by these criteria. *)
     List.iter (List.range 0 (t.height - 1)) ~f:(
       fun row -> List.iter (List.range 0 (t.width - 1)) ~f:(
         fun col -> match get t {row; col}, get t {row; col = col + 1}, get t {row = row + 1; col}, get t {row = row + 1; col = col + 1} with
         | Some square1, Some square2, Some square3, Some square4 ->
           if Color.equal square1.color square2.color && Color.equal square1.color square3.color && Color.equal square1.color square4.color
             then (
               square1.sweeper_state <- Filled_square.Sweeper_state.To_sweep;
               square2.sweeper_state <- Filled_square.Sweeper_state.To_sweep;
               square3.sweeper_state <- Filled_square.Sweeper_state.To_sweep;
               square4.sweeper_state <- Filled_square.Sweeper_state.To_sweep
             )
         | _ -> ())
     )
;;

let remove_squares t =
  (* TODO: remove any squares marked as [Swept] from the board.  Gravity should
     be applied appropriately. This is the function that is called by the
     [Sweeper.t] to clear squares from the board.

     At the end of this function, we should call
     [mark_squares_that_are_sweepable] so that we ensure that we leave the board
     in a valid state.  *)
  List.iter (List.range 0 t.height) ~f:(fun row ->
      List.iter (List.range 0 t.width) ~f:(fun col ->
          match get t {row; col} with
          | None -> ()
          | Some square -> if Filled_square.Sweeper_state.equal square.sweeper_state Filled_square.Sweeper_state.To_sweep
            then set t {row; col} None));
  mark_squares_that_are_sweepable t

;;

let add_piece_and_apply_gravity t ~moving_piece ~col =
  (* TODO: insert (affix) the moving piece into the board, applying gravity
     appropriately. Make sure to leave the board in a valid state. *)
  let find ~col = 
    List.find (List.range 0 t.height) ~f:(
      fun row -> match get t {row; col} with
      | None -> true
      | Some _ -> false)
  in
  let find_left = find ~col:col in
  let find_right = find ~col:(col + 1) in
  match find_left, find_right with
  | None, _ | _, None -> false
  | Some left_row, Some right_row ->
    if left_row < t.height - 1 && right_row < t.height - 1
      then (
        set t {row = left_row; col} (Some moving_piece.Moving_piece.bottom_left);
        set t {row = left_row + 1; col} (Some moving_piece.Moving_piece.top_left);
        set t {row = right_row; col = col + 1} (Some moving_piece.Moving_piece.bottom_right);
        set t {row = right_row + 1; col = col + 1} (Some moving_piece.Moving_piece.top_right);
        true
      )
    else false
;;

let is_empty t point =
  match get t point with
  | None -> true
  | Some _ -> false
;;

(* Tests *)
let is_filled_with_color t ~row ~col color =
  match get t { Point.row; col } with
  | None -> false
  | Some square -> Color.equal color square.color
;;

let is_marked t ~row ~col =
  match get t { Point.row; col } with
  | None -> false
  | Some square ->
    Filled_square.Sweeper_state.equal
      square.Filled_square.sweeper_state
      Filled_square.Sweeper_state.To_sweep
;;

let test_piece =
  { Moving_piece.top_left = Filled_square.create Color.Orange
  ; top_right = Filled_square.create Color.White
  ; bottom_left = Filled_square.create Color.White
  ; bottom_right = Filled_square.create Color.White
  }
;;

let%test "Testing add_piece_and_apply_gravity add one..." =
  let t = create ~height:4 ~width:4 in
  add_piece_and_apply_gravity t ~moving_piece:test_piece ~col:0
  && is_filled_with_color t ~row:0 ~col:0 Color.White
  && is_filled_with_color t ~row:0 ~col:1 Color.White
  && is_filled_with_color t ~row:1 ~col:0 Color.Orange
  && is_filled_with_color t ~row:1 ~col:1 Color.White
;;

let%test "Testing add_piece_and_apply_gravity add many..." =
  let t = create ~height:4 ~width:4 in
  add_piece_and_apply_gravity t ~moving_piece:test_piece ~col:0
  && add_piece_and_apply_gravity t ~moving_piece:test_piece ~col:0
  && not (add_piece_and_apply_gravity t ~moving_piece:test_piece ~col:0)
;;

let test_removable_piece =
  { Moving_piece.top_left = Filled_square.create Color.White
  ; top_right = Filled_square.create Color.White
  ; bottom_left = Filled_square.create Color.White
  ; bottom_right = Filled_square.create Color.White
  }
;;

let%test "Testing mark_squares_that_are_sweepable..." =
  let t = create ~height:4 ~width:4 in
  assert (add_piece_and_apply_gravity t ~moving_piece:test_removable_piece ~col:0);
  assert (add_piece_and_apply_gravity t ~moving_piece:test_piece ~col:0);
  mark_squares_that_are_sweepable t;
  is_marked t ~row:0 ~col:0
  && is_marked t ~row:0 ~col:1
  && is_marked t ~row:1 ~col:0
  && is_marked t ~row:1 ~col:1
  && is_marked t ~row:2 ~col:0
  && is_marked t ~row:2 ~col:1
  && (not (is_marked t ~row:3 ~col:0))
  && not (is_marked t ~row:3 ~col:1)
;;

let sweep_board t =
  Array.iter t.board ~f:(fun row ->
      Array.iter row ~f:(fun square ->
          Option.iter square ~f:(fun square -> ignore (Filled_square.sweep square))))
;;

let%test "Testing Remove_squares..." =
  let t = create ~height:4 ~width:4 in
  assert (add_piece_and_apply_gravity t ~moving_piece:test_removable_piece ~col:0);
  assert (add_piece_and_apply_gravity t ~moving_piece:test_piece ~col:0);
  mark_squares_that_are_sweepable t;
  sweep_board t;
  remove_squares t;
  is_filled_with_color t ~row:0 ~col:0 Color.Orange
  && is_filled_with_color t ~row:0 ~col:1 Color.White
;;
