open Graphics

let cell_color = function 0 -> white | _ -> black
let new_cell = 1 (* cellule vivante *)
let empty = 0
let cell_size = 10
let is_alive cell = cell <> empty

let rules cell n =
  match n with n when n = 2 -> cell | n when n = 3 -> new_cell | _ -> 0

let rec build_lst n v = match n with 0 -> [] | n -> v :: build_lst (n - 1) v

let gen_board n v =
  let n_fix = n in
  let rec gen n v =
    match n with 0 -> [] | n -> build_lst n_fix v :: gen (n - 1) v
  in
  gen n v

let rec nth lst i =
  match lst with [] -> empty | e :: lst -> if i = 0 then e else nth lst (i - 1)

let get_cell (x, y) board =
  let rec getc (x, y) board =
    match board with
    | [] -> empty
    | el :: board -> if x = 0 then nth el y else getc (x - 1, y) board
  in
  getc (x, y) board

let replace l pos a = List.mapi (fun i x -> if i = pos then a else x) l

let put_cell cell (x, y) board =
  let rec put cell (x, y) board =
    match board with
    | [] -> []
    | el :: rest ->
        if x = 0 then replace el y cell :: rest
        else el :: put cell (x - 1, y) rest
  in
  put cell (x, y) board

let count_neighbours (x, y) board =
  let rec count_neighbours_in_row y row =
    match row with
    | [] -> 0
    | _ :: tl ->
        let count_right =
          if is_alive (get_cell (x, y + 1) board) then 1 else 0
        in
        let count_left =
          if is_alive (get_cell (x, y - 1) board) then 1 else 0
        in
        count_right + count_left + count_neighbours_in_row (y + 1) tl
  in
  let rec count_neighbours_in_matrix x y matrix =
    match matrix with
    | [] -> 0
    | row :: tl ->
        let count_above =
          if is_alive (get_cell (x - 1, y) board) then
            count_neighbours_in_row y row
          else 0
        in
        let count_below =
          if is_alive (get_cell (x + 1, y) board) then
            count_neighbours_in_row y row
          else 0
        in
        count_above + count_below + count_neighbours_in_matrix (x + 1) y tl
  in
  count_neighbours_in_matrix x y board

let open_window size =
  open_graph (" " ^ string_of_int size ^ "x" ^ string_of_int (size + 20))

let grey = rgb 127 127 127

let draw_cell (x, y) size color =
  let draw_size = size - 1 in
  let screen_x = x * size in
  let screen_y = y * size in

  set_color grey;
  (*couleur contour gris*)
  draw_rect screen_x screen_y draw_size draw_size;

  set_color color;
  (*couleur cellule (noire)*)
  fill_rect (screen_x + 1) (screen_y + 1) draw_size draw_size

let draw_board board cell_size =
  clear_graph ();

  (* Effacer la fenêtre graphique *)
  let rows = List.length board in
  let cols = List.length (List.hd board) in

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let cell_value = get_cell (i, j) board in
      let cell_color_r = cell_color cell_value in
      draw_cell (i, j) cell_size cell_color_r
    done
  done

let rec seed_life board size nb_cell =
  match nb_cell with
  | 0 -> board
  | _ ->
      let a = Random.int size and b = Random.int size in
      if get_cell (a, b) board = 1 then seed_life board size nb_cell
      else
        let board_2 = put_cell new_cell (a, b) board in
        seed_life board_2 size (nb_cell - 1)

let new_board size nb_cell =
  let nw_brd = gen_board size 0 in
  seed_life nw_brd size nb_cell

let next_generation board size =
  let rec fun2 (x, y) board2 =
    match (x, y) with
    | x, y when x = size && y = size -> board2
    | _, y when y = size -> board2
    | _, _ ->
        fun2
          (x, y + 1)
          (put_cell
             (rules (get_cell (x, y) board) (count_neighbours (x, y) board))
             (x, y) board2)
  in
  fun2 (0, 0) board

let rec game board size n =
  match n with
  | 0 -> ()
  | n ->
      draw_board board cell_size;
      game (next_generation board size) size (n - 1)

let new_game size nb n =
  open_window size;
  game (new_board size nb) size n

let () = new_game 100 300 1000