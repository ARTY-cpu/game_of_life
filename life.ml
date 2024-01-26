open Graphics

let cell_color = function 0 -> white | _ -> black
let new_cell = 1 (* cellule vivante *)
let empty = 0
let cell_size = 30 (*taille cases carré pour nombre cases totales tableau*)
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
  let is_valid_coord (i, j) =
    i >= 0 && i < List.length board && j >= 0 && j < List.length (List.hd board)
  in
  let live_neighbors =
    let offsets =
      [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
    in
    List.fold_left
      (fun count (dx, dy) ->
        let nx, ny = (x + dx, y + dy) in
        if is_valid_coord (nx, ny) && is_alive (get_cell (nx, ny) board) then
          count + 1
        else count)
      0 offsets
  in
  live_neighbors

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

let seed_life board size count =
  let rec place_cells count board =
    if count = 0 then board
    else
      let random_row = Random.int size in
      let random_col = Random.int size in
      let new_board = put_cell new_cell (random_row, random_col) board in
      place_cells (count - 1) new_board
  in
  (* Initialiser le générateur de nb aléatoires
      avec une graine basée sur le temps*)
  Random.self_init ();
  place_cells count board

let new_board size nb_cell =
  let nw_brd = gen_board size 0 in
  seed_life nw_brd size nb_cell

let next_generation board =
  let apply_rules i j current_board =
    let cell_value = get_cell (i, j) current_board in
    let live_neighbors = count_neighbours (i, j) current_board in
    rules cell_value live_neighbors
  in
  let new_board =
    List.mapi
      (fun i row -> List.mapi (fun j _ -> apply_rules i j board) row)
      board
  in
  new_board

let rec game board size n =
  if n = 0 then ()
  else (
    draw_board board size;
    synchronize ();
    (* Attendre 1 seconde entre les générations *)
    Unix.sleepf 0.5;
    let next_gen_board = next_generation board in
    game next_gen_board size (n - 1))

let new_game board_size initial_cell_count num_generations =
  open_window ( cell_size * cell_size );
  (* Créer un nouveau plateau de jeu avec
     la taille spécifiée et le nombre de cellules initiales *)
  let game_board = new_board board_size initial_cell_count in

  (* Jouer au jeu de la vie pour le nombre de générations spécifié *)
  game game_board board_size num_generations;
  ignore (wait_next_event [ Button_down ])
(*pour fin fonction*)

let () = new_game cell_size 300 100