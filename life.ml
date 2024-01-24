open Graphics

let new_cell = 1 ;; (* cellule vivante *)
let empty = 0 ;;

let is_alive cell = cell <> empty

let rules cell n = match n with
  | n when n = 2 -> cell
  | n when n = 3 -> new_cell
  | _ -> 0

let rec build_lst n v = match n with
  | 0 -> []
  | n -> v :: build_lst (n-1) v

let gen_board n v =
  let n_fix = n in
  let rec gen n v = match n with
  | 0 -> []
  | n -> (build_lst n_fix v ) :: gen (n-1) v
in gen n v

let rec nth lst i = match lst with
| [] -> empty
| e :: lst -> if i = 0 then e else nth lst (i-1)

let get_cell (x,y) board =
  let rec getc (x, y) board = match board with
  | [] -> empty
  | el :: board -> if x = 0 then nth el y
  else getc (x-1, y) board
in getc (x,y) board

let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l

let put_cell cell (x,y) board =
  let rec put cell (x, y) board = match board with
    | [] -> []
    | el :: rest ->
        if x = 0 then replace el y cell :: rest
        else el :: put cell (x - 1, y) rest
  in put cell (x, y) board

let count_neighbours (x,y) board =
  let rec count_neighbours_in_row y row =
    match row with
    | [] -> 0
    | _ :: tl ->
        let count_right = if is_alive(get_cell(x, y+1) board) then 1 else 0 in
        let count_left = if is_alive(get_cell(x, y-1) board) then 1 else 0 in
        count_right + count_left + count_neighbours_in_row (y + 1) tl
  in
  let rec count_neighbours_in_matrix x y matrix =
    match matrix with
    | [] -> 0
    | row :: tl ->
        let count_above = if is_alive(get_cell(x-1, y) board) then count_neighbours_in_row y row else 0 in
        let count_below = if is_alive(get_cell(x+1, y) board) then count_neighbours_in_row y row else 0 in
        count_above + count_below + count_neighbours_in_matrix (x + 1) y tl
  in count_neighbours_in_matrix x y board

let open_window size = open_graph (" " ^string_of_int size ^ "x" ^ string_of_int (size+20))

let draw_square (x,y) size = 
  moveto x y;
  lineto (x+size) y;
  lineto (x+size) (y+size);
  lineto x (y+size);
  lineto x y

let draw_fill (x,y) size color =
  set_color color;
  let finalX = x+size in 
  let rec aux x y = 
    moveto x y;
    match x with
    |x when x = finalX -> lineto x (y+size)
    |x -> lineto x (y+size); aux (x+1) y
  in
  aux x y
  
let draw_cell (x,y) size cell = 
  match cell with
    |0 -> draw_square (x,y) size
    |1 -> draw_fill (x,y) size black
    |_ -> failwith "invalid cell"

let rec draw_line line size (x,y) = match line with
  |[] -> ()
  |e::l -> draw_cell (y,x) size e; draw_line l size  (x+size,y)

let draw_board board size =
  clear_graph();
  let (x,y) = (0,0) in
  let rec aux (x,y) = function
    |[] -> ()
    |e::l -> draw_line e size (x,y); aux (x,y+size) l
  in
  aux (x,y) board

let rec seed_life board size nb_cell = match nb_cell with
| 0 -> board
| _ -> let a = Random.int size and b = Random.int size in
          if get_cell (a,b) board = 1 then seed_life board size nb_cell
          else
            let board_2 = put_cell new_cell (a,b) board in seed_life board_2 size (nb_cell - 1)

let new_board size nb_cell =
  let nw_brd = gen_board size 0 in
  seed_life nw_brd size nb_cell

let next_generation board size =
  let rec fun2 (x,y) board2 = match (x,y) with
  | (x,y) when x = size && y = size -> board2
  | (_,y) when y = size -> board2
  | (_,_) -> fun2 (x, y+1) (put_cell (rules (get_cell(x,y) board) (count_neighbours(x,y) board)) (x,y) board2)
in fun2 (0,0) board

let rec game board size n = match n with
  | 0 -> ()
  | n -> draw_board board size; game (next_generation board size) size (n-1)

let new_game size nb n = open_window size ; game (new_board size nb) size n

let () = new_game 300 110 1000