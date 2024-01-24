open Graphics;;
open_graph "";;


let () = print_endline "test";;

let cell_color = function
| 0 -> white (* nécessite l’ouverture de Graphics *)
| _ -> black ;;
let new_cell = 1 ;; (* cellule vivante *)
let empty = 0 ;;
let size_cell = 10 ;;

let is_alive cell = cell = 1

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
  let rec put cell (x,y) board = match board with
  | [] -> []
  | el :: board ->  if x = 0 then replace el y cell
  else put cell (x-1,y) board
in put cell (x,y) board

let rec count_neighbours (x,y) board =
  let rec count_neighbours_in_row y row =
    match row with
    | [] -> 0
    | hd :: tl ->
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

let open_window size = open_graph (string_of_int size ^ "x" ^ string_of_int (size+20))

let draw_cell (x,y) size color = 
  let grey = rgb 127 127 127 in
  let x_size = x * size in
  let y_size = y * size in
  set_color grey;
  draw_rect x_size y_size size size;
  set_color color;
  fill_rect (x_size+1) (y_size+1) (size-2) (size-2)

let draw_board board size =
  