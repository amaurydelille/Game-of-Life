#use "topfind";;
#require "graphics";;
open Graphics;;

(*list_tools*)
let nth n l =
  if n < 1 then
    invalid_arg "nth : n must be natural"
  else
    let rec nthrec n l =
      match l with 
       []    -> failwith "nth : list too short"
      |e::l2 -> if n = 0 then e
        else nthrec (n-1) l2
            in nthrec n l;;

let rec init_list n x =
  match n, x with
    0, _            -> []
  | n, _ when n < 0 -> invalid_arg "init_list: n must be natural" 
  | _, _            -> x::init_list (n-1) x ;;

let rec init_board (l, c) x =
  match (l, c) with
    (0, _)|(_, 0) -> []
  |_              -> (init_list c x)::(init_board ((l-1),c) x) ;;

let rec init_list n x =
  match (n, x) with
    (0, x)             -> []
  | (_, x) when  n < 0 -> invalid_arg "n must be natural"
  | (_, x)             -> x::init_list (n-1) x;;

let rec put_list v i list =
  match i, list with
    _, []   -> []
  | 0, e::l -> v::l
  | i, e::l -> e::(put_list v (i-1) l);;    

let rec get_cell (x, y) board =
  if x < 0 || y < 0 then
    invalid_arg "get_cell: (x, y) must be positive"
  else
    match (x, y), board with
      (0,_), l::board  -> nth y l
    | (_, _), []       -> invalid_arg "get_cell: value not included in the list"
    | (_, _), l::board -> get_cell ((x-1), y) board;;

let rec put_cell v (x, y) board =
  if x < 0 || y < 0 then invalid_arg "put_cell : must be natural"
  else
    let rec putrec v (x, y) board =
      match x, board with 
        (0, l::board) ->( put_list v y l)::board
      | (_, [])       -> invalid_arg "put_cell : value not included in list"
      | (_, l::board) -> l::(putrec v ((x-1), y) board) in putrec v (x, y) board ;;

(*draw_cell*)
clear_graph();;
let draw_cell (x, y) size color =
  if color = white then
    begin
      moveto x y;
      draw_rect x y size size
    end
  else
    begin
      set_color color;
      fill_rect x y (size+1) (size+1);
      set_color white;
      draw_rect x y size size
        end;;

(*draw board*)
let cell_color = function
    0 -> white
  | _ -> black;;

let rec column (x, y) list cellsize =
  match list, (x,y) with
      [],_ -> ()
    | e::l,(x,y) -> if e = 1 then
        begin draw_cell (x, y) cellsize black;
          (*recol*)column (x, y+cellsize) l cellsize
        end
      else
        begin
          draw_cell (x, y) cellsize white;
          column (x, y+cellsize) l cellsize
        end;;

let rec draw_board board cellsize =
  let rec drawrec (x, y) board cellsize =
    match board, (x, y) with
      [], _   -> ()
    | l::b, (x, y) -> begin
              column (x, y) l cellsize;
              drawrec ((x + cellsize), y) b cellsize;
            end in drawrec (0, 0) board cellsize;;


let board = init_board (10, 10) 0;;

draw_board board 50;;
clear_graph();;
open_graph"1200x800";;
