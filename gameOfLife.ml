#use "topfind";;
#require "graphics";;
open Graphics;;
open_graph"1200x800";;

(*rules0*)
let rules0 cell near =
  if cell = 0 then if near = 3 then 1 else 0
  else
  if near = 2 || near = 3 then 1 else 0;;

(*count_neighbours*)
let board = [[1;0;1];[0;1;0];[0;0;1]];;

let nth n l =
  if n < 0 then
    invalid_arg "nth : n must be natural"
  else
    let rec nthrec n l =
      match l with 
        []    -> 0(*failwith "nth : list too short"*)
      |e::l2 -> if n = 0 then e
        else nthrec (n-1) l2
    in nthrec n l;;

let get_cell (x, y) board =
  if x < 0 || y < 0 then invalid_arg "get_cell : must be natural"
  else
    let rec getrec (x, y) board =
      match (x, y), board with
        (0, _), l::board -> nth y l
      | (_, _), [] -> 0(*invalid_arg "get_cell : value not included in list"*)
      | (_, _), l::board -> getrec ((x-1), y) board in getrec (x, y) board;;

let count_neighbours (x, y) board size = 
  if x > size || y > size then 0 else
  match (x, y) with
  
    (0, 0)             -> get_cell ((x+1), y) board + get_cell (x, (y+1)) board +
                          get_cell ((x+1), (y+1)) board
                            
  | (0, y) when y <> 0 -> get_cell (x, (y+1)) board + get_cell ((x+1), (y+1)) board +
                          get_cell ((x+1), y) board + get_cell ((x+1), (y-1)) board +
                          get_cell (x, (y-1)) board 
                            
  | (x, 0) when x <> 0 -> get_cell ((x-1), y) board + get_cell ((x-1), (y+1)) board +
                          get_cell (x, (y+1)) board + get_cell ((x+1), (y+1)) board +
                          get_cell ((x+1), y) board
                            
  | (_, _)             -> get_cell ((x-1), (y-1)) board + get_cell (x, (y-1)) board +
                          get_cell ((x+1), (y-1)) board + get_cell ((x+1), y) board +
                          get_cell ((x+1), (y+1)) board + get_cell (x, (y+1)) board +
                          get_cell ((x-1), (y+1)) board + get_cell ((x-1), y) board ;;

let board = [[1; 1; 1; 1; 1; 1; 1; 1; 1; 1];
[0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
[1; 0; 1; 0; 1; 0; 1; 0; 1; 0];
[0; 1; 0; 1; 0; 1; 0; 1; 0; 1];
[0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
[1; 1; 1; 1; 1; 1; 1; 1; 1; 1];
[0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
[1; 0; 1; 0; 1; 0; 1; 0; 1; 0];
[0; 1; 0; 1; 0; 1; 0; 1; 0; 1];
[0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
            ] ;;

(*seed_life*)
let rec put_list v i list =
  match i, list with
    _, []   -> []
  | 0, e::l -> v::l
  | i, e::l -> e::(put_list v (i-1) l);;

let rec put_cell v (x, y) board =
  if x < 0 || y < 0 then invalid_arg "put_cell : must be natural"
  else
    let rec putrec v (x, y) board =
      match x, board with 
        (0, l::board) ->( put_list v y l)::board
      | (_, [])       -> invalid_arg "put_cell : value not included in list"
      | (_, l::board) -> l::(putrec v ((x-1), y) board) in putrec v (x, y) board ;;

let seed_life board size nb_cell =
  let rec seedrec board nb_cell =
    match nb_cell with
      0 -> board
    | _ -> seedrec (put_cell 1 ((Random.int size),(Random.int size)) board) (nb_cell - 1)
  in seedrec board nb_cell;;

let board = [[0;0;0];[0;0;0];[0;0;0]];;

(*new_board*)

let rec init_list n x =
  match (n, x) with
    (0, x)             -> []
  | (_, x) when  n < 0 -> invalid_arg "n must be natural"
  | (_, x)             -> x::init_list (n-1) x;;

let rec init_board (l, c) x =
  match (l, c) with
    (0, _)|(_, 0) -> []
  |_              -> (init_list c x)::(init_board ((l-1), c) x);;

let new_board size nb_cell =
  seed_life (init_board (size, size) 0) size nb_cell;;

(*next_generation*)
let draw_cell (x, y) size color =
  if color = white then
    begin
      moveto x y;
      draw_rect x y size size;
      set_color black;
      draw_rect x y size size
    end
  else
    begin
      set_color color;
      fill_rect x y (size+1) (size+1);
      set_color white;
      draw_rect x y (size+1) (size+1)
        end;;

let cell_color = function
    0 -> white
  | _ -> black;;

let rec colonne (x, y) list cellsize =
  match list, (x,y) with
      [],_ -> ()
    | e::l,(x,y) -> if e = 1 then
        begin draw_cell (x, y) cellsize black;
          (*recol*)colonne (x, y+cellsize) l cellsize
        end
      else
        begin
          draw_cell (x, y) cellsize white;
          colonne (x, y+cellsize) l cellsize
        end;;

let rec draw_board board cellsize =
  let rec drawrec (x, y) board cellsize =
    match board, (x, y) with
      [], _   -> ()
    | l::b, (x, y) -> begin
              colonne (x, y) l cellsize;
              drawrec ((x + cellsize), y) b cellsize;
              end in drawrec (0, 0) board cellsize;;


let next_generation board size =
  let rec column x board =
    match (x, board) with
      (_, [])   -> []
    | (_, l::accb) -> (ligne x 0 l)::(column (x + 1) accb)
  and ligne x y = function
      []     -> []
    | l::accb -> (rules0 l (count_neighbours (x, y) board size))::(ligne x (y + 1) accb)
  in column 0 board;;
open_graph"1200x800";;

next_generation board 3;;
clear_graph();;

(*game*)

let rec game board size n =
  match n with
    0 -> ()
  | _ -> begin
      clear_graph();
      draw_board board 20;
      game (next_generation board size) size (n-1)
    end;;
  
let board = [[1; 1; 1; 1; 1; 1; 1; 1; 1; 1];
[0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
[1; 0; 1; 0; 1; 0; 1; 0; 1; 0];
[0; 1; 0; 1; 0; 1; 0; 1; 0; 1];
[0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
[1; 1; 1; 1; 1; 1; 1; 1; 1; 1];
[0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
[1; 0; 1; 0; 1; 0; 1; 0; 1; 0];
[0; 1; 0; 1; 0; 1; 0; 1; 0; 1];
[0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
            ] ;;
game board 10 9;;

(*new_game*)

let open_window size = open_graph (" " ^ string_of_int size ^ "x" ^
                                   string_of_int ( size +20) ) ;;

let rec new_game size nb_cell n =
  open_window ((size*size)+20);
  game (new_board size nb_cell) size n;;

new_game 20 30 1;;
