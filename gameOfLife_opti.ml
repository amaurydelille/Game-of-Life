#use "topfind";;
#require "graphics";;
open Graphics;;
#use "gameOfLife.ml";;

(*Tant qu'il y a de la vie*)
(*remaining*)
let rec remaining board =
  match board with
    []    -> false
  | l::bo -> let rec remain l =
               match l with
                 []   -> remaining bo
               | e::l -> if e = 1 then true else remain l
    in remain l;;

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

let new_game_survival size nb_cells =
  let rec newrec size nb_cells =
  if (remaining board) <> 0 then new_game size nb_cells (n+1)
  else new_game size 0 1
  in newrec size nb_cells;;

let new_game_survival size nb_cells =
  let rec newrec size nb_cells n =
    let board = new_board size nb_cells in 
    if (remaining board) = false then
      new_game size 0 1
    else
      new_game size nb_cells (n+1)
  in newrec size nb_cells 1;;

let new_game_survival size nb =
  let rec survival board size n =
    if remaining board then
      begin
        clear_graph();
        draw_board board 10;
        survival (next_generation board size) size (n - 1)
      end
    else
      ()
      in
  open_window (size*size);
  survival (new_board size (nb)) size nb;;

new_game_survival 20 200;;
      
    
