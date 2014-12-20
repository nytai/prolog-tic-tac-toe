

%%%% FACTS %%%%%

next_player(1, 2).
next_player(2, 1).

opposite('x', 'o').
opposite('o', 'x').

player(1, 'x').
player(2, 'o').

player_symbol(1, 'x').
player_symbol(2, 'o').

op_symbol(1, 'o').
op_symbol(2, 'x').

empty_mark('e').



max('x').
min('o').


%%%%%%% tiles correspond to items in a list %%%%%%

tile(board([S,_,_,_,_,_,_,_,_]),1,S).
tile(board([_,S,_,_,_,_,_,_,_]),2,S).
tile(board([_,_,S,_,_,_,_,_,_]),3,S).
tile(board([_,_,_,S,_,_,_,_,_]),4,S).
tile(board([_,_,_,_,S,_,_,_,_]),5,S).
tile(board([_,_,_,_,_,S,_,_,_]),6,S).
tile(board([_,_,_,_,_,_,S,_,_]),7,S).
tile(board([_,_,_,_,_,_,_,S,_]),8,S).
tile(board([_,_,_,_,_,_,_,_,S]),9,S).


%%%% list winning configurations %%%%%%%
win(board([S,S,S, _,_,_, _,_,_]),S).
win(board([_,_,_, S,S,S, _,_,_]),S).
win(board([_,_,_, _,_,_, S,S,S]),S).
win(board([S,_,_, S,_,_, S,_,_]),S).
win(board([_,S,_, _,S,_, _,S,_]),S).
win(board([_,_,S, _,_,S, _,_,S]),S).
win(board([S,_,_, _,S,_, _,_,S]),S).
win(board([_,_,S, _,S,_, S,_,_]),S).



%%%%%%%%%%%%%


%%%% MAIN PROGRAM %%%%%%%
%% start <-- Begin tic tac toe 
start :-
	nl,
	nl,
	write('Play Tic-Tac-Toe.'),
	print_players,
	empty_mark(E),
	play(1, board([E,E,E, E,E,E, E,E,E])).


%% finish(B) <--- declare winner of board B, replay 
finish(B):-
	nl,
	nl,
	write('Game over.'),
	print_winner(B),
	print_board(B),
	play_again(V),
	!,
	(V== 'y'),
	!,
	start.

%% play_again <--- promt useser to play again.
play_again(V):-
	nl,
	nl,
	write('play again? [enter y/n]  '),
	read(V),
	(V== 'y'; V=='n'),
	!.

play_again(V):-
	nl,
	nl,
	write('please enter y or n.  '),
	play_again(V).


%% play(Player, Board) <--- play tic tac toe until there is a winner 
play(P, B):-
	game_over(P, B),
	finish(B).

play(P, B):-
	print_board(B),
	!,
	\+(game_over(P, B)),
	!,
	make_move(P, B, NB),
	!,
	next_player(P, P2),
	!,
	play(P2, NB),
	!.




%% make_move(Player, Board, NewBoard) <-- prompt player for move, apply move to Board as NewBoard.
make_move(P, B, NB):-
	P=1,
	nl,
	nl,
	write('Player 1 choose a tile to mark:  '),
	read(T),

	empty_mark(E),
	tile(B, T, E),
	player_symbol(P, S),
	place(B, T, S, NB),
	!.

make_move(P, B, NB):-
	P=2,
	nl,
	nl,
	write('Player 2 choose a tile to mark:   '),
	read(T),

	empty_mark(E),
	tile(B, T, E),
	player_symbol(P, S),
	place(B, T, S, NB),
	!.

make_move(P, B, NB):-
	nl,
	nl,
	write('please select an available/numbered tile.'),
	make_move(P, B, NB).



%% Place(Board, Tile, Symbol, NewBoard) <-- Place Symbol on Tile in Board as NewBoard
place(board(SB), T, S, NB):-
	set_item(SB, T, S, NSB),
	NB=board(NSB).

% set_item(List, Position, V, NewList) <-- Newlist is List with V inserted in index Position
set_item(L, N, V, L2) :-
    set_itemOp(L, N, V, 1, L2).

set_itemOp( [], N, V, A, L2) :- 
    N == -1, 
    L2 = [].

set_itemOp( [_|T1], N, V, A, [V|T2] ) :- 
    A = N,
    A1 is N + 1,
    set_itemOp( T1, -1, V, A1, T2 ).

set_itemOp( [H|T1], N, V, A, [H|T2] ) :- 
    A1 is A + 1, 
    set_itemOp( T1, N, V, A1, T2 ).




%% game_over(Player, Board) <--- determine is player or openent has won the Board
game_over(P, B):-
	game_over_op(P, B).

game_over_op(P, B):-
	player_symbol(P, S),
	win(B, S).

%Game over if oponent wins
game_over_op(P, B):-
	op_symbol(P, S),
	win(B, S).
%Game over if no tiles are empty
game_over_op(P, B):-
	empty_mark(E),
	\+(tile(B, T, E)).


%%%%%Pint Functions, self explanatory%%%%%%%%

print_players:-
	nl, 
	player(1, V1),
	write('Player 1: '),
	write(V1),
	nl,
	player(2, V2),
	write('Player 2: '),
	write(V2),
	!.

print_winner(B):-
	win(B, x),
	write('x wins.'),
	!.
print_winner(B):-
	win(B, o),
	write('o wins'),
	!.
print_winner(B):-
	write('No winner. Match is a draw').

print_board(B):-
	nl,
	write('_________'),
	nl,
	print_tile(B, 1), write(' | '),
	print_tile(B, 2), write(' | '),
	print_tile(B, 3), write(' | '),
	nl,
	write('__________'),
	nl,
	print_tile(B, 4), write(' | '),
	print_tile(B, 5), write(' | '),
	print_tile(B, 6), write(' | '),
	nl,
	write('__________'),
	nl,
	print_tile(B, 7), write(' | '),
	print_tile(B, 8), write(' | '),
	print_tile(B, 9), write(' | '),
	nl,
	write('__________'),
	!.

print_tile(B, N):-
	tile(B, N, S),
	print_tile_op(N, S),
	!.

print_tile_op(N, E):-
	empty_mark(E),
	write(N),
	!.
print_tile_op(N, S):-
	write(S),
	!.




