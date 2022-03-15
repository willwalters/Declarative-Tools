% File:    sudoku
% Author:  William Walters 
% Purpose: Solves a regular Sudoku puzzle of size n x n, subject to logical constraints
%
% Copyright Will Walters 2022*  
%     *I declare no authorship of the puzzle, only the implementation below
%

:- use_module(library(apply)).
:- ensure_loaded(library(clpfd)).

/*==============================================================================
                              MATHS PUZZLE SOLVER

sudoku_solution/1 assumes that the matrix provided is a valid sudoku puzzle.
That is, it is a list of 9 lists each of length 9 that represent the puzzle.
All other predicates below are "helper" predicates that sudoku_solution/1
depends upon. 

  ============================================================================*/

  % puzzle_solution(+Matrix)
% Ensures that a solvable Puzzle is solved.
% I.e. filled with ground numbers according 
% to the constraints of soduku
sudoku_solution(Puzzle):-
    num_domain(Puzzle),
    transpose(Puzzle, PuzzleT),
    all_unique(Puzzle),
    all_unique(PuzzleT),
    diagonal_unique(Puzzle),
    diagonal_unique(PuzzleT),
    ground_puzzle(Puzzle).

/*==============================================================================
                      1. NATURAL NUMBER DOMAIN CONSTRAINT               

Numbers that fill the puzzle are Natural numbers, constrained to the domain: 
[1,9]. This constraint will prevent prolog from searching for solutions that use 
numbers <1 and >9, thus decreasing run time.

  ============================================================================*/

% num_domain(+Matrix)
% Ensures the domain constraint is applied to all rows
num_domain(Puzzle):-
    maplist(valid_nums, Puzzle).   

% valid_nums(+Vars)
% Ensures all numbers in a row satisfy the domain constraint
valid_nums(Row):- 
    length(Row, Max),
    Row ins 1..Max.


/*==============================================================================
                         3. DISTINCT NUMBERS CONSTRAINT

For each row (and each column) in the puzzle (excluding headings), there must be 
no duplicated numbers. I.e. Given a row (or column), [Head|Xs], all elements of
Xs must be distinct.

  ============================================================================*/

% all_unique(+Matrix)
% True if and only if unique_row
% holds for all Rows in Matrix
all_unique(Rows):- maplist(unique_row, Rows).

% unique_row(+Vars)
% Ensures all numbers in a row satisfy the unique number constraint
unique_row(Row):- all_distinct(Row).


/*==============================================================================
                       2. UNIQUE DIAGONAL CONSTRAINT
		       
Numbers along the diagonal of the non-heading section of the Puzzle must also 
be distinct.

  ============================================================================*/

% diagonal_unique(+Matrix)
% Ensures the diagonal Puzzle, not including the
% heading row, and heading column, is unifiable. 
diagonal_unique(Puzzle):-
    diagonal(Puzzle, Diag),
    all_distinct(Diag).

% diagonal(+Matrix, -List)
% True if and only if List 
% is the diagonal of Matrix
diagonal([], []).
diagonal([[X|_]|Rows], [X|Xs]):-
    maplist(tail, Rows, RowTails),
    diagonal(RowTails, Xs).

% tail(?List, ?Tail)
tail([_|Xs], Xs).

/*==============================================================================
                            PUZZLE GROUNDEDNESS   
  ============================================================================*/

% ground_puzzle(+Matrix) 
% Holds when label from CLP(FD), has given values to row variables within the 
% domain defined in constraint 1. This substantially reduces the number of 
% solutions, thus reducing run time.  
ground_puzzle(Puzzle):- maplist(label, Puzzle).