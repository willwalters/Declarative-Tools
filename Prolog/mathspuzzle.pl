% File:    mathspuzzle
% Author:  William Walters 
% Purpose: Solves a "Maths Puzzle" of size n x n, subject to logical constraints
%
% Copyright Will Walters 2019*  
%     *I declare no authorship of the puzzle, only the implementation below
%
% This program solves Maths Puzzles. 
% Maths puzzles are matrices that must adhere to a set of constraints
% These constraints have been outlined below. 
% The program is separated into headings with associated predicates directly 
% below these headings. 
%
% By ensuring that all of these constraints are met and that solutions are made
% ground, this program is able to solve Maths Puzzles of varying shapes

:- use_module(library(apply)).
:- ensure_loaded(library(clpfd)).


/*==============================================================================
                              MATHS PUZZLE SOLVER

puzzle_solution/1 assumes that the Puzzle provided is a valid puzzle. That is,
The Puzzle given has valid headings and all non-heading numbers are instantiated
but possibly non-ground numbers. 
All other predicates below are "helper" predicates that puzzle_solution/1
depends upon. 

  ============================================================================*/

% puzzle_solution(+Matrix)
% Ensures that a solvable Puzzle is solved.
% I.e. filled with ground numbers according 
% to the constraints outlined below.
puzzle_solution(Puzzle):-
    num_domain(Puzzle),
    unify_diagonal(Puzzle),
    transpose(Puzzle, PuzzleT),
    all_unique(Puzzle),
    all_unique(PuzzleT),
    valid_headings(Puzzle), 
    valid_headings(PuzzleT), 
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
    Puzzle = [_|Rows],
    maplist(valid_nums, Rows).   

% valid_nums(+Vars)
% Ensures all numbers in a row satisfy the domain constraint
valid_nums([_|Row]):- Row ins 1..9.


/*==============================================================================
                       2. UNIFIABLE DIAGONAL CONSTRAINT
		       
Numbers along the diagonal of the non-heading section of the Puzzle must be 
identical. That is, prolog must be able to unify them.
The non-heading section of Puzzle, is the Puzzle without the first row and first
column.

  ============================================================================*/

% unify_diagonal(+Matrix)
% Ensures the diagonal Puzzle, not including the
% heading row, and heading column, is unifiable. 
unify_diagonal(Puzzle):-
    diagonal(Puzzle, [_,X|Diag]),
    maplist(#=(X), Diag).

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
                         3. DISTINCT NUMBERS CONSTRAINT

For each row (and each column) in the puzzle (excluding headings), there must be 
no duplicated numbers. I.e. Given a row (or column), [Head|Xs], all elements of
Xs must be distinct.

  ============================================================================*/

% all_unique(+Matrix)
% True if and only if unique_row
% holds for all Rows in Matrix
all_unique([_|Rows]):- maplist(unique_row, Rows).

% unique_row(+Vars)
% Ensures all numbers in a row satisfy the unique number constraint
unique_row([_|Row]):- all_distinct(Row).


 /*==============================================================================
                         4. PRODUCT OR SUM CONSTRAINT

For each row (and each column) in the puzzle, the heading of that row must equal
either sum of that rows numbers or its product. I.e. Given a row (or column), 
[Head|Xs], either Head = sum(Xs) or Head = product(Xs).

   ============================================================================*/

% valid_headings(+Matrix)
% True if and only if valid_row_heading
% holds for all Rows in Matrix
valid_headings([_|Rows]):- maplist(valid_row_heading, Rows).

% valid_row_heading(+List)
% True if and only if H is the sum of the
% elements in row, or the product.
valid_row_heading([H|Row]):-
    sum(Row, #=, H);
    product(Row, H).

% product(+List, ?Expr)
% The product of the elements of List is Expr.
product([X], X).
product([X|Xs], Prod) :-
    Prod #= X*P,
    product(Xs, P).


/*==============================================================================
                            PUZZLE GROUNDEDNESS   
  ============================================================================*/

% ground_puzzle(+Matrix) 
% Holds when label from CLP(FD), has given values to row variables within the 
% domain defined in constraint 1. This substantially reduces the number of 
% solutions, thus reducing run time.  
ground_puzzle([_|Rows]):- maplist(label, Rows).