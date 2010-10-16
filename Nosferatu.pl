%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%   Nosferatu em Prolog  %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%  Gaspar Furtado  &  Miao Sun   %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  Grupo207  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%  PLOG  MIEIC 2010/2011  %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(lists)).

%% Estado inicial do tabuleiro
% V representa estado vazio.
% 9 - espaço
% 0 - vazia
% 1 - aldeao
% 2 - vampiro
% 3 - nosferatu
estadoInicial([ [9,9,0,0,0,9,9],
                [9,0,0,0,0,0,9],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0],
                [9,0,0,0,0,0,9],
                [9,9,0,0,0,9,9]
               ]).

%%% Inicio do programa
start:- welcome,
        %write('entrou'),nl,nl,
        %menu_start,
        estadoInicial(Tab),
        mostra_tabuleiro(Tab).

%% Welcome %%
welcome:-
       write('**********************************'),nl,
       write('*                                *'),nl,
       write('*     Bemvindo ao Nosferatu      *'),nl,
       write('*                                *'),nl,
       write('**********************************'),nl,nl.
%% menu inicial
menu_start:-
       write('**********************************'),nl,
       write('*                                *'),nl,
       write('*     Escolhe o mode do jogo:    *'),nl,
       write('*                                *'),nl,
       write('*       1.Humano VS Humano       *'),nl,
       write('*     2.Humano VS Computador     *'),nl,
       write('*   3.Computador VS Computador   *'),nl,
       write('*                                *'),nl,
       write('**********************************'),nl,nl.
 %      repeat, get_code(Op), Op>=49, Op=<51,
  %     tipo_jogo(Op, Pl1, Pl2),
   %    assert(jogador(1, Pl1)),
    %   assert(jogador(2, Pl2)).

%% define o tipo de jogo
%% tipo_jogo(Op, Pl1, Pl2)
%tipo_jogo(49, humano, humano).
%tipo_jogo(50, humano, computador).
%tipo_jogo(51, computador, computador).

%menu_lvl:-
%       write('**********************************'),nl,
 %          write('*                                *'),nl,
  %     write('*         Nivel do Jogo          *'),nl,
   %    write('*                                *'),nl,
%       write('*            1.Easy              *'),nl,
 %      write('*           2.Normal             *'),nl,
  %     write('*            3.Hard              *'),nl,
   %        write('*                                *'),nl,
%       write('**********************************'),nl,nl.



mostra_tabuleiro(Tab):- write('   A   B   C   D   E   F   G '),nl,
                 linhas(1,Tab),
                 write('   A   B   C   D   E   F   G '),nl.

linhas(_,[]).
%primeira linha
linhas(N,[ [H|[H2|T2]] | T]):- H=:=9, H2=:=9, N=\=7, lim(1), write(N), write(' '),
                  linha([H|[H2|T2]]),
                  write(' '), write(N), nl,
                  N2 is N+1,
                  linhas(N2, T).
%ultima linha
linhas(N,[ [H|[H2|T2]] | T]):- H=:=9, H2=:=9, N=:=7, lim(2), write(N), write(' '),
                  linha([H|[H2|T2]]),
                  write(' '), write(N), nl, lim(1),
                  N2 is N+1,
                  linhas(N2, T).
%segunda linha
linhas(N,[ [H|T] | T2]):- H=:=9, T=\=9, N=\=6, lim(2), write(N), write(' '),
                  linha(H),
                  write(' '), write(N), nl,
                  N2 is N+1,
                  linhas(N2, T2).
%penultima linha
linhas(N,[ [H|T] | T2]):- H=:=9, T=\=9, N=:=6, lim(3), write(N), write(' '),
                  linha(H),
                  write(' '), write(N), nl,
                  N2 is N+1,
                  linhas(N2, T2).
%linhas do meio
linhas(N,[H|T]):- lim(3), write(N), write(' '),
                  linha(H),
                  write(' '), write(N), nl,
                  N2 is N+1,
                  linhas(N2, T).

linha([]).
linha([9|[9|T]]):- write('    '), linha([9|T]).
linha([9|[H|T]]):- H =\= 9, write('   |'), linha([H|T]).
linha([H|T]):- H=\=9, escreve(H), linha(T).


lim(1):- write('         +---+---+---+'), nl.
lim(2):- write('     +---+---+---+---+---+'), nl.
lim(3):- write(' +---+---+---+---+---+---+---+'), nl.
lim(_).

%escreve(N)
escreve(0):-write('   |').   %vazia
escreve(1):-write(' o |').   %aldeao
escreve(2):-write(' $ |').   %vampiro
escreve(3):-write(' N |').   %nosferatu


