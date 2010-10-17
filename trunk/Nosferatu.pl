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
                [0,0,1,0,0,0,0],
                [0,0,0,0,0,0,0],
                [9,0,0,0,0,0,9],
                [9,9,0,0,0,9,9]
               ]).

%%% Inicio do programa
start:- welcome,
        menu_start,
        estadoInicial(Tab),
        mostra_tabuleiro(Tab),
        pede_casa(X,Y),
        conteudo_casa(X,Y,V,Tab)
        write(V).

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
       write('**********************************'),nl,nl,
       write('faca a sua escolha'),nl,repeat, get_code(Op), Op>=49, Op=<51,
       menu_lvl,
       write('faca a sua escolha'),nl,repeat, get_code(Op), Op>=49, Op=<51,
       tipo_jogo(Op, Pl1, Pl2),
       assert(jogador(1, Pl1)),
       assert(jogador(2, Pl2)).

%% define o tipo de jogo
%% tipo_jogo(Op, Pl1, Pl2)
tipo_jogo(49, humano, humano).
tipo_jogo(50, humano, computador).
tipo_jogo(51, computador, computador).

menu_lvl:-
       write('**********************************'),nl,
       write('*                                *'),nl,
       write('*         Nivel do Jogo          *'),nl,
       write('*                                *'),nl,
       write('*            1.Easy              *'),nl,
       write('*           2.Normal             *'),nl,
       write('*            3.Hard              *'),nl,
       write('*                                *'),nl,
       write('**********************************'),nl,nl.
       
       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% METODOS AUXILIARES AO JOGO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%pede casa
pede_casa(X,Y):- write('linha '), read(SY), Y is SY-1,
                 write('coluna '), get_code(SX), conv(SX,X).
                 
conv(Let,Valor):- maiuscula(Let), Valor is Let-65.
conv(Let,Valor):- minuscula(Let), Valor is Let-97.
conv(Let,Valor):- numero(Let), Valor is Let-49.

maiuscula(Let):- Let>=65, Let=<72.
minuscula(Let):- Let>=97, Let=<104.
numero(Let):- Let>=48, Let=<56.

% posicao(X,Y,Valor,Tab) verifica o valor da casa X,Y
conteudo_casa(X,0,Valor,[H|_]):- conteudo_casa_linha(X,Valor,H).
conteudo_casa(X,Y,Valor,[_|T]):- Y>0, Y2 is Y-1, conteudo_casa(X,Y2,Valor,T).

conteudo_casa_linha(0,Valor,[H|_]):- Valor is H.
conteudo_casa_linha(X,Valor,[_|T]):- X>0, X2 is X-1, conteudo_casa_linha(X2,Valor,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% VISUALIZAÇÃO DO TABULEIRO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


mostra_tabuleiro(Tab):- write('    A   B   C   D   E   F   G '),nl,
                 linhas(1,Tab),
                 write('    A   B   C   D   E   F   G '),nl,nl,!.

linhas(_,[]):-!.
%primeira linha
linhas(N,[H|T]):- N=:=1, lim(1), write(N), write('  '),
                  linha(H),
                  write(' '), write(N), nl,
                  N2 is N+1,
                  linhas(N2, T).
%segunda linha
linhas(N,[H|T]):- N=:=2, lim(2), write(N), write('  '),
                  linha(H),
                  write(' '), write(N), nl,
                  N2 is N+1,
                  linhas(N2, T).
%linhas do meio
linhas(N,[H|T]):- N>2, N<6, lim(3), write(N), write(' |'),
                  linha(H),
                  write(' '), write(N), nl,
                  N2 is N+1,
                  linhas(N2, T).
%penultima linha
linhas(N,[H|T]):- N=:=6, lim(3), write(N), write('  '),
                  linha(H),
                  write(' '), write(N), nl,
                  N2 is N+1,
                  linhas(N2, T).
%ultima linha
linhas(N,[H|T]):- N=:=7, lim(2), write(N), write('  '),
                  linha(H),
                  write(' '), write(N), nl, lim(1),
                  N2 is N+1,
                  linhas(N2, T).

linha([]):-!.
linha([9|[9|T]]):- write('    '), linha([9|T]).
linha([9|[H|T]]):- H=\=9, write('   |'), linha([H|T]).
linha([9|T]):- write('    '), linha(T).
linha([H|T]):- H=\=9, escreve(H), linha(T).


lim(1):- write('          +---+---+---+'), nl.
lim(2):- write('      +---+---+---+---+---+'), nl.
lim(3):- write('  +---+---+---+---+---+---+---+'), nl.

%escreve(N)
escreve(0):-write('   |').   %vazia
escreve(1):-write(' o |').   %aldeao
escreve(2):-write(' $ |').   %vampiro
escreve(3):-write(' N |').   %nosferatu





