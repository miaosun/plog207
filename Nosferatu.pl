%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%   Nosferatu em Prolog  %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%  Gaspar Furtado  &  Miao Sun   %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  Grupo207  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%  PLOG  MIEIC 2010/2011  %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(lists)).

peca(v, ' ').
peca(o, 'O').
peca(x, 'X').
peca(n, 'N').

print_cell(v):-write(' ').
print_cell(o):-write('O').
print_cell(x):-write('X').
print_cell(n):-write('N').

mainmenu:-
       write('**********************************'),nl,
           write('*                                *'),nl,
       write('*     Bemvindo ao Nosferatu      *'),nl,
           write('*                                *'),nl,
       write('**********************************'),nl,nl.

modJogo:-
       write('**********************************'),nl,
           write('*                                *'),nl,
       write('*     Escolhe o mode do jogo:    *'),nl,
       write('*                                *'),nl,
       write('*       1.Humano VS Humano       *'),nl,
       write('*     2.Humano VS Computador     *'),nl,
       write('*   3.Computador VS Computador   *'),nl,
           write('*                                *'),nl,
       write('**********************************'),nl,nl.

nivelJogo:-
       write('**********************************'),nl,
           write('*                                *'),nl,
       write('*         Nivel do Jogo          *'),nl,
       write('*                                *'),nl,
       write('*            1.Easy              *'),nl,
       write('*           2.Normal             *'),nl,
       write('*            3.Hard              *'),nl,
           write('*                                *'),nl,
       write('**********************************'),nl,nl.

tabuleiro1:-
               %write('        Tabuleiro de Jogo        '),nl,
               %write('                                 '),nl,
                write('    A   B   C   D   E   F   G    '),nl,
                write('          +---+---+---+          '),nl,
                write('1         |   |   |   |         1'),nl,
                write('      +---+---+---+---+---+      '),nl,
                write('2     |   |   |   |   |   |     2'),nl,
                write('  +---+---+---+---+---+---+---+  '),nl,
                write('3 |   |   |   |   |   |   |   | 3'),nl,
                write('  +---+---+---+---+---+---+---+  '),nl,
                write('4 |   |   |   |   |   |   |   | 4'),nl,
                write('  +---+---+---+---+---+---+---+  '),nl,
                write('5 |   |   |   |   |   |   |   | 5'),nl,
                write('  +---+---+---+---+---+---+---+  '),nl,
                write('6     |   |   |   |   |   |     6'),nl,
                write('      +---+---+---+---+---+      '),nl,
                write('7         |   |   |   |         7'),nl,
                write('          +---+---+---+          '),nl,
                write('    A   B   C   D   E   F   G    '),nl,nl.

% V representa estado vazio.

estadoInicial([     [v,v,v],
                  [v,v,v,v,v],
                [v,v,v,v,v,v,v],
                [v,v,v,v,v,v,v],
                [v,v,v,v,v,v,v],
                  [v,v,v,v,v],
                    [v,v,v]
               ]).

% X representa peca preta(Vampiro), O representa peca branca(aldeao), e N peca especial(Nosferatu)


possivelEstado([    [v,z,v],
                  [v,x,d,v,o],
                [v,v,v,w,v,v,v],
                [n,x,v,q,v,o,v],
                [v,x,v,o,o,o,v],
                  [v,v,o,o,v],
                    [v,o,o]
                ]).

                          
inicio:-
       % mainmenu, modJogo, nl,nl,nivelJogo,
           nl,nl,tabuleiro.

tabuleiro(Tab):- write('   A   B   C   D   E   F   G   '),nl,
                 linhas([Tab]),
                 write('   A   B   C   D   E   F   G   '),nl.

linhas([]).
linhas([H|T]):- write(' +---+---+---+---+---+---+---+ '), nl, linha(T).

linha([]).
linha([H|T]):- write(H), write('  '), linhas(T).

