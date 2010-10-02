%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%   Nosferatu em Prolog  %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%  Gaspar Furtado  &  Miao Sun   %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  Grupo207  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%  PLOG  MIEIC 2010/2011  %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(lists)).

mainmenu:-
       write('*******************************************'),nl,
	   write('*                                         *'),nl,
       write('*          Bemvindo ao Nosferatu          *'),nl,
	   write('*                                         *'),nl,
       write('*******************************************'),nl,nl.

modJogo:-
       write('*******************************************'),nl,
	   write('*                                         *'),nl,
       write('*         Escolhe o mode do jogo:         *'),nl,
       write('*                                         *'),nl,
       write('*           1.Humano VS Humano            *'),nl,
       write('*         2.Humano VS Computador          *'),nl,
       write('*       3.Computador VS Computador        *'),nl,
	   write('*                                         *'),nl,
       write('*******************************************'),nl.

nivelJogo:-
       write('*******************************************'),nl,
	   write('*                                         *'),nl,
       write('*              Nivel do Jogo              *'),nl,
       write('*                                         *'),nl,
       write('*                1.Easy                   *'),nl,
       write('*               2.Normal                  *'),nl,
       write('*                3.Hard                   *'),nl,
	   write('*                                         *'),nl,
       write('*******************************************'),nl,nl.

inicio:-
        mainmenu, modJogo, nivelJogo.
