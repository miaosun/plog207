%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%   Nosferatu em Prolog  %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%  Gaspar Furtado  &  Miao Sun   %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  Grupo207  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%  PLOG  MIEIC 2010/2011  %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(lists)).

mainmenu:-
       write('*******************************************'),nl.
       write('***        Bemvindo ao Nosferatu        ***'),nl.
       write('*******************************************'),nl,nl.
       
modJogo:-
       write('*******************************************'),nl.
       write('*         Escolhe o mode do jogo:         *'),nl.
       write('*                                         *'),nl.
       write('*             Humano VS Humano            *'),nl.
       write('*           Humano VS Computador          *'),nl.
       write('*         Computador VS Computador        *'),nl.
       write('*******************************************'),nl.
       
nivelJogo:-
       write('*******************************************'),nl.
       write('*              Nivel do Jogo              *'),nl.
       write('*                                         *'),nl.
       write('*                  Easy                   *'),nl.
       write('*                 Normal                  *'),nl.
       write('*                  Hard                   *'),nl.
       write('*******************************************'),nl.