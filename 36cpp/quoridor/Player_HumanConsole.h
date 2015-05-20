#ifndef Player_HumanConsoleH
#define Player_HumanConsoleH

#include "GameCore.h"

extern char *pnames[2];

class Player_HumanConsole : public Player {
public:
        virtual Move* choose_move(const GameState &gs);
};

#endif

