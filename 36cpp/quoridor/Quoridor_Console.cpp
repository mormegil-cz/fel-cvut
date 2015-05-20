#include <iostream>
#include <cstdio>
#pragma hdrstop

#include "myassert.h"
#include "GameCore.h"
#include "Player_HumanConsole.h"
#include "AI.h"

using namespace std;

char pnames_buff[2][50];
char *pnames[2] = { pnames_buff[0], pnames_buff[1] };

static void move_notifier(const GameState &gs, const Move &m)
{
        if (gs.white_playing)
                cout << pnames[0];
        else
                cout << pnames[1];
        cout << ' ' << m << ".\n";
}

static Player *get_Player(int num)
{
        cout << "Player " << (num+1) << "'s name (empty for AI Player): ";
        fgets(pnames[num], sizeof(pnames_buff[num])-1, stdin);
        pnames[num][sizeof(pnames_buff[num])-1] = '\0';
        // trim right whitespace
        for (char *p = pnames[num] + strlen(pnames[num]) - 1; p >= pnames[num]; p--)
          if (*p <= ' ') *p = '\0';
          else break;
        if (*pnames[num]) return new Player_HumanConsole();
        else {
                char snum[5];
                itoa(num, snum+1, 10);
                Player_AI *p = new Player_AI();
                cout << "Search depth? ";
                cin >> p->search_depth;
                p->search_time = 20000;
                strcpy(pnames[num], "AI Player ");
                strcat(pnames[num], snum);
                return p;
        }
}

int main(int, char**)
{
        Player *p1 = get_Player(0);
        Player *p2 = get_Player(1);
        /*
        Player *p1 = new Player_AI();
        static_cast<Player_AI*>(p1)->search_depth = 2;
        static_cast<Player_AI*>(p1)->search_time = 20000;
        Player *p2 = new Player_HumanConsole();
        strcpy(pnames[0], "AI");
        strcpy(pnames[1], "Player");
        */

        PlayersVector players;
        players.insert(players.end(), p1);
        players.insert(players.end(), p2);

        Quoridor_Game game(players.begin(), players.end());

        game.move_notifier = move_notifier;

        if (game.go()) cout << "Congratulations, " << pnames[0] << "!\n";
        else cout << "Congratulations, " << pnames[1] << "!\n";

        return 0;
}

