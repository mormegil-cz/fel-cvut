#include <iostream>
#include <locale>
#pragma hdrstop

#include "myassert.h"
#include "Player_HumanConsole.h"

//---------------------------------------------------------------------------

using namespace std;

Direction read_Direction(istream &in, ostream &out, const char *prompt)
{
  char d;
  do {
      out << prompt;
      in >> d;
      d = char(toupper(d));
  } while (d != 'U' && d != 'D' && d != 'R' && d != 'L');
  switch (d) {
    case 'U': return UP;
    case 'D': return DOWN;
    case 'R': return RIGHT;
    case 'L': return LEFT;
  }
  assert(!"read_Direction");
  return UP;  // just to remove the "function should return value"
}

static Move* get_move(const GameState &gs)
{
  cout << gs;
  char move_type;
  do {
        cout << '\n';
        if (gs.white_playing)
                cout << pnames[0] << " (@) : ";
        else
                cout << pnames[1] << " (%) : ";
        cout << "Put (B)arrier, (M)ove figure, or (P)ass move? ";
        cin >> move_type;
        move_type = char(toupper(move_type));
        if (move_type == 'Q' || move_type == '\x1b') {
          cout << "OK, exiting. Bye!\n";
          exit(0);
        }
  } while (move_type != 'B' && move_type != 'M' && move_type != 'P');
  if (move_type == 'B') {
    // Put barrier
    char t;
    do {
        cout << "(H)orizontal, or (V)ertical? ";
        cin >> t;
        t = char(toupper(t));
    } while (t != 'H' && t != 'V');
    char x;
    int y;
    do {
        cout << "Where? ";
        cin >> x >> y;
        x = char(tolower(x));
    } while (x < 'a' || x > 'i' || y < 1 || y > 9);
    if (t == 'H')
        return new PutHBarrier_Move(x - 'a', y - 1);
    else
        return new PutVBarrier_Move(x - 'a' + 1, y - 2);
  } else if (move_type == 'M') {
    // Move figure
    Direction dir = read_Direction(cin, cout, "Direction? (Up, Down, Right, Left) ");
    MoveFigure_Move *m = new MoveFigure_Move(dir);
    if (m->should_jump(gs)) {
        delete m;
        Direction td = read_Direction(cin, cout, "Jump direction? (Up, Down, Right, Left) ");
        m = new JumpFigure_Move(dir, td);
    }
    return m;
  } else return new Pass_Move();
}

Move* Player_HumanConsole::choose_move(const GameState &gs)
{
  Move *result;
  while(1) {
    result = get_move(gs);
    if (result->is_valid_for(gs)) return result;
    delete result;
    cout << "Invalid move!\n";
  }
}

