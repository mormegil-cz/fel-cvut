//  Board game ``Quoridor''
//  Copyright (C) 2003  Petr Kadlec <mormegil@centrum.cz>
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#include <ostream>
#include <stdexcept>
#include "myassert.h"
#include <cstdlib>
#pragma hdrstop

#include "GameCore.h"

//---------------------------------------------------------------------------

using namespace std;

static const char *dir_name[9] = { 0, "up", "left", 0, "down", 0,0,0, "right" };

class invalid_move_exception : public logic_error {
public:
        invalid_move_exception(const char *msg): logic_error(msg) { }
};

Quoridor_Game::Quoridor_Game(PlayerListIterator players_start, PlayerListIterator players_end) :
        players(players_start, players_end), update_notifier(0), move_notifier(0)
{
}

bool Quoridor_Game::go()
{
        while (gs.game_result() == GameState::NOT_OVER) {
                for (PlayerListIterator player = players.begin(); player < players.end() && gs.game_result() == GameState::NOT_OVER; player++) {
                        Move *move = (*player)->choose_move(gs);
                        assert(move);
                        if (move_notifier)
                                move_notifier(gs, *move);
                        if (!move->is_valid_for(gs))
                                throw invalid_move_exception("This move is invalid");
                        move->apply_to(gs);
                        if (update_notifier)
                                update_notifier(gs);
                }
        };
        return (gs.game_result() == GameState::WHITE_WON);
}
 
GameState::GameState()
{
        player_x[0] = player_x[1] = BOARD_SIZE/2;
        player_y[0] = 0;
        player_y[1] = BOARD_SIZE-1;
        barriers_remaining[0] = barriers_remaining[1] = 10;
        for (int x = 1; x < BOARD_SIZE-1; x++)
          for (int y = 1; y < BOARD_SIZE-1; y++)
            board[x][y] = UP | LEFT | DOWN | RIGHT;

        for (int w = 1; w < BOARD_SIZE-1; w++) {
                board[0][w] = UP | DOWN | RIGHT;
                board[BOARD_SIZE-1][w] = UP | DOWN | LEFT;
                board[w][0] = LEFT | RIGHT | UP;
                board[w][BOARD_SIZE-1] = LEFT | RIGHT | DOWN;
        }
        board[0][0]                     = RIGHT | UP;
        board[BOARD_SIZE-1][0]            = LEFT | UP;
        board[0][BOARD_SIZE-1]            = RIGHT | DOWN;
        board[BOARD_SIZE-1][BOARD_SIZE-1]   = LEFT | DOWN;

        for (int x = 0; x < BOARD_SIZE-1; x++)
          for (int y = 0; y < BOARD_SIZE-1; y++)
            points[x][y] = EMPTY;

        white_playing = true;
}

GameState::GameResult GameState::game_result() const
{
        if (player_y[0] == BOARD_SIZE-1) return WHITE_WON;
        if (player_y[1] == 0) return BLACK_WON;
        return NOT_OVER;
}

static void recursive_path_check(const GameState &gs, bool visited[], int x, int y)
{
        assert(x >= 0 && x < BOARD_SIZE && y >= 0 && y < BOARD_SIZE);
        visited[x + y * BOARD_SIZE] = true;
        if (gs.board[x][y] & RIGHT && !visited[x+1 + y*BOARD_SIZE])
                recursive_path_check(gs, visited, x+1, y);
        if (gs.board[x][y] & LEFT && !visited[x-1 + y*BOARD_SIZE])
                recursive_path_check(gs, visited, x-1, y);
        if (gs.board[x][y] & UP && !visited[x + (y+1)*BOARD_SIZE])
                recursive_path_check(gs, visited, x, y+1);
        if (gs.board[x][y] & DOWN && !visited[x + (y-1)*BOARD_SIZE])
                recursive_path_check(gs, visited, x, y-1);
}

bool GameState::is_valid() const
{
        if (player_x[0] < 0 || player_x[0] >= BOARD_SIZE || player_y[0] < 0 || player_y[0] >= BOARD_SIZE ||
            player_x[1] < 0 || player_x[1] >= BOARD_SIZE || player_y[1] < 0 || player_y[1] >= BOARD_SIZE) return false;
        if (barriers_remaining[0] < 0 || barriers_remaining[1] < 0) return false;

        bool visited[BOARD_SIZE * BOARD_SIZE];
        bool path_found;
        memset(visited, 0, sizeof(visited));
        recursive_path_check(*this, visited, player_x[0], player_y[0]);
        path_found = false;
        for (int x = 0; x < BOARD_SIZE; x++)
          if (visited[x + BOARD_SIZE * (BOARD_SIZE-1)]) {
                path_found = true;
                break;
          }
        if (!path_found) return false;
        memset(visited, 0, sizeof(visited));
        recursive_path_check(*this, visited, player_x[1], player_y[1]);
        path_found = false;
        for (int x = 0; x < BOARD_SIZE; x++)
          if (visited[x]) {
                path_found = true;
                break;
          }
        return path_found;
}

ostream &operator << (ostream &f, const GameState &gs)
{
  static char point_char[5] = { ' ', '-', '|', '=', '!' };
  int x1 = gs.player_x[0];
  int y1 = gs.player_y[0];
  int x2 = gs.player_x[1];
  int y2 = gs.player_y[1];
  f << ' ';
  for (int i = 0; i < BOARD_SIZE; i++)
     f << "   " << char(i + 'a');
  f << "\n +";
  for (int i = 0; i < BOARD_SIZE; i++)
     f << "----";
  f << "-+\n";
  for (int y = BOARD_SIZE-1; y >= 0; y--) {
        f << char (y + '1') << "| ";
        for (int x = 0; x < BOARD_SIZE; x++) {
                if (x == x1 && y == y1) f << " @ ";
                else if (x == x2 && y == y2) f << " % ";
                else f << "[ ]";
                //else cout << hex << gs.board[x][y] << dec;

                if (x < BOARD_SIZE-1 && !(gs.board[x][y] & RIGHT)) f << '|';
                else f << ' ';
        }
        f << '|' << char (y + '1');
        if (y == BOARD_SIZE/2) {
                f << '\t';
                if (gs.white_playing) f << '[';
                f << gs.barriers_remaining[0];
                if (gs.white_playing) f << "] / ";
                else f << " / [";
                f << gs.barriers_remaining[1];
                if (!gs.white_playing) f << ']';
        }
        f << '\n';
        if (y > 0) {
                f << " | ";
                for (int x = 0; x < BOARD_SIZE; x++) {
                        if (!(gs.board[x][y] & DOWN)) f << "---";
                        else f << "   ";

                        if (x < BOARD_SIZE-1) {
                                if (y < BOARD_SIZE)
                                        f << point_char[gs.points[x][y-1]];
                                        else f << ' ';
                        }
                }
                f << " |\n";
        }
  }
  f << " +";
  for (int i = 0; i < BOARD_SIZE; i++)
     f << "----";
  f << "-+\n ";
  for (int i = 0; i < BOARD_SIZE; i++)
     f << "   " << char(i + 'a');
  f << '\n';

  return f;
}

ostream& operator << (ostream &f, const Move &m)
{
        return m.print_to(f);
}

bool Move::exists_path_after(GameState gs, const Move &m)
{
        m.apply_to(gs);
        return gs.is_valid();
}

ostream &Pass_Move::print_to(ostream &f) const
{
        return f << "passes the move";
}

bool Pass_Move::is_valid_for(const GameState &gs) const
{
        return true;
}

void Pass_Move::apply_to(GameState &gs) const
{
        gs.white_playing = !gs.white_playing;
}

void Pass_Move::unapply_to(GameState &gs) const
{
        gs.white_playing = !gs.white_playing;
}

ostream &PutHBarrier_Move::print_to(ostream &f) const
{
        return f << "puts a horizontal barrier at " << char(x + 'a') << char(y + '1');
}

bool PutHBarrier_Move::is_valid_for(const GameState &gs) const
{
        if (x < 0 || x >= BOARD_SIZE-1 || y <= 0 || y >= BOARD_SIZE) return false;      // out of bounds
        if (!gs.barriers_remaining[int(!gs.white_playing)]) return false;               // no barriers remaining
        if (!(gs.board[x][y] & DOWN) || !(gs.board[x+1][y] & DOWN)) return false;       // a barrier is already there
        if (gs.points[x][y-1] != GameState::EMPTY) return false;                        // crossing barrier
        if (!exists_path_after(gs, *this)) return false;                                // no path after placing the barrier
        return true;
}

void PutHBarrier_Move::apply_to(GameState &gs) const
{
        //! MUST BE ALREADY CHECKED BY is_valid_for !
        //! (And unable to check here -- is_valid_for contains apply_to call)
        assert(gs.board[x][y] & DOWN &&
               gs.board[x+1][y] & DOWN &&
               gs.board[x][y-1] & UP &&
               gs.board[x+1][y-1] & UP
        );
        gs.board[x][y] &= ~DOWN;
        gs.board[x+1][y] &= ~DOWN;
        gs.board[x][y-1] &= ~UP;
        gs.board[x+1][y-1] &= ~UP;
        gs.points[x][y-1] = gs.white_playing ? GameState::HORIZ_WHITE : GameState::HORIZ_BLACK;
        --gs.barriers_remaining[int(!gs.white_playing)];
        gs.white_playing = !gs.white_playing;
}

void PutHBarrier_Move::unapply_to(GameState &gs) const
{
        //! MUST be called only on GameState that is just after the corresponding apply_to !!!
        //! No checks are made!
        assert(!(gs.board[x][y] & DOWN) &&
               !(gs.board[x+1][y] & DOWN) &&
               !(gs.board[x][y-1] & UP) &&
               !(gs.board[x+1][y-1] & UP)
        );
        gs.white_playing = !gs.white_playing;
        gs.board[x][y] |= DOWN;
        gs.board[x+1][y] |= DOWN;
        gs.board[x][y-1] |= UP;
        gs.board[x+1][y-1] |= UP;
        gs.points[x][y-1] = GameState::EMPTY;
        ++gs.barriers_remaining[int(!gs.white_playing)];
}

ostream &PutVBarrier_Move::print_to(ostream &f) const
{
        return f << "puts a vertical barrier at " << char(x + 'a' - 1) << char(y + '2');
}

bool PutVBarrier_Move::is_valid_for(const GameState &gs) const
{
        if (x <= 0 || x >= BOARD_SIZE || y < 0 || y >= BOARD_SIZE-1) return false;      // out of bounds
        if (!gs.barriers_remaining[int(!gs.white_playing)]) return false;               // no barriers remaining
        if (!(gs.board[x][y] & LEFT) || !(gs.board[x][y+1] & LEFT)) return false;       // a barrier is already there
        if (gs.points[x-1][y] != GameState::EMPTY) return false;                        // crossing barrier
        if (!exists_path_after(gs, *this)) return false;                                // no path after placing the barrier
        return true;
}

void PutVBarrier_Move::apply_to(GameState &gs) const
{
        //! MUST BE ALREADY CHECKED BY is_valid_for !
        //! (And unable to check here -- is_valid_for contains apply_to call)
        assert(gs.board[x][y] & LEFT &&
               gs.board[x][y+1] & LEFT &&
               gs.board[x-1][y] & RIGHT &&
               gs.board[x-1][y+1] & RIGHT
        );
        gs.board[x][y] &= ~LEFT;
        gs.board[x][y+1] &= ~LEFT;
        gs.board[x-1][y] &= ~RIGHT;
        gs.board[x-1][y+1] &= ~RIGHT;
        gs.points[x-1][y] = gs.white_playing ? GameState::VERT_WHITE : GameState::VERT_BLACK;
        --gs.barriers_remaining[int(!gs.white_playing)];
        gs.white_playing = !gs.white_playing;
}

void PutVBarrier_Move::unapply_to(GameState &gs) const
{
        //! MUST be called only on GameState that is just after the corresponding apply_to !!!
        //! No checks are made!
        assert(!(gs.board[x][y] & LEFT) &&
               !(gs.board[x][y+1] & LEFT) &&
               !(gs.board[x-1][y] & RIGHT) &&
               !(gs.board[x-1][y+1] & RIGHT)
        );
        gs.white_playing = !gs.white_playing;
        gs.board[x][y] |= LEFT;
        gs.board[x][y+1] |= LEFT;
        gs.board[x-1][y] |= RIGHT;
        gs.board[x-1][y+1] |= RIGHT;
        gs.points[x-1][y] = GameState::EMPTY;
        ++gs.barriers_remaining[int(!gs.white_playing)];
}

ostream &MoveFigure_Move::print_to(ostream &f) const
{
        return f << "moves the figure " << dir_name[dir];
}

signed char MoveFigure_Move::dx[9] = { 0, /*UP*/0, /*LEFT*/-1, 0, /*DOWN*/0, 0,0,0, /*RIGHT*/+1};
signed char MoveFigure_Move::dy[9] = { 0, /*UP*/+1, /*LEFT*/0, 0, /*DOWN*/-1, 0,0,0, /*RIGHT*/0};

bool MoveFigure_Move::is_valid_for(const GameState &gs) const
{
        int x = gs.player_x[int(!gs.white_playing)];
        int y = gs.player_y[int(!gs.white_playing)];
        int nx = x + deltax(dir);
        int ny = y + deltay(dir);
        if (nx < 0 || nx >= BOARD_SIZE || ny < 0 || ny >= BOARD_SIZE) return false;     // out of bounds
        if (!(gs.board[x][y] & dir)) return false;                                      // blocked
        if (nx == gs.player_x[int(gs.white_playing)] && ny == gs.player_y[int(gs.white_playing)]) {
                // jump around the other player -- use JumpFigure_Move instead
                return false;
        }
        return true;
}

void MoveFigure_Move::apply_to(GameState &gs) const
{
        //! MUST BE ALREADY CHECKED BY is_valid_for !
        gs.player_x[int(!gs.white_playing)] += deltax(dir);
        gs.player_y[int(!gs.white_playing)] += deltay(dir);
        gs.white_playing = !gs.white_playing;
}

void MoveFigure_Move::unapply_to(GameState &gs) const
{
        //! MUST be called only on GameState that is just after the corresponding apply_to !!!
        //! No checks are made!
        gs.white_playing = !gs.white_playing;
        gs.player_x[int(!gs.white_playing)] -= deltax(dir);
        gs.player_y[int(!gs.white_playing)] -= deltay(dir);
}

bool MoveFigure_Move::should_jump(const GameState &gs) const
{
        // Would this move be invalid AND some jump would be correct?
        int x = gs.player_x[int(!gs.white_playing)];
        int y = gs.player_y[int(!gs.white_playing)];
        int nx = x + deltax(dir);
        int ny = y + deltay(dir);
        if (nx < 0 || nx >= BOARD_SIZE || ny < 0 || ny >= BOARD_SIZE) return false;     // out of bounds
        if (!(gs.board[x][y] & dir)) return false;                                      // blocked
        if (nx == gs.player_x[int(gs.white_playing)] && ny == gs.player_y[int(gs.white_playing)]) {
                // jump around the other player -- use JumpFigure_Move instead
                return true;
        }
        return false;
}

ostream &JumpFigure_Move::print_to(ostream &f) const
{
        return f << "jumps around the opponent's figure and to the " << dir_name[turn_dir];
}

bool JumpFigure_Move::is_valid_for(const GameState &gs) const
{
        if (turn_dir == opposite(dir)) return false;                                    // jump forth-and-back
        int x = gs.player_x[int(!gs.white_playing)];
        int y = gs.player_y[int(!gs.white_playing)];
        if (!(gs.board[x][y] & dir)) return false;                                      // blocked
        int nx = x + deltax(dir);
        int ny = y + deltay(dir);
        if (nx < 0 || nx >= BOARD_SIZE || ny < 0 || ny >= BOARD_SIZE) return false;     // out of bounds
        if (nx != gs.player_x[int(gs.white_playing)] || ny != gs.player_y[int(gs.white_playing)]) {
                // the another player is not here -- use MoveFigure_Move instead
                return false;
        }
        if (dir != turn_dir) {
                // turns allowed only when direct is blocked
                if (gs.board[nx][ny] & dir) return false;
        }
        if (!(gs.board[nx][ny] & turn_dir)) return false;                               // blocked
        nx += deltax(turn_dir);
        ny += deltay(turn_dir);
        if (nx < 0 || nx >= BOARD_SIZE || ny < 0 || ny >= BOARD_SIZE) return false;     // out of bounds
        return true;
}

void JumpFigure_Move::apply_to(GameState &gs) const
{
        //! MUST BE ALREADY CHECKED BY is_valid_for !
        gs.player_x[int(!gs.white_playing)] += deltax(dir) + deltax(turn_dir);
        gs.player_y[int(!gs.white_playing)] += deltay(dir) + deltay(turn_dir);
        gs.white_playing = !gs.white_playing;
}

void JumpFigure_Move::unapply_to(GameState &gs) const
{
        //! MUST be called only on GameState that is just after the corresponding apply_to !!!
        //! No checks are made!
        gs.white_playing = !gs.white_playing;
        gs.player_x[int(!gs.white_playing)] -= deltax(dir) + deltax(turn_dir);
        gs.player_y[int(!gs.white_playing)] -= deltay(dir) + deltay(turn_dir);
}

