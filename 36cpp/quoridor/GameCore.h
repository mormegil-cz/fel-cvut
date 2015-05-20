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

#ifndef GameCoreH
#define GameCoreH

#include <ostream>
#include <iterator>
#include <vector>

#define BOARD_SIZE 9

enum Direction {
        UP    = 1,
        LEFT  = 2,
        DOWN  = 4,
        RIGHT = 8
};

inline Direction opposite(Direction d)
{
        // the opposite direction
        return Direction(((d | (d << 4)) >> 2) & 0xf);
}

class Move;
class GameState;

class Player {
public:
        virtual ~Player() { };

        virtual ::Move* choose_move(const GameState &gs) = 0;
};

typedef std::vector<Player*> PlayersVector;
typedef PlayersVector::iterator PlayerListIterator;

struct GameState {
        enum BarrierPoint {
                EMPTY, HORIZ_WHITE, VERT_WHITE, HORIZ_BLACK, VERT_BLACK
        };
        int   player_x[2];
        int   player_y[2];
        int   barriers_remaining[2];
        unsigned short board[BOARD_SIZE][BOARD_SIZE];
        BarrierPoint points[BOARD_SIZE-1][BOARD_SIZE-1];
        bool  white_playing;

        enum GameResult { NOT_OVER, WHITE_WON, BLACK_WON };

        GameState();

        GameResult game_result() const;
        bool is_valid() const;

        friend std::ostream &operator << (std::ostream &f, const GameState &gs);
};

class Quoridor_Game {
        PlayersVector players;
        GameState gs;
public:
        typedef notifier_t (void (*)(const GameState&));

        void (*move_notifier)(const GameState&, const ::Move&);
        void (*update_notifier)(const GameState&);

        Quoridor_Game(PlayerListIterator players_start, PlayerListIterator players_end);

        const GameState& get_gamestate() const { return gs; }
        bool go();      // Returns whether white won
};

class Move {
protected:
        static bool exists_path_after(GameState, const Move&);
        virtual std::ostream& print_to(std::ostream&) const = 0;
public:
        virtual bool is_valid_for(const GameState &gs) const = 0;
        virtual void apply_to(GameState &gs) const = 0;
        virtual void unapply_to(GameState &gs) const = 0;

        friend std::ostream& operator << (std::ostream&, const Move&);
};

class Pass_Move : public Move {
protected:
        virtual std::ostream& print_to(std::ostream&) const;
public:
        virtual bool is_valid_for(const GameState &gs) const;
        virtual void apply_to(GameState &gs) const;
        virtual void unapply_to(GameState &gs) const;
};

class PutHBarrier_Move : public Move {
protected:
        int x, y;       // left corner (with smaller x)

        virtual std::ostream& print_to(std::ostream&) const;
public:
        PutHBarrier_Move(int x, int y) : x(x), y(y) { }
        virtual bool is_valid_for(const GameState &gs) const;
        virtual void apply_to(GameState &gs) const;
        virtual void unapply_to(GameState &gs) const;

        inline int get_x() const { return x; }
        inline int get_y() const { return y; }
};

class PutVBarrier_Move : public Move {
protected:
        int x, y;       // lower corner (with smaller y)

        virtual std::ostream& print_to(std::ostream&) const;
public:
        PutVBarrier_Move(int x, int y) : x(x), y(y) { }
        virtual bool is_valid_for(const GameState &gs) const;
        virtual void apply_to(GameState &gs) const;
        virtual void unapply_to(GameState &gs) const;

        inline int get_x() const { return x; }
        inline int get_y() const { return y; }
};

class MoveFigure_Move : public Move {
private:
        static signed char dx[9];
        static signed char dy[9];
protected:
        Direction dir;

        static inline int deltax(Direction d) {
                return dx[d];
        }
        static inline int deltay(Direction d) {
                return dy[d];
        }

        virtual std::ostream& print_to(std::ostream&) const;
public:
        MoveFigure_Move(Direction dir) : dir(dir) { }
        virtual bool is_valid_for(const GameState &gs) const;
        virtual void apply_to(GameState &gs) const;
        virtual void unapply_to(GameState &gs) const;

        bool should_jump(const GameState &gs) const;

        inline Direction get_dir() const { return dir; }
};

class JumpFigure_Move : public MoveFigure_Move {
protected:
        Direction turn_dir;

        virtual std::ostream& print_to(std::ostream&) const;
public:
        JumpFigure_Move(Direction dir, Direction turn_dir) : MoveFigure_Move(dir), turn_dir(turn_dir) { }
        virtual bool is_valid_for(const GameState &gs) const;
        virtual void apply_to(GameState &gs) const;
        virtual void unapply_to(GameState &gs) const;

        inline Direction get_turn_dir() const { return turn_dir; }
};

#endif

