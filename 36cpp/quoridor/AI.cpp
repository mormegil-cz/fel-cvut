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

#include <vector>
#include <limits>
#include "myassert.h"
#include <iostream>
#pragma hdrstop

#include "GameCore.h"
#include "AI.h"
#include "Clock.h"

//---------------------------------------------------------------------------

#define TRACE
#undef TRACE

#define INF (std::numeric_limits<float>::infinity())
#define NO_PATH (BOARD_SIZE * BOARD_SIZE * 2)

static unsigned find_shortest_path(const GameState &gs, int x, int y, int desty)
{
        unsigned distance[BOARD_SIZE][BOARD_SIZE];
        for (int x = 0; x < BOARD_SIZE; x++)
          for (int y = 0; y < BOARD_SIZE; y++)
            distance[x][y] = NO_PATH;

        distance[x][y] = 0;
        bool changed;
        do {
          changed = false;
          for (int x = 0; x < BOARD_SIZE; x++)
            for (int y = 0; y < BOARD_SIZE; y++) {
              unsigned nd = distance[x][y] + 1;
              if (nd < NO_PATH) {
                if (x > 0 && gs.board[x][y] & LEFT && distance[x-1][y] > nd) { distance[x-1][y] = nd; changed = true; }
                if (y > 0 && gs.board[x][y] & DOWN && distance[x][y-1] > nd) { distance[x][y-1] = nd; changed = true; }
                if (x < BOARD_SIZE-1 && gs.board[x][y] & RIGHT && distance[x+1][y] > nd) { distance[x+1][y] = nd; changed = true; }
                if (y < BOARD_SIZE-1 && gs.board[x][y] & UP && distance[x][y+1] > nd) { distance[x][y+1] = nd; changed = true; }
              }
            }
        } while (changed);

        unsigned min_dest = distance[0][desty];
        for (int x = 1; x < BOARD_SIZE; x++)
                if (distance[x][desty] < min_dest) min_dest = distance[x][desty];

        return min_dest;
}

float static_evaluator(const GameState &gs)
{
        switch(gs.game_result()) {
        case GameState::WHITE_WON: return +INF;
        case GameState::BLACK_WON: return -INF;
        }

        // Attributes
        int path_length[2] = {
                find_shortest_path(gs, gs.player_x[0], gs.player_y[0], BOARD_SIZE-1),
                find_shortest_path(gs, gs.player_x[1], gs.player_y[1], 0)
        };
        assert(path_length[0] < NO_PATH && path_length[1] < NO_PATH);
        const int *rem_barriers = gs.barriers_remaining;

        //path_length[int(!gs.white_playing)] -= 0.8f;

        float value_path = path_length[1] - path_length[0];
        float value_barriers = rem_barriers[0] - rem_barriers[1];
        float value_turn = 2 * int(gs.white_playing) - 1.0f;

        /*
        if (value_path < 0) return 4*value_path + 4*value_barriers;
        else return 6*value_path + 4*value_barriers;
        */

        if ((value_path > 0) ^ (value_barriers < 0)) {
           value_barriers *= 0.9f;
        } else {
           value_barriers *= 1.1f;
        }

        float result = value_path + value_barriers + value_turn;

        #ifdef TRACE
        std::cout << "################### STATIC EVALUATOR ####################\n";
        std::cout << gs;
        std::cout << "path_length = { " << path_length[0] << ", " << path_length[1] << " }\n";
        std::cout << "rem_barriers = { " << rem_barriers[0] << ", " << rem_barriers[1] << " }\n";
        std::cout << "value_path = " << value_path << '\n';
        std::cout << "value_barriers = " << value_barriers << '\n';
        std::cout << "value_turn = " << value_barriers << '\n';
        std::cout << "VALUE = " << result << '\n';
        std::cout << "#########################################################\n";
        #endif

        /*if (gs.white_playing) return result;
        else return -result;*/
        return result;
}

class MoveGenerator {
        enum { MH_INIT, MH_MOVE, MH_HBARR, MH_VBARR, MH_PASS, MH_END_OF_LIST } type;
        union {
                struct { int x, y; } coords;
                struct { Direction move, jump; } directions;
        } params;
        Move *curr_move;
        const GameState *state;
public:
        MoveGenerator(const GameState &gs) : state(&gs), type(MH_INIT) {
          ++(*this);
        }

        MoveGenerator& operator ++ () {
            while(1) {  // return inside
                curr_move = 0;
                switch(type) {
                case MH_INIT:
                        type = MH_MOVE;
                        params.directions.move = params.directions.jump = RIGHT;
                        break;

                case MH_MOVE:
                    int j;
                    j = (int)params.directions.jump;
                    j >>= 1;
                    params.directions.jump = (Direction)j;
                        if (!j) {
                                params.directions.jump = RIGHT;
                                int m;
                                m = (int)params.directions.move;
                                m >>= 1;
                                params.directions.move = (Direction)m;
                                if (!m) {
                                        type = MH_HBARR;
                                        params.coords.x = BOARD_SIZE-2;
                                        params.coords.y = BOARD_SIZE-1;
                                }
                        }
                        break;

                case MH_HBARR:
                        if (--params.coords.x < 0) {
                                params.coords.x = BOARD_SIZE-2;
                                if (--params.coords.y < 1) {
                                        type = MH_VBARR;
                                        params.coords.x = BOARD_SIZE-1;
                                        params.coords.y = BOARD_SIZE-2;
                                }
                        }
                        break;

                case MH_VBARR:
                        if (--params.coords.y < 0) {
                                params.coords.y = BOARD_SIZE-2;
                                if (--params.coords.x < 1) {
                                        type = MH_PASS;
                                }
                        }
                        break;

                case MH_PASS:
                        type = MH_END_OF_LIST;
                        return *this;

                default:
                        assert(!"Invalid ++MoveGenerator!");
                }

                Move *m = *this;
                if (m->is_valid_for(*state)) {
                        //std::cout << '"' << *m << "\" is a valid move.\n";
                        return *this;
                }
                delete m;
            }
        }

        operator Move*() {
                if (!curr_move)
                        switch(type) {
                        case MH_PASS:
                                curr_move = new Pass_Move();
                                break;

                        case MH_MOVE:
                                curr_move = new MoveFigure_Move(params.directions.move);
                                if (static_cast<MoveFigure_Move *>(curr_move)->should_jump(*state)) {
                                //if (!curr_move->is_valid_for(*state)) {
                                        delete curr_move;
                                        curr_move = 0;
                                        curr_move = new JumpFigure_Move(params.directions.move, params.directions.jump);
                                }
                                break;

                        case MH_HBARR:
                                curr_move = new PutHBarrier_Move(params.coords.x, params.coords.y);
                                break;

                        case MH_VBARR:
                                curr_move = new PutVBarrier_Move(params.coords.x, params.coords.y);
                                break;

                        default:
                                assert(!"Invalid ++MoveGenerator!");
                        }

                return curr_move;
        }

        inline operator bool() {
                return (type != MH_END_OF_LIST);
        }
};

static unsigned current_depth;
static unsigned maximum_depth;
static unsigned long maximum_time;
static Move *bestmove_store;
static unsigned long start_time;

float min_value(GameState &gs, float alpha, float beta);
float max_value(GameState &gs, float alpha, float beta);

float min_value(GameState &gs, float alpha, float beta)
{
        if (current_depth >= maximum_depth ||
            (get_timestamp() - start_time) >= maximum_time ||
            gs.game_result() != GameState::NOT_OVER) {
                return static_evaluator(gs);
        }

        current_depth++;
        Move *bestmove = 0;

        #ifdef TRACE
        std::cout << "---------- min_value(" << current_depth << ") ----------\n";
        std::cout << gs;
        #endif
        for (MoveGenerator moves(gs); moves; ++moves) {
                Move *currmove = moves;
                assert(currmove->is_valid_for(gs));
                //std::cout << "The move '" << *currmove << "' is valid [" << currmove->is_valid_for(gs) << "] for\n" << gs << "--------------\n";
                //if (currmove->is_valid_for(gs)) {
                        #ifdef TRACE
                        std::cout << "min_value(" << current_depth << ") TRYING '" << *currmove << "', current state:\n" << gs;
                        #endif
                        currmove->apply_to(gs);
                        #ifdef TRACE
                        std::cout << "State after apply_to:\n" << gs;
                        #endif
                        float value = max_value(gs, alpha, beta);
                        #ifdef TRACE
                        std::cout << "min_value(" << current_depth << ") UNAPPLYING '" << *currmove << "' (value = " << value << "), current state:\n" << gs;
                        #endif
                        currmove->unapply_to(gs);
                        #ifdef TRACE
                        std::cout << "State after unapply_to:\n" << gs;
                        #endif
                        if (value < beta) {
                                #ifdef TRACE
                                std::cout << '\'' << *currmove << "' (" << value << ") is better than '" << bestmove << "' (beta=" << beta << ")\n";
                                #endif
                                delete bestmove;
                                bestmove = currmove;
                                beta = value;
                        } else delete currmove;
                        if (alpha >= beta) {
                                #ifdef TRACE
                                std::cout << "min_value(" << current_depth << ") -- PRUNED\n";
                                #endif
                                goto prune;
                        }
                /*
                } else {
                        std::cout << "INVALID MOVE GENERATED IN min_value(" << current_depth << ") : " << *currmove << '\n';
                        std::cout << gs;
                        delete currmove;
                        assert(0);
                }
                */
        }

prune:
        current_depth--;
        delete bestmove_store;
        bestmove_store = bestmove;
        return beta;
}

float max_value(GameState &gs, float alpha, float beta)    // also sets bestmove_store to the bestmove
{
        if (current_depth >= maximum_depth ||
            (get_timestamp() - start_time) >= maximum_time ||
            gs.game_result() != GameState::NOT_OVER) {
                return static_evaluator(gs);
        }

        current_depth++;
        Move *bestmove = 0;

        for (MoveGenerator moves(gs); moves; ++moves) {
                Move *currmove = moves;
                assert(currmove->is_valid_for(gs));
                //if (currmove->is_valid_for(gs)) {
                        #ifdef TRACE
                        std::cout << "max_value(" << current_depth << ") TRYING '" << *currmove << "', current state:\n" << gs;
                        #endif
                        currmove->apply_to(gs);
                        #ifdef TRACE
                        std::cout << "State after apply_to:\n" << gs;
                        #endif
                        float value = min_value(gs, alpha, beta);
                        #ifdef TRACE
                        std::cout << "max_value(" << current_depth << ") UNAPPLYING '" << *currmove << "' (value = " << value << "), current state:\n" << gs;
                        #endif
                        currmove->unapply_to(gs);
                        #ifdef TRACE
                        std::cout << "State after unapply_to:\n" << gs;
                        #endif
                        if (value > alpha) {
                                #ifdef TRACE
                                std::cout << '\'' << *currmove << "' (" << value << ") is better than '" << bestmove << "' (alpha=" << alpha << ")\n";
                                #endif
                                alpha = value;
                                delete bestmove;
                                bestmove = currmove;
                        } else delete currmove;
                        if (alpha >= beta) {
                                #ifdef TRACE
                                std::cout << "max_value(" << current_depth << ") -- PRUNED\n";
                                #endif
                                goto prune;
                        }
                /*
                } else {
                        std::cout << "INVALID MOVE GENERATED! " << *currmove << '\n';
                        std::cout << gs;
                        delete currmove;
                        assert(0);
                }
                */
        }

prune:
        current_depth--;
        delete bestmove_store;
        bestmove_store = bestmove;
        return alpha;
}

Move* Player_AI::choose_move(const GameState &gs)
{
        std::cerr << "Static value before move = " << static_evaluator(gs);

        bestmove_store = 0;
        current_depth = 0;
        maximum_depth = this->search_depth;
        maximum_time = this->search_time;
        start_time = get_timestamp();
        float value;
        if (gs.white_playing) value = max_value(GameState(gs), -INF, INF);
        else value = min_value(GameState(gs), -INF, INF);
        if (!bestmove_store) bestmove_store = new Pass_Move();
        assert(bestmove_store);
        std::cerr << ", after move = " << value << std::endl;

        return bestmove_store;
}

float AI_analyse(const GameState &gs)
{
        bestmove_store = 0;
        current_depth = 0;
        maximum_depth = 2;
        maximum_time = 20000;
        start_time = get_timestamp();
        if (gs.white_playing) return max_value(GameState(gs), -INF, INF);
        else return min_value(GameState(gs), -INF, INF);
}

