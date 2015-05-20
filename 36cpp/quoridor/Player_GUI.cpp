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

//---------------------------------------------------------------------------

#include <strstream>
#include <vcl.h>
#pragma hdrstop

#include "GameCore.h"
#include "Player_GUI.h"
#include "AI.h"
#include "myassert.h"
#include "NewGameDlg.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmGameMain *frmGameMain;
//---------------------------------------------------------------------------

class Player_GUI : public Player {
public:
        virtual ::Move* choose_move(const GameState &gs) {
                return frmGameMain->GetMove();
        }
};

Quoridor_Game *game;
AnsiString names[2];

static void initGrid(TDrawGrid* grid)
{
  grid->FixedRows = 0;
  grid->FixedCols = 0;
  grid->RowCount = 2 * BOARD_SIZE - 1;
  grid->ColCount = 2 * BOARD_SIZE - 1;
  for (int i = 0; i < BOARD_SIZE; i++) {
    grid->ColWidths[2*i] = 30;
    grid->RowHeights[2*i] = 30;
    if (i < BOARD_SIZE - 1) {
        grid->ColWidths[2*i + 1] = 10;
        grid->RowHeights[2*i + 1] = 10;
    }
  }
  grid->Width = BOARD_SIZE * 31 + (BOARD_SIZE - 1) * 11 + 3;
  grid->Height = BOARD_SIZE * 31 + (BOARD_SIZE - 1) * 11 + 3;
}

static void GameMove(const GameState &gs, const ::Move &move)
{
  std::strstream s;
  s << move;
  frmGameMain->GameGrid->Invalidate();
  frmGameMain->StatusBar->Panels->Items[0]->Text = names[int(!gs.white_playing)] + " " + s.str();
  Application->ProcessMessages();
  if (Application->Terminated) Abort();
}

static void GameUpdate(const GameState &gs)
{
  frmGameMain->GameGrid->Invalidate();
  frmGameMain->lblBarriers0->Caption = IntToStr(gs.barriers_remaining[0]);
  frmGameMain->lblBarriers1->Caption = IntToStr(gs.barriers_remaining[1]);
  Application->ProcessMessages();
  if (Application->Terminated) Abort();
}

::Move* __fastcall TfrmGameMain::GetMove()
{
  Screen->Cursor = crDefault;
  //frmGameMain->StatusBar->Panels[0]->Text = "Choose your move";
  selected_move = 0;
  while (!selected_move) {
    Application->HandleMessage();
    if (Application->Terminated) Abort();
    if (selected_move && !selected_move->is_valid_for(game->get_gamestate())) {
      delete selected_move;
      selected_move = 0;
      frmGameMain->StatusBar->Panels->Items[0]->Text = "That move is invalid, choose another";
      MessageBeep(0xffffffff);
    }
  }
  frmGameMain->StatusBar->Panels->Items[0]->Text = "";
  Application->ProcessMessages();
  Screen->Cursor = crHourGlass;
  return selected_move;
}

//---------------------------------------------------------------------------
__fastcall TfrmGameMain::TfrmGameMain(TComponent* Owner)
        : TForm(Owner)
{
  initGrid(GameGrid);
  ClientHeight = GameGrid->Height + StatusBar->Height;
  ClientWidth = GameGrid->Width + pnlGameInfo->Width;
  lblBarriers0->Top = GameGrid->Height - lblBarriers0->Height;

  Player *p[2];

  frmNewGame = new TfrmNewGame(this);
  if (frmNewGame->ShowModal() != mrOk) {
        Application->Terminate();
        Abort();
  }
  if (frmNewGame->human1) {
        p[0] = new Player_GUI();
  } else {
        Player_AI *pai = new Player_AI();
        pai->search_depth = frmNewGame->cpu_depth1;
        pai->search_time = 15000;
        p[0] = pai;
  }
  if (frmNewGame->human2) {
        p[1] = new Player_GUI();
  } else {
        Player_AI *pai = new Player_AI();
        pai->search_depth = frmNewGame->cpu_depth2;
        pai->search_time = 15000;
        p[1] = pai;
  }
  names[0] = frmNewGame->editName1->Text;
  names[1] = frmNewGame->editName2->Text;
  delete frmNewGame;
  frmNewGame = 0;

  game = new Quoridor_Game(p, p + 2);
  game->update_notifier = GameUpdate;
  game->move_notifier = GameMove;
}
//---------------------------------------------------------------------------

void __fastcall TfrmGameMain::GameGridDrawCell(TObject *Sender, int ACol,
      int ARow, TRect &Rect, TGridDrawState State)
{
  TCanvas* Canvas = GameGrid->Canvas;
  if (!game) {
        Canvas->Brush->Style = bsSolid;
        Canvas->Brush->Color = clGrayText;
        Canvas->Pen->Style = psSolid;
        Canvas->Pen->Color = clGrayText;
        Canvas->FillRect(Rect);
        return;
  }

  static const TColor colors[] = { clWhite, clRed, clRed, clBlue, clBlue };
  const GameState &gameState = game->get_gamestate();
  ARow = 2 * BOARD_SIZE - ARow - 2;

  bool colBarrier = ACol & 1;
  bool rowBarrier = ARow & 1;

  ACol >>= 1;
  ARow >>= 1;
  if (colBarrier && rowBarrier) {
    Canvas->Brush->Style = bsSolid;
    Canvas->Brush->Color = colors[gameState.points[ACol][ARow]];
    Canvas->Pen->Style = psSolid;
    Canvas->Pen->Color = Canvas->Brush->Color;
    Canvas->FillRect(Rect);
    return;
  }
  if (colBarrier) {
    Canvas->Brush->Style = bsSolid;
    Canvas->Brush->Color = (gameState.board[ACol][ARow] & RIGHT) ? clWhite : clBlack;
    Canvas->Pen->Style = psSolid;
    Canvas->Pen->Color = Canvas->Brush->Color;
    Canvas->FillRect(Rect);
    return;
  }
  if (rowBarrier) {
    Canvas->Brush->Style = bsSolid;
    Canvas->Brush->Color = (gameState.board[ACol][ARow] & UP) ? clWhite : clBlack;
    Canvas->Pen->Style = psSolid;
    Canvas->Pen->Color = Canvas->Brush->Color;
    Canvas->FillRect(Rect);
    return;
  }

  Canvas->Brush->Style = bsSolid;
  Canvas->Brush->Color = clWhite;
  Canvas->Pen->Style = psSolid;
  Canvas->Pen->Color = Canvas->Brush->Color;
  Canvas->FillRect(Rect);

  if (ACol == gameState.player_x[0] && ARow == gameState.player_y[0]) {
    Canvas->Brush->Color = clRed;
    Canvas->Ellipse(Rect);
  } else if (ACol == gameState.player_x[1] && ARow == gameState.player_y[1]) {
    Canvas->Brush->Color = clBlue;
    Canvas->Ellipse(Rect);
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmGameMain::FormActivate(TObject *Sender)
{
   OnActivate = 0;
   Application->ProcessMessages();
   try {
     Screen->Cursor = crHourGlass;
     if (game->go()) {
       MessageDlg("Congratulations, " + names[0] + "!", mtInformation, TMsgDlgButtons() << mbOK, 0);
     } else {
       MessageDlg("Congratulations, " + names[1] + "!", mtInformation, TMsgDlgButtons() << mbOK, 0);
     }
     Application->Terminate();
   }
   __finally {
     Screen->Cursor = crDefault;
   }
}
//---------------------------------------------------------------------------

static Direction dir_lut[3][3] = {
    { Direction(-1), UP,            Direction(-1) },
    { LEFT,          Direction(-1), RIGHT         },
    { Direction(-1), DOWN,          Direction(-1) }
};

inline Direction deltas_to_dir(int dx, int dy)
{
  assert(dx >= -1 && dx <= +1 && dy >= -1 && dy <= +1);
  return dir_lut[1 - dy][dx + 1];
}

void __fastcall TfrmGameMain::GameGridClick(TObject *Sender)
{
  TPoint mouse = GameGrid->ScreenToClient(Mouse->CursorPos);
  int ACol, ARow;
  GameGrid->MouseToCell(mouse.x, mouse.y, ACol, ARow);
  if (ACol < 0 || ARow < 0) {
    MessageBeep(0xffffffff);
    return;
  }
  ARow = 2 * BOARD_SIZE - ARow - 2;

  bool colBarrier = ACol & 1;
  bool rowBarrier = ARow & 1;

  ACol >>= 1;
  ARow >>= 1;

  if (colBarrier && rowBarrier) {
    MessageBeep(0xffffffff);
    return;
  }

  if (rowBarrier) {
    int x = ACol;
    int y = ARow + 1;
    delete selected_move;
    selected_move = new PutHBarrier_Move(x, y);
    return;
  }
  if (colBarrier) {
    int x = ACol + 1;
    int y = ARow - 1;
    delete selected_move;
    selected_move = new PutVBarrier_Move(x, y);
    return;
  }

  const GameState& gameState = game->get_gamestate();
  int my_x, my_y, his_x, his_y;
  my_x = gameState.player_x[int(!gameState.white_playing)];
  his_x = gameState.player_x[int(gameState.white_playing)];
  my_y = gameState.player_y[int(!gameState.white_playing)];
  his_y = gameState.player_y[int(gameState.white_playing)];
  if (ACol == my_x && ARow == my_y) {
    if (MessageDlg("Do you want to pass your move?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo, 0) == ID_YES) {
      delete selected_move;
      selected_move = new Pass_Move();
      return;
    }
  }
  if (abs(ACol - my_x) + abs(ARow - my_y) == 1) {
    Direction dir = deltas_to_dir(ACol - my_x, ARow - my_y);
    delete selected_move;
    selected_move = new MoveFigure_Move(dir);
    return;
  }
  if (abs(ACol - my_x) + abs(ARow - my_y) == 2 &&
      abs(his_x - my_x) + abs(his_y - my_y) == 1 &&
      abs(ACol - his_x) + abs(ARow - his_y) == 1
      ) {
    Direction dir1 = deltas_to_dir(his_x - my_x, his_y - my_y);
    Direction dir2 = deltas_to_dir(ACol - his_x, ARow - his_y);
    delete selected_move;
    selected_move = new JumpFigure_Move(dir1, dir2);
    return;
  }

  MessageBeep(0xffffffff);
}
//---------------------------------------------------------------------------

void __fastcall TfrmGameMain::FormKeyDown(TObject *Sender, WORD &Key,
      TShiftState Shift)
{
  if (Key == VK_F1) {
      MessageDlg("Quoridor v0.5\n© 2003 Petr Kadlec <mormegil@centrum.cz>\nThis is free software under the GNU GPL",
                 mtInformation, TMsgDlgButtons() << mbOK, 0);
      Key = 0;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmGameMain::GameGridMouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
  TPoint mouse = GameGrid->ScreenToClient(Mouse->CursorPos);
  int ACol, ARow;
  GameGrid->MouseToCell(mouse.x, mouse.y, ACol, ARow);
  if (ACol < 0 || ARow < 0) {
    GameGrid->Cursor = crNo;
    return;
  }
  ARow = 2 * BOARD_SIZE - ARow - 2;

  bool colBarrier = ACol & 1;
  bool rowBarrier = ARow & 1;

  if (!game) {
    if (colBarrier && rowBarrier) GameGrid->Cursor = crNo;
    else if (colBarrier || rowBarrier) GameGrid->Cursor = crDrag;
    else GameGrid->Cursor = crDefault;
    return;
  }

  ACol >>= 1;
  ARow >>= 1;

  if (colBarrier && rowBarrier) {
    GameGrid->Cursor = crNo;
    return;
  }

  if (rowBarrier) {
    int x = ACol;
    int y = ARow + 1;
    if (PutHBarrier_Move(x, y).is_valid_for(game->get_gamestate())) GameGrid->Cursor = crDrag;
    else GameGrid->Cursor = crNo;
    return;
  }
  if (colBarrier) {
    int x = ACol + 1;
    int y = ARow - 1;
    if (PutVBarrier_Move(x, y).is_valid_for(game->get_gamestate())) GameGrid->Cursor = crDrag;
    else GameGrid->Cursor = crNo;
    return;
  }

  const GameState& gameState = game->get_gamestate();
  int my_x, my_y, his_x, his_y;
  my_x = gameState.player_x[int(!gameState.white_playing)];
  his_x = gameState.player_x[int(gameState.white_playing)];
  my_y = gameState.player_y[int(!gameState.white_playing)];
  his_y = gameState.player_y[int(gameState.white_playing)];
  if (ACol == my_x && ARow == my_y) {
    GameGrid->Cursor = crDefault;
    return;
  }
  if (abs(ACol - my_x) + abs(ARow - my_y) == 1) {
    Direction dir = deltas_to_dir(ACol - my_x, ARow - my_y);
    if (MoveFigure_Move(dir).is_valid_for(game->get_gamestate())) GameGrid->Cursor = crUpArrow;
    else GameGrid->Cursor = crNo;
    return;
  }
  if (abs(ACol - my_x) + abs(ARow - my_y) == 2 &&
      abs(his_x - my_x) + abs(his_y - my_y) == 1 &&
      abs(ACol - his_x) + abs(ARow - his_y) == 1
      ) {
    Direction dir1 = deltas_to_dir(his_x - my_x, his_y - my_y);
    Direction dir2 = deltas_to_dir(ACol - his_x, ARow - his_y);
    if (JumpFigure_Move(dir1, dir2).is_valid_for(game->get_gamestate())) GameGrid->Cursor = crUpArrow;
    else GameGrid->Cursor = crNo;
    return;
  }

  GameGrid->Cursor = crNo;
}
//---------------------------------------------------------------------------

