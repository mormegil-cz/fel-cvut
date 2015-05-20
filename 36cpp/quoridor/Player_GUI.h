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

#ifndef Player_GUIH
#define Player_GUIH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Grids.hpp>
#include <ExtCtrls.hpp>
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmGameMain : public TForm
{
__published:	// IDE-managed Components
        TDrawGrid *GameGrid;
        TPanel *pnlGameInfo;
        TLabel *lblBarriers1;
        TLabel *lblBarriers0;
        TStatusBar *StatusBar;
        void __fastcall GameGridDrawCell(TObject *Sender, int ACol,
          int ARow, TRect &Rect, TGridDrawState State);
        void __fastcall FormActivate(TObject *Sender);
        void __fastcall GameGridClick(TObject *Sender);
        void __fastcall FormKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
        void __fastcall GameGridMouseMove(TObject *Sender,
          TShiftState Shift, int X, int Y);
private:	// User declarations
        ::Move *selected_move;
public:		// User declarations
        __fastcall TfrmGameMain(TComponent* Owner);
        ::Move* __fastcall GetMove();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmGameMain *frmGameMain;
//---------------------------------------------------------------------------
extern Quoridor_Game *game;

#endif

