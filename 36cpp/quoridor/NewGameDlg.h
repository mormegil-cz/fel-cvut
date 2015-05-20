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

#ifndef NewGameDlgH
#define NewGameDlgH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include "CSPIN.h"
//---------------------------------------------------------------------------
class TfrmNewGame : public TForm
{
__published:	// IDE-managed Components
        TLabel *lblPlayer1;
        TEdit *editName1;
        TLabel *lblPlayer2;
        TEdit *editName2;
        TButton *btnStart;
        TSpeedButton *btnHuman1;
        TSpeedButton *btnCPU1;
        TSpeedButton *btnHuman2;
        TSpeedButton *btnCPU2;
        TCSpinEdit *editCPULevel1;
        TCSpinEdit *editCPULevel2;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall btnPlayerTypeClick(TObject *Sender);
        void __fastcall btnStartClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        bool human1, human2;
        int cpu_depth1, cpu_depth2;
        __fastcall TfrmNewGame(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmNewGame *frmNewGame;
//---------------------------------------------------------------------------
#endif

