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

#include <vcl.h>
#pragma hdrstop

#include "NewGameDlg.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "CSPIN"
#pragma resource "*.dfm"
TfrmNewGame *frmNewGame;
//---------------------------------------------------------------------------
__fastcall TfrmNewGame::TfrmNewGame(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TfrmNewGame::FormCreate(TObject *Sender)
{
        btnHuman2->Glyph = btnHuman1->Glyph;
        btnCPU2->Glyph = btnCPU1->Glyph;

        btnPlayerTypeClick(0);
}
//---------------------------------------------------------------------------

void __fastcall TfrmNewGame::btnPlayerTypeClick(TObject *Sender)
{
        enum { NAMES_COUNT = 4 };
        static const char* cpu_names[NAMES_COUNT] = { "Robbie", "Cybie", "Arnie", "Bobbie" };

        bool iscpu;
        iscpu = btnCPU1->Down;
        editCPULevel1->Enabled = iscpu;
        if (iscpu) {
          editName1->Enabled = false;
          do {
            editName1->Text = cpu_names[random(NAMES_COUNT)];
          } while (editName1->Text == editName2->Text);
        } else {
          editName1->Enabled = true;
          editName1->Text = "Player 1";
        }

        iscpu = btnCPU2->Down;
        editCPULevel2->Enabled = iscpu;
        if (iscpu) {
          editName2->Enabled = false;
          do {
            editName2->Text = cpu_names[random(NAMES_COUNT)];
          } while (editName1->Text == editName2->Text);
        } else {
          editName2->Enabled = true;
          editName2->Text = "Player 2";
        }
}
//---------------------------------------------------------------------------

void __fastcall TfrmNewGame::btnStartClick(TObject *Sender)
{
        human1 = btnHuman1->Down;
        human2 = btnHuman2->Down;
        cpu_depth1 = editCPULevel1->Value;
        cpu_depth2 = editCPULevel2->Value;
        ModalResult = mrOk;
}
//---------------------------------------------------------------------------

