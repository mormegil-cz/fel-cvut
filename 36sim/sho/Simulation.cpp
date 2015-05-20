// Simulation.cpp
//
//////////////////////////////////////////////////////////////////////
#include <windows.h>
#include "Simulation.h"
#include "Simset.h"
#include <string>
#include "Process.h"

//////////////////////////////////////////////////////////////////////
// EventNotice.cpp: implementation of the CEventNotice class.
//
//////////////////////////////////////////////////////////////////////

CEventNotice::CEventNotice(TIME time, CProcess* p)
{
    evTime = time;
    proc = p;
    SetName();       // nastavi name objektu
}

CEventNotice::~CEventNotice()
{
    this->Out();    // pred svym zrusenim se vyradi ze seznamu
}

// zaradi pozadavek na casovou osu seznamu SQS
// before = true ... pred vsechny pozadavky se stejnym evTime
// before = false ... pred pozadavky s vestim evTime
void CEventNotice::Rank(bool before)
{
    CEventNotice* p;

    p = (CEventNotice*) CProcess::SQS->First();

    while ((p != NULL) && (p->evTime < evTime))
        p = (CEventNotice*) p->Suc();
    
    if (!before)
        while ((p != NULL) && (p->evTime == evTime))
            p = (CEventNotice*) p->Suc();
    
    SetName();
    if (p != NULL) Precede(p);
    else Into(CProcess::SQS);
}

// vytvori pro polozku jmeno ve tvaru Event at ABC on Device XYZ
void CEventNotice::SetName()
{
    string text;               // pro sestaveni textu pro list
    char t[50];
    text = "Event at ";
    gcvt( evTime, 10, t);
    text +=t;
    text +=" on ";
    text +=proc->GetName();    // prida name planovaneho procesu
    CLinkage::SetName(text);   // nastavi name v Clinkage dle text 
}

//////////////////////////////////////////////////////////////////////
// Process.cpp: implementation of the CProcess class.
//
//////////////////////////////////////////////////////////////////////

CHead* CProcess::SQS = NULL;    // seznam je vytvoren ve tride CSimulation

CProcess::CProcess(bool createThread)
{
    terminated = false;
    event = NULL;                // zatim nema zaznam o udalosti
    char x[20];
    itoa((long int) this, x, 16);
    SetName(string("Process ")+ x);   // nastavi name v CLinkage
    if (createThread) InitThread();      // vola CreateThread, ktera vytvori vlakno 
                                      // ve stavu SUSPENDED
}

CProcess::~CProcess()
{
    if (event != NULL) delete (event);
}

// vraci true, je-li proces ukoncen
bool CProcess::Terminated()
{
    return terminated;
}

// vraci true, je-li proces pasivni
bool CProcess::Idle()
{
    return (event == NULL);
}

// vrati cas udalosti spojene s procesem, pokud je proces IDLE
// vraci konstantu EVENT_IDLE (-1)
TIME CProcess::EvTime()
{
    if (Idle()) return EVENT_IDLE;          // vraci -1
        else    return event->evTime;
}

//telo provadeciho vlakna procesu
DWORD CProcess::Start()
{
    Run();                // popis aktivnich fazi procesu
                        // po jejich vykonani:
    CProcess* next;
    if (event != NULL)        // pokud ma proces zaznam o udalosti
    {
        next=NextEv();
        event->Out();
        delete(event);        // odstrani jej
        event = NULL;

        if (next!=NULL)         // v SQS je zaznam o dalsim procesu
                                         // tady nekde prerusit pri krokovani
            next->Resume();        // aktivuj tento proces
    }
    terminated = true;            // oznac proces za ukonceny
    return 0;                        // a ukonci proceduru vlakna
}

// vrati proces jehoz aktivni faze bude zahajena po potlaceni tohoto procesu
CProcess* CProcess::NextEv()
{
    if (Idle()) return NULL;    // proces nema zaznam o udalosti
    if (event->Suc() == NULL) 
        return NULL;            // udalost procesu nema naslednika
    
    return ((CEventNotice*)event->Suc())->proc;
}

// preplanuje prave aktivni proces na cas Time + T
void CProcess::Hold(TIME T)
{
    if (this != Current()) return;  // pracuje jen s prave aktivnim procesem
    if (T <= 0) return;            // pokud je T <= 0, nedelej nic
    
    CEventNotice* e = FirstEv();    // prave zpracovavanou udalost
    e->evTime += T;                    // naplanuj na dobu Time()+T

    if (e->Suc() != NULL)            // pokud to neni posledni pl. udalost
    {
        if (((CEventNotice*) e->Suc())->evTime <= e->evTime)    
        {                            // pokud nasledujici udalost nastane driv
            e->Out();                // vyrad soucasnou prvni udalost
            e->Rank(false);            // zarad ji na spravne misto podle casu
        }
    }
    else
        e->SetName();       // nastavi name v CLinkage
    ResumeCurrent();        // predej rizeni prvnimu procesu
}

// prave aktivni proces zpasivuje a zaznam o jeho udalosti vyradi z SQS
// rizeni preda dalsimu procesu podle SQS
void CProcess::Passivate()
{
    if (this != Current()) return;
    CProcess* cur = Current();
    
    cur->event->Out();                // zrus zaznam o tomto procesu z SQS
    delete (cur->event);
    cur->event=NULL;
    
    if (SQS->Empty()); // cout << "CHYBA PASSIVATE\n\n";
                                    // pokud je fronta prazdna -> chyba
    else ResumeCurrent();            // prepnuti na novy prvni proces
}

// aktualni proces zaradi do fronty S a zpasivuje ho
void CProcess::Wait(CHead * S)
{
    if (this != Current()) return;
    Current()->Into(S);                // pokud S neni NULL, je na konec
                                    // seznamu S zarazen proces Current()
    Passivate();                    // potlaceni aktivniho procesu
}

// potlaceni procesu p
void CProcess::Cancel(CProcess * p)
{
    if (p == Current()) Passivate();  // potlaceni prave aktivniho procesu
    else                              // provadi uz Passivate
    {
        if (p->event != NULL)          // pokud ma p zaznam v SQS
        {
            p->event->Out();          // zrus ho
            delete (p->event);
            p->event=NULL;
        }
    }
}

// metoda pro aktivaci procesu volana pomoci maker
// ActivateXXXXX a ReactivateXXXXX
// reac == true znamena, ze jde o volani Reactivate
// code ucuje typ aktivace
// t je cas aktivace
// r udava vztazny proces
// prior - aktivace pred/po procesu r
void CProcess::_activate(bool reac, CODE code, 
                           TIME t, CProcess * r, bool prior)
{
    if (this->Terminated()) return; 
                // pokud je proces ukonceny, nelze jej jiz aktivovat
    if ((this->event != NULL) & !reac) return;
                    // nelze take aktivovat jiz naplanovany proces
                            // nebo pokud nema platny zaznam v SQS

    CEventNotice* oldEv = this->event;    
                        // odkazy na this->event pro reactivate
    CProcess* oldProc = Current();

    switch (code) 
            {
            case (DIRECT):                // Activate()
                this->event = new CEventNotice(Time(), this); 
                this->event->Rank(true);
                                // vytvor zaznam pro proces
                                // a zarad ho na zacatek SQS
                break;
                
            case (DELAY): t+= Time();    // ActivateDelay(T)
                                        // = activate at T+Time()
            case (AT):                    // ActivateAt(T)
                if (t < Time()) t = Time();  // korekce zadaneho casu
                this->event = new CEventNotice(t, this); 
                this->event->Rank(prior);
                                        // vytvoreni a zarazeni udalosti
                break;
            
            case (AFTER):        // ActivateAfter
            case (BEFORE):        // ActivateBefore
                // pokud neexistuje vztazny proces, prip. ten nema
                // zaznam v SQS, pak ani tento novy proces nebude aktivovan
                if ((r == NULL) || (r->event == NULL))
                    this->event = NULL;
                else
                {
                    //vytvor polozku eventnotice s casem odpovidajici
                    //casu vztazneho procesu
                    this->event = new CEventNotice(r->event->evTime, this); 
                    
                    //zarad ji podle pozadavku <AFTER> <BEFORE>
                    if (code == BEFORE) this->event->Precede(r->event);
                        else this->event->Follow(r->event);
                }
                break;
            default: break;
            }
    
    if (reac)        // zruseni stareho zaznamu pro reaktivovany porces
        if (oldEv != NULL)
        {oldEv->Out();
         delete(oldEv);
         oldEv=NULL;
//         if (SQS->Empty()) cout << "CHYBA, prazdna fronta SQS";
                            // CHYBA! prazdna fronta SQS, nemelo
                            // by nikdy nastat
        }    
    if (oldProc != NULL) oldProc->ResumeCurrent();
        else ResumeCurrent();    
        // predani rizeni prvnimu naplanovanemu procesu
}

// vraci prave aktivni proces
CProcess* CProcess::Current()
{
    if (FirstEv() == NULL) return NULL;
    else return FirstEv()->proc;
}

//vraci prvni polozku v seznamu SQS
CEventNotice* CProcess::FirstEv()
{
    return (CEventNotice*) SQS->First();
}

//okamzita hodnota modeloveho casu
TIME CProcess::Time()
{
    if (FirstEv()==NULL) return 0;
    else return FirstEv()->evTime;
}

// predani rizeni procesu Current()
void CProcess::ResumeCurrent()
{
    if (Current() != this)
    {    
    Current()->Resume();            // obnov prvni proces 
    this->Suspend();            // sebe pozastav
    }
}


//////////////////////////////////////////////////////////////////////
// Simulation.cpp: implementation of the CSimulation class.
//
//////////////////////////////////////////////////////////////////////

CSimulation::CSimulation(BOOL run)
{
    SetName("Simulation");  // nastavi name v CLinkage
    SQS = new CHead();    
    SQS->SetName("SQS");    // vytvoteni seznamu SQS
    
    event = new CEventNotice(0, this);        //vytvoreni EventNotice
                                            //pro proces simulation
    event->Into(SQS);            // zarazeni teto udalosti do SQS
    if (run) this->Resume();    // aktivace CSimulation::Run()
                                // pokud run == true
                                // totez co Activate(this)
}

CSimulation::~CSimulation()
{
    delete(SQS);
    SQS = NULL;
}

//////////////////////////////////////////////////////////////////////
// Thread.cpp: implementation of the CThread class.
//
//////////////////////////////////////////////////////////////////////

CThread::CThread()
{
    hThread = NULL;
    threadID = 0;
}

CThread::~CThread()
{
    if (hThread != NULL)
        TerminateThread(hThread, threadID);
        // zruseni vlakna v pripade, ze neni ukonceno pred volanim destruktoru
};


// obnovi beh vlakna
DWORD CThread::Resume()
{
    if (hThread == NULL)        // vlakno dosud nebylo vytvoreno
    {                            // zadna akce    
        return 0;
    }
    DWORD res = ResumeThread(hThread);
    while (res!=1) {
        ::Sleep(10);
        res = ResumeThread(hThread);
    }
    return res;
//    return ResumeThread(hThread); // vlastni spusteni vlakna (WIN API)
};

// pozastavi beh vlakna
DWORD CThread::Suspend()
{
    if (hThread == NULL)         // vlakno dosud nebylo vytvoreno
    {                            // zadna akce
        return 0;
    }
    return SuspendThread(hThread); // pozastaveni behu vlakna (WIN API)
};


//Init vytvori pro objekt CThread jeho provadeci tok - vlakno
//tim je staticka metoda ThreadProc, ktere se preda ukazatel this
void CThread::InitThread()
{
    hThread = CreateThread( NULL, 0, ThreadProc, 
                    (LPVOID) this, CREATE_SUSPENDED, &threadID);
};

//hlavni procedura vlakna
DWORD WINAPI CThread::ThreadProc(void * x)
{
    CThread* myThread = (CThread*) x;
    // pomoci myThread lze jiz pristupovat i k nestatickym clenum tridy
    
    myThread->Start();                // dynamicky volana metoda Start() 
                                    // po jejim vykonani je
                                    // prace vlakna ukoncena 
    return (myThread->threadID);    // opustenim ThreadProc zanika
                                    // pracovni vlakno objektu CThread
};




