#include <stdio.h>
#include "simulation.h"

#define DELKA_FRONTY 10
#define CAS_KONZUMACE 1
#define CAS_VAJICEK  20
#define POCET_VAJICEK 10

enum { LEVA_RUKA, PRAVA_RUKA, LEVA_NOHA, PRAVA_NOHA, TELO_MAX };

class CKomar : public CProcess {
  int m_Ident;
 public:
  CKomar(int id) : m_Ident(id) {};
  virtual void Run();

  int Ident() { return m_Ident; };
};

class CKusTela : public CProcess {
 private:
  CHead *m_Fronta;
 public:
  CKusTela(CHead *f) { m_Fronta = f; };
  virtual void Run();
};

// -------------------------------

CHead       *g_Fronta[TELO_MAX];
CKusTela    *g_Profesor[TELO_MAX];
int g_KomarId = 1;

void ukaz_fronty()
{
  printf("\tFronty: [");
  for (int i = 0; i < TELO_MAX; i++)
    printf("%2d ", g_Fronta[i] -> Cardinal());
  printf("]\n");
  fflush(stdout);
}

void CKomar::Run()
{
  printf("%5.1f: Komar %d se narodil", Time(), m_Ident); ukaz_fronty();

  int minLen = g_Fronta[0] -> Cardinal();
  int minIdx = 0;

  for (int i = 1; i < TELO_MAX; i++) {
    int len = g_Fronta[i] -> Cardinal();
    if (len < minLen) {
       minLen = len;
       minIdx = i;
    }
  }

  if (minLen >= DELKA_FRONTY) {
    printf("\tnema kam jit a umira hlady\n");
    fflush(stdout);
    return;
  }

  printf("\tzaradil se do fronty %d\n", minIdx);

  CHead *fronta = g_Fronta[minIdx];
  //bool isEmpty = fronta -> Empty();

  //Into(fronta);
  //if (isEmpty) {
  if (fronta->Empty()) {
     printf("\tktera je prazdna, takze zacal pit\n");
     Into(fronta);
     g_Profesor[minIdx] -> Activate();
  } else {
    Into(fronta);
    printf("\tktera neni prazdna, takze musi cekat\n");
  }

  Passivate();
  printf("%5.1f: Komar %d odleta zalozit rodinu (%d - %d)", Time(), m_Ident, g_KomarId, g_KomarId + POCET_VAJICEK - 1); ukaz_fronty();

  for (int i = 0; i < POCET_VAJICEK; i++) {
    (new CKomar(g_KomarId++)) -> ActivateDelay(CAS_VAJICEK);
  }
}

void CKusTela::Run()
{
  while(1) {
    if (m_Fronta->Empty()) {
      printf("%5.1f: Kus tela ceka, az nekdo prileti.", Time()); ukaz_fronty();
      Passivate();
      printf("%5.1f: ...na kus tela konecne nekdo priletel...", Time()); ukaz_fronty();
    }

    CKomar *K = (CKomar *) m_Fronta->First();
    printf("%5.1f: Komar %d byl nalezen ve fronte", Time(), K->Ident()); ukaz_fronty();
    K->Out();
    printf("\ta vybran z ni"); ukaz_fronty();

    printf("%5.1f: Komar %d se zacal krmit", Time(), K->Ident()); ukaz_fronty();
    Hold(CAS_KONZUMACE);
    printf("%5.1f: Komar %d skoncil krmeni", Time(), K->Ident()); ukaz_fronty();
    K->Activate();
  }
}

// -------------------------------

void CSimulation::Run()                          // hlavni proces
{
  printf("### Simulace utoku komaru ###\n\n%5.1f: Start simulace\n", Time());

  for (int i = 0; i < TELO_MAX; i++) {
    g_Fronta[i] = new CHead();
    g_Profesor[i] = new CKusTela(g_Fronta[i]);
  }

  (new CKomar(g_KomarId++)) -> Activate();

  Hold(1000); // celkova doba simulace

  printf("%5.1f: Konec simulace", Time()); ukaz_fronty();
}

