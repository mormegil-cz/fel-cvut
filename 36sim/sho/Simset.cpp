// Linkage.cpp: implementation of the CLinkage class.
//
//////////////////////////////////////////////////////////////////////

#include "Simset.h"

CLinkage::CLinkage()
{
	char x[20];
	SetName(itoa((unsigned long)this, x, 16));	
		// jmeno obsahuje pouze adresu objektu
}

CLinkage::~CLinkage()
{}

//metody pro pojmenovani prvku seznamu
void CLinkage::SetName(string newName)
{
	name = newName;
}

string CLinkage::GetName()
{
	return name;
}

//////////////////////////////////////////////////////////////////////
// Head.cpp: implementation of the CHead class.
//
//////////////////////////////////////////////////////////////////////

CHead::CHead()
{
	pred = this;
	suc = this;
	list = "";
	enableInfo = true;
	SetName("Head "+GetName());	   // nastavi name
}

CHead::~CHead()
{
	//Win_UnregisterHead();
}

//vrati polozku na niz ukazuje pred
CLink* CHead::Pred()
{
	if (this == NULL) return NULL;	// neplatna hlava seznamu
	if (pred == this) return NULL;	// seznam je prazdny
		else return (CLink*) pred;
}

//vrati polozku na niz ukazuje suc
CLink* CHead::Suc()
{
	if (this == NULL) return NULL;  // neplatna hlava
	if (suc == this) return NULL;   // seznam je prazdny
		else return (CLink*) suc;
}

// vrati prvni prvek seznamu prip. NULL, je-li seznam prazdny
CLink* CHead::First()
{
	if (this == NULL) return NULL;  // neplatna hlava
	return Suc();                   // NULL, je-li prazdny seznam
}

// vrati posledni prvek seznamu prip. NULL, je-li seznam prazdny
CLink* CHead::Last()
{
	if (this == NULL) return NULL;   //neplatna hlava
	return Pred();                   // NULL, je-li seznam prazdny
}

// vraci true pokud je seznam prazdny
bool CHead::Empty()
{
	if (this == NULL) return true;   // neplatna hlava
	return (this == suc);            //je-li suc == this, pak true		
}

// vraci pocet prvku seznamu, na nejz hlava odkazuje
int CHead::Cardinal()
{
	if (this == NULL) return 0;      // neplatna hlava
	int count = 0;
	CLinkage* x = this;              // nastavi x na hlavu
	while(x->Suc() != NULL)  // projdi vsechny polozky a spocitej je
	{
		count++;
		x = x->Suc();
	}
	return count;
}

// odstrani vsechny prvky ze seznamu
void CHead::Clear()
{
	CLink* x;
	while ((x = First()) != NULL) x->Out();
				// na kazdou polozku seznamu zavola Out()
}

// obnovi informace v retezci list ( pro ladici ucely)
// po zmene v seznamu
string* CHead::RefreshList()
{
	if (!enableInfo) return &list;
	list = "";
	CLinkage* x = First();                  // nastavi x na 1.objekt seznamu
	while (x != NULL)
	{
		if (list != "") list +=" | ";
		list += x->GetName();               // pripoji jmeno objektu
		x=x->Suc();                         // pokracuj na dalsi
	}
	return &list;
}

// povoluje/zakazuje generovani pomocne promenne list
void CHead::EnableInfo(bool enable)
{
	enableInfo = enable;
}




//////////////////////////////////////////////////////////////////////
// Link.cpp: implementation of the CLink class.
//
//////////////////////////////////////////////////////////////////////

CLink::CLink()
{
	pred = NULL;
	suc = NULL;
	head = NULL;
	SetName("Link "+GetName());          // nastavi name: Link + adresa
}

CLink::~CLink()
{
}

// vraci predchozi polozku v seznamu, NULL pokud je tato polozka prvni
CLink* CLink::Pred()
{
	if (pred == NULL) return NULL;        //pridano
	if (pred->IsHead()) return NULL;     // jde o jediny prvek seznamu 
	else return (CLink*) pred;
}

// vraci nasledujici polozku v seznamu, NULL pokud je tato polozka posledni
CLink* CLink::Suc()
{
	if (suc->IsHead()) return NULL;      // jde o jediny prvek seznamu
	return (CLink*) suc;
}

// odstraneni polozky ze seznamu
void CLink::Out()
{
	if (suc != NULL)			// pokud je polozka v nejakem seznamu
	{
		suc->pred = pred;		// znovuzretezeni seznamu po 
		pred->suc = suc;		// odtraneni polozky

		head->RefreshList();    // obnovi list
		
		suc = NULL;				// polozka se stava samostatnou
		pred = NULL;
		head = NULL;
	}
}

// zarazeni polozky na konec seznamu
void CLink::Into(CHead* h)
{
	if (h != NULL)				// pokud existuje hlava seznamu
	{
		Out();					// vyrad polozku z prip.
								// minuleho seznamu
		suc = h;				// a zarad ji na konec noveho seznamu
		pred = h->pred;
		pred->suc = this;
		h->pred = this;
		head = h;

		head->RefreshList();    //obnovi list
	}
}

// zaradi polozku do seznamu za polozku l
void CLink::Follow(CLinkage * l)
{
	if (l == NULL) return;		// polozka l neexistuje
	if (l->suc == NULL) return;	// polozka l neni v zadnem seznamu

	Out();						
	pred = l;					// vlastni zarazeni do seznamu
	suc = l->suc;
	l->suc = this;
	suc->pred = this;
    // priradi polozce ukazatel na hlavu seznamu
	if (!l->IsHead()) head = ((CLink*) l)->head;   // neni-li l hlava									
		else head = (CHead*) l;                    // nyni jde o hlavu		
	
	head->RefreshList();        // obnovi list
}

// zaradi polozku do seznamu pred polozku l
void CLink::Precede(CLinkage * l)
{
	if (l == NULL) return;		// polozka l neexistuje
	if (l->suc == NULL) return;	// polozka neni v zadnem seznamu

	Out();
	suc = l;
	pred = l->pred;
	pred->suc = this;
	l->pred = this;
	
	if (!l->IsHead()) head = ((CLink*) l)->head; //prirazeni hlavy seznamu
		else head = (CHead*) l;

	head->RefreshList();
}
