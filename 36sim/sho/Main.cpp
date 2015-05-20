// main.cpp

#include "Simulation.h"
#include <windows.h>

void main()
{
     CSimulation sim;
  ::WaitForSingleObject(sim.hThread, INFINITE);
}
