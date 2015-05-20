#include <iostream>
#include <algorithm>

#ifdef WIN32
#include <windows.h>
#endif

#include "water.h"

using namespace std;

typedef JugsConfigHistory::reverse_iterator HistoryIterator;

int main(int argc, char *argv[])
{
    int jugcount;
    JugsConfig initial, capacity, goal;

    cout << "Number of jugs: ";
    cin >> jugcount;

    initial.reserve(jugcount);
    capacity.reserve(jugcount);
    goal.reserve(jugcount);

    int j;
    cout << "Capacities: ";
    for (j = 0; j < jugcount; j++) {
	int c;
	cin >> c;
	capacity.push_back(c);
    }
    cout << "Initial state: ";
    for (j = 0; j < jugcount; j++) {
	int c;
	cin >> c;
	initial.push_back(c);
    }
    cout << "Goal: ";
    for (j = 0; j < jugcount; j++) {
	int c;
	cin >> c;
	goal.push_back(c);
    }

    cout << "Solving...";

#ifdef WIN32
    LARGE_INTEGER start, stop, freq;
    QueryPerformanceCounter(&start);
#endif
    JugsConfigHistory h = solve_water_heuristics(capacity, initial, goal);
#ifdef WIN32
    QueryPerformanceCounter(&stop);
    QueryPerformanceFrequency(&freq);
    cout << (stop.QuadPart - start.QuadPart) / float(freq.QuadPart) << "s, ";
#endif

    if (h.size()) {
	cout << (h.size() - 1) << " moves: " << endl;
	for (HistoryIterator i = h.rbegin(); i != h.rend(); ++i) {
	    cout << *i << endl;
	}
    } else cout << "no solution found." << endl;

    cout << "Statistics: " << processed_states << " states processed, " << pushed_states << " states pushed, " << updated_states << " state value updates" << endl;

    return 0;
}
