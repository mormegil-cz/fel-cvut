#ifndef __WATER_H_
#define __WATER_H_

#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;

extern unsigned processed_states;
extern unsigned pushed_states;
extern unsigned updated_states;

typedef vector<int> JugsConfig;
typedef vector<JugsConfig> JugsConfigHistory;
typedef JugsConfig::const_iterator JugsConfigConstIterator;

JugsConfigHistory solve_water_heuristics(const JugsConfig &capacity, const JugsConfig &initial, const JugsConfig &goal);

ostream& operator<<(ostream& out, const JugsConfig& c);

#endif // ifndef __WATER_H_
