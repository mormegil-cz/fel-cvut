#pragma warning(disable: 4786)	// "long name for debug information, truncated"

#include "water.h"

#include <cassert>

#include <vector>
#include <set>
#include <list>
#include <algorithm>

//#define BFS

#ifdef BFS
#define USE_HEAP
#else
#define USE_HEAP
#define USE_HEURISTICS
#endif

using namespace std;

unsigned processed_states;
unsigned pushed_states;
unsigned updated_states;

const JugsConfig *capacity;
const JugsConfig *initial;
const JugsConfig *goal;
int jug_count;

ostream& operator<<(ostream& out, const JugsConfig& c)
{
    copy(c.begin(), c.end(), ostream_iterator<int,char>(out," "));

    return out;
}

inline int min(int a, int b)
{
    if (a <= b) return a;
    else return b;
}

inline float min(float a, float b)
{
    if (a <= b) return a;
    else return b;
}

#define HEUR_VALUE 1.0f // NOTE: Only for HEUR_VALUE <= 1 is the best solution guaranteed!

class JugsState {
private:
    float hv;
    int	  moves;

    float heuristic_value() const {
	float v = 0;
#ifdef USE_HEURISTICS
	JugsConfigConstIterator i, c, g, e;
	for (i = config.begin(), c = capacity->begin(), g = goal->begin(), e = config.end(); i != e; ++i, ++c, ++g) {
	    if (*i != *g) {
		if (*g == *c || *g == 0) {
		    // either full or empty is correct
		    v += 0.1f * HEUR_VALUE;
		} else {
		    if (*i == *c || *i == 0) {
			// is either empty or full
			v += 1.0f * HEUR_VALUE;
		    } else {
			// is neither empty, nor full, nor correct
			// the rest (now wrong, neither full, nor empty, and neither full, nor empty is correct)
			v += min(*i / *c, (*c - *i) / *c) * HEUR_VALUE;
			//v += 0.9f * HEUR_VALUE;
		    }
		}
	    } // else jug correct => +0
	}
#endif
	return v;
    }

public:
    enum Operation { OP_NOP, OP_EMPTY, OP_FILL, OP_POUR };

    const JugsState *pred;
    JugsConfig config;
    bool processed;

    JugsState(const JugsConfig cfg) : config(cfg), pred(0) {
	processed = false;
	hv = heuristic_value();
    }

    JugsState(const JugsState *from_state, Operation op, int op_param1 = 0, int op_param2 = 0) : config(from_state->config) {
	assert(op_param1 < jug_count && op_param2 < jug_count);

	switch(op) {
	case OP_NOP:
	    break;

	case OP_EMPTY:
	    config[op_param1] = 0;
	    break;

	case OP_FILL:
	    config[op_param1] = (*capacity)[op_param1];
	    break;

	case OP_POUR:
	    {
		int xfer = min(config[op_param1], (*capacity)[op_param2] - config[op_param2]);
		config[op_param1] -= xfer;
		config[op_param2] += xfer;
		break;
	    }

	default:
	    assert(0);
	}

	hv = heuristic_value();
	set_pred(from_state);
	processed = false;
    }

    void set_pred(const JugsState *pred) {
	this->pred = pred;
	moves = pred ? pred->get_moves() + 1 : 0;
    }

    int get_moves() const { 
	//return pred ? pred->get_moves() + 1 : 0;
	return moves;
    }

    float get_value() const {
	//return get_moves() + hv;
	return moves + hv;
    }

    bool operator == (const JugsState &to) const { return config == to.config; }
    bool operator < (const JugsState &than) const { return config < than.config; }
};

struct JugsStateCompare {   // comparator for "JugsState *" containers
    bool operator () (const JugsState *left, const JugsState *right) const {
	return *left < *right;
    }
};

struct JugsStateValueCompare {   // comparator for "JugsState *" containers, by state's value
    bool operator () (const JugsState *left, const JugsState *right) const {
	return left->get_value() > right->get_value();
    }
};

bool operator == (const JugsState *left, const JugsState &right) {
    return *left == right;
}

ostream& operator<<(ostream& out, const JugsState& s)
{
    out << s.config << "(" << s.get_moves() << " moves";
    if (s.pred) out << " via " << s.pred->config;
    out << ")";

    return out;
}

typedef set<JugsState *, JugsStateCompare> KnownStatesSet;
typedef KnownStatesSet::const_iterator JugsStateIterator;
#ifdef USE_HEAP
typedef vector<JugsState *> StatesHeap;
#else
typedef list<JugsState *> StatesHeap;
typedef StatesHeap::iterator StatesHeapIterator;
#endif

KnownStatesSet known_states;
StatesHeap search_space;
JugsStateValueCompare jugsValueComparator;

static inline void try_insert(JugsState &what)
{
    JugsStateIterator i = known_states.find(&what);
    if (i != known_states.end()) {
	// already present
	JugsState *olds = *i;
	//cout << "try_insert: what = " << what << ", olds = " << *olds;
	if (olds->get_moves() > what.get_moves()) {
	    // update moves
	    //cout << "...UPDATE!\n";
	    olds->set_pred(what.pred);
/*
#ifdef USE_HEAP
	    make_heap(search_space.begin(), search_space.end(), jugsValueComparator);
#endif
*/
#ifdef USE_HEAP
	    search_space.push_back(olds);
	    push_heap(search_space.begin(), search_space.end(), jugsValueComparator);
#else
	    search_space.push_back(olds);
#endif
	    updated_states++;
	} //else cout << "...ignored\n";
	return;
    }

    assert(find(search_space.begin(), search_space.end(), what) == search_space.end());

    JugsState *ns = new JugsState(what);
    known_states.insert(ns);
#ifdef USE_HEAP
    search_space.push_back(ns);
    push_heap(search_space.begin(), search_space.end(), jugsValueComparator);
#else
    search_space.push_back(ns);
#endif
    pushed_states++;

    //cout << "push: " << what << endl;
}

JugsConfigHistory solve_water_heuristics(const JugsConfig &cap, const JugsConfig &init, const JugsConfig &g)
{
    jug_count = cap.size();
    assert(init.size() == jug_count && g.size() == jug_count);
    capacity = &cap;
    initial = &init;
    goal = &g;

    // clear the data structures -- required only when called more than once...
    search_space.clear();
    for (JugsStateIterator i = known_states.begin(); i != known_states.end(); ++i) {
	delete *i;
    }
    known_states.clear();
    processed_states = 0;
    pushed_states = 0;
    updated_states = 0;

    // create the initial value and insert it into the queue
    JugsState *init_state = new JugsState(init);
    known_states.insert(init_state);
    search_space.push_back(init_state);

    // go search
    while (!search_space.empty()) { // or, there is a return statement inside, when a solution is found
	JugsState *s;
#ifdef USE_HEAP
	s = *search_space.begin();
	pop_heap(search_space.begin(), search_space.end(), jugsValueComparator);
	search_space.pop_back();
#else
	{
	    StatesHeapIterator hi = search_space.begin();
	    StatesHeapIterator he = search_space.end();
	    StatesHeapIterator bi = hi;
	    s = *hi++;
	    while (hi != he) {
		if (jugsValueComparator(s, *hi)) {
		    s = *hi;
		    bi = hi;
		}
		++hi;
	    }
	    search_space.erase(bi);
	}
#endif
	if (s->processed) continue;
	s->processed = true;
	processed_states++;
	//cout << "popped: " << *s << endl;

	if (s->config == *goal) {
	    // SOLUTION FOUND -- construct the history
	    const JugsState *cs = s;
	    JugsConfigHistory history;
	    while (cs) {
		history.push_back(cs->config);
		cs = cs->pred;
	    }
	    return history;
	}
	// generate all successors of the current state
	int j;
	// 1. by emptying
	for (j = 0; j < jug_count; j++) 
	    if (s->config[j]) {
		//JugsState succ(s, JugsState::OP_EMPTY, j);
		try_insert(JugsState(s, JugsState::OP_EMPTY, j));
	    }
	// 2. by filling
	for (j = 0; j < jug_count; j++) 
	    if (s->config[j] < (*capacity)[j]) {
		try_insert(JugsState(s, JugsState::OP_FILL, j));
	    }
        // 3. by pouring
        for (int f = 0; f < jug_count; f++)
	    if (s->config[f])
		for (int t = 0; t < jug_count; t++)
		    if (f != t && s->config[t] < (*capacity)[t]) {
			try_insert(JugsState(s, JugsState::OP_POUR, f, t));
		    }
    }

    // NO SOLUTION FOUND
    return JugsConfigHistory();
}
