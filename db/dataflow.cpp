/*
 * Copyright (C) 2005, Mike Van Emmerik
 *
 * See the file "LICENSE.TERMS" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 *
 */

/*==============================================================================
 * FILE:	   dataflow.cpp
 * OVERVIEW:   Implementation of the DataFlow class
 *============================================================================*/

/*
 * $Revision: 1.43.2.4 $
 * 15 Mar 05 - Mike: Separated from cfg.cpp
 */

#include <sstream>

#include "dataflow.h"
#include "cfg.h"
#include "proc.h"
#include "exp.h"

extern char debug_buffer[];		 // For prints functions


/*
 * Dominator frontier code largely as per Appel 2002 ("Modern Compiler Implementation in Java")
 */

void DataFlow::DFS(int p, int n) {
	if (dfnum[n] == 0) {
		dfnum[n] = N; vertex[N] = n; parent[n] = p;
		N++;
		// For each successor w of n
		PBB bb = BBs[n];
		std::vector<PBB>& outEdges = bb->getOutEdges();
		std::vector<PBB>::iterator oo;
		for (oo = outEdges.begin(); oo != outEdges.end(); oo++) {
			DFS(n, indices[*oo]);
		}
	}
}

void DataFlow::dominators(Cfg* cfg) {
	PBB r = cfg->getEntryBB();
	unsigned numBB = cfg->getNumBBs();
	BBs.resize(numBB, (PBB)-1);
	N = 0; BBs[0] = r; indices[r] = 0;
	// Initialise to "none"
	dfnum.resize(numBB, 0);
	semi.resize(numBB, -1);
	ancestor.resize(numBB, -1);
	idom.resize(numBB, -1);
	samedom.resize(numBB, -1);
	vertex.resize(numBB, -1);
	parent.resize(numBB, -1);
	best.resize(numBB, -1);
	bucket.resize(numBB);
	DF.resize(numBB);
	// Set up the BBs and indices vectors. Do this here because sometimes a BB can be unreachable (so relying on in-edges
	// doesn't work)
	std::list<PBB>::iterator ii;
	int idx = 1;
	for (ii = cfg->begin(); ii != cfg->end(); ii++) {
		PBB bb = *ii;
		if (bb != r) {	   // Entry BB r already done
			indices[bb] = idx;
			BBs[idx++] = bb;
		}
	}
	DFS(-1, 0);
	int i;
	for (i=N-1; i >= 1; i--) {
		int n = vertex[i]; int p = parent[n]; int s = p;
		/* These lines calculate the semi-dominator of n, based on the Semidominator Theorem */
		// for each predecessor v of n
		PBB bb = BBs[n];
		std::vector<PBB>& inEdges = bb->getInEdges();
		std::vector<PBB>::iterator it;
		for (it = inEdges.begin(); it != inEdges.end(); it++) {
			if (indices.find(*it) == indices.end()) {
				std::cerr << "BB not in indices: "; (*it)->print(std::cerr);
				assert(false);
			}
			int v = indices[*it];
			int sdash;
			if (dfnum[v] <= dfnum[n])
				sdash = v;
			else sdash = semi[ancestorWithLowestSemi(v)];
			if (dfnum[sdash] < dfnum[s])
				s = sdash;
		}
		semi[n] = s;
		/* Calculation of n'd dominator is deferred until the path from s to n
			has been linked into the forest */
		bucket[s].insert(n);
		Link(p, n);
		// for each v in bucket[p]
		std::set<int>::iterator jj;
		for (jj=bucket[p].begin(); jj != bucket[p].end(); jj++) {
			int v = *jj;
			/* Now that the path from p to v has been linked into the spanning
				forest, these lines calculate the dominator of v, based on the
				first clause of the Dominator Theorem, or else defer the calc-
				ulation until y's dominator is known. */
			int y = ancestorWithLowestSemi(v);
			if (semi[y] == semi[v])
				idom[v] = p;		 // Success!
			else samedom[v] = y;	 // Defer
		}
		bucket[p].clear();
	}
	for (i=1; i < N-1; i++) {
		/* Now all the deferred dominator calculations, based on the second
			clause of the Dominator Theorem, are performed. */
		int n = vertex[i];
		if (samedom[n] != -1) {
			idom[n] = idom[samedom[n]];	   // Deferred success!
		}
	}
	computeDF(0);				// Finally, compute the dominance frontiers
}

int DataFlow::ancestorWithLowestSemi(int v) {
	int a = ancestor[v];
	if (ancestor[a] != -1) {
		int b = ancestorWithLowestSemi(a);
		ancestor[v] = ancestor[a];
		if (dfnum[semi[b]] < dfnum[semi[best[v]]])
			best[v] = b;
	}
	return best[v];
}

void DataFlow::Link(int p, int n) {
	ancestor[n] = p; best[n] = n;
}

// Return true if n dominates w
bool DataFlow::doesDominate(int n, int w) {
	while (idom[w] != -1) {
		if (idom[w] == n)
			return true;
		w = idom[w];	 // Move up the dominator tree
	}
	return false;
}

void DataFlow::computeDF(int n) {
	std::set<int> S;
	/* THis loop computes DF_local[n] */
	// for each node y in succ(n)
	PBB bb = BBs[n];
	std::vector<PBB>& outEdges = bb->getOutEdges();
	std::vector<PBB>::iterator it;
	for (it = outEdges.begin(); it != outEdges.end(); it++) {
		int y = indices[*it];
		if (idom[y] != n)
			S.insert(y);
	}
	// for each child c of n in the dominator tree
	// Note: this is a linear search!
	int sz = ancestor.size();
	for (int c = 0; c < sz; c++) {
		if (idom[c] != n) continue;
		computeDF(c);
		/* This loop computes DF_up[c] */
		// for each element w of DF[c]
		std::set<int>& s = DF[c];
		std::set<int>::iterator ww;
		for (ww = s.begin(); ww != s.end(); ww++) {
			int w = *ww;
			// if n does not dominate w, or if n = w
			if (n == w || !doesDominate(n, w)) {
				S.insert(w);
			}
		}
	}
	DF[n] = S;
}

void DataFlow::placePhiFunctions(int memDepth, UserProc* proc) {
	// First free some memory no longer needed
	dfnum.resize(0);
	semi.resize(0);
	ancestor.resize(0);
	samedom.resize(0);
	vertex.resize(0);
	parent.resize(0);
	best.resize(0);
	bucket.resize(0);
	defsites.clear();			// Clear defsites map,
	A_orig.clear();				// and A_orig,
	defStmts.clear();			// and the map from variable to defining Stmt 

	// Set the sizes of needed vectors
	unsigned numBB = indices.size();
	Cfg* cfg = proc->getCFG();
	assert(numBB = cfg->getNumBBs());
	A_orig.resize(numBB);

	// We need to create A_orig for the current memory depth
	unsigned n;
	for (n=0; n < numBB; n++) {
		BasicBlock::rtlit rit; StatementList::iterator sit;
		PBB bb = BBs[n];
		for (Statement* s = bb->getFirstStmt(rit, sit); s;
						s = bb->getNextStmt(rit, sit)) {
			LocationSet ls;
			LocationSet::iterator it;
			s->getDefinitions(ls);
			for (it = ls.begin(); it != ls.end(); it++) {
				if ((*it)->getMemDepth() == memDepth) {
					A_orig[n].insert((*it)->clone());
					defStmts[*it] = s;
				}
			}
		}
	}

	// For each node n
	for (n=0; n < numBB; n++) {
		// For each variable a in A_orig[n]
		std::set<Exp*, lessExpStar>& s = A_orig[n];
		std::set<Exp*, lessExpStar>::iterator aa;
		for (aa = s.begin(); aa != s.end(); aa++) {
			Exp* a = *aa;
			defsites[a].insert(n);
		}
	}

	// For each variable a (in defsites)
	std::map<Exp*, std::set<int>, lessExpStar>::iterator mm;
	for (mm = defsites.begin(); mm != defsites.end(); mm++) {
		Exp* a = (*mm).first;				// *mm is pair<Exp*, set<int>>
		std::set<int> W = defsites[a];	 // set copy
		// While W not empty
		while (W.size()) {
			// Remove some node n from W
			int n = *W.begin();				// Copy first element
			W.erase(W.begin());				// Remove first element
			// for each y in DF[n]
			std::set<int>::iterator yy;
			std::set<int>& DFn = DF[n];
			for (yy = DFn.begin(); yy != DFn.end(); yy++) {
				int y = *yy;
				// if y not element of A_phi[a]
				std::set<int>& s = A_phi[a];
				if (s.find(y) == s.end()) {
					// Insert trivial phi function for a at top of block y
					// a := phi()
					Statement* as = new PhiAssign(a->clone());
					PBB Ybb = BBs[y];
					Ybb->prependStmt(as, proc);
					// A_phi[a] <- A_phi[a] U {y}
					s.insert(y);
					// if a !elementof A_orig[y]
					if (A_orig[y].find(a) == A_orig[y].end()) {
						// W <- W U {y}
						W.insert(y);
					}
				}
			}
		}
	}
}

// Subscript dataflow variables
void DataFlow::renameBlockVars(Cfg* cfg, int n, int memDepth, bool clearStack /* = false */ ) {
	// Need to clear the Stack of old, renamed locations like m[esp-4] (these will be deleted, and will cause compare
	// failures in the Stack, so it can't be correctly ordered and hence balanced etc, and will lead to segfaults)
	if (clearStack) Stack.clear();

	// For each statement S in block n
	BasicBlock::rtlit rit; StatementList::iterator sit;
	PBB bb = BBs[n];
	Statement* S;
	for (S = bb->getFirstStmt(rit, sit); S; S = bb->getNextStmt(rit, sit)) {
		// if S is not a phi function
		if (1) { //!S->isPhi()) 
			// For each use of some variable x in S (not just assignments)
			LocationSet locs;
			if (S->isPhi()) {
				if (S->getLeft()->isMemOf() || S->getLeft()->isRegOf())
					S->getLeft()->getSubExp1()->addUsedLocs(locs);
			}
			else
				S->addUsedLocs(locs);
			LocationSet::iterator xx;
			for (xx = locs.begin(); xx != locs.end(); xx++) {
				Exp* x = *xx;
				// Ignore variables of the wrong memory depth
				if (x->getMemDepth() != memDepth) continue;
				// Ignore variables that have already been subscripted
				if (x->isSubscript()) continue;
				Statement* def;
				if (Stack[x].empty()) {
					// If the stack is empty, use a NULL definition. This will be changed into a pointer
					// to an implicit definition at the start of type analysis, but not until all the m[...]
					// have stopped changing their expressions (complicates implicit assignments considerably).
					def = NULL;
				}
				else
					def = Stack[x].top();
				// Replace the use of x with x{def} in S
				if (S->isPhi())
					S->getLeft()->refSubExp1() = S->getLeft()->getSubExp1()->expSubscriptVar(x, def /*, this*/);
				else S->subscriptVar(x, def /*, this */);
			}
		}

		// For each definition of some variable a in S
		// MVE: Check for Call and Return Statements; these have Collector objects that need to be updated
		// Do before the below, so CallStatements have not yet processed their returns
		if (S->isCall() || S->isReturn()) {
			Collector* col;
			if (S->isCall())
				col = ((CallStatement*)S)->getCollector();
			else
				col = ((ReturnStatement*)S)->getCollector();
			col->updateLocs(Stack);
		}

		LocationSet defs;
		S->getDefinitions(defs);
		LocationSet::iterator dd;
		for (dd = defs.begin(); dd != defs.end(); dd++) {
			Exp *a = *dd;
			if (a->getMemDepth() == memDepth) {
				// Push i onto Stack[a]
				// Note: we clone a because otherwise it could be an expression that gets deleted through various
				// modifications. This is necessary because we do several passes of this algorithm with various memory depths
				Stack[a->clone()].push(S);
				// Replace definition of a with definition of a_i in S (we don't do this)
			}
			// MVE: do we need this awful hack?
			if (a->getOper() == opLocal) {
				a = S->getProc()->getLocalExp(((Const*)a->getSubExp1())->getStr());
				// Note: used to assert(a) here. However, with switch statements and in other cases, a local may be
				// created which does not represent memory at all (created with UserProc::newLocal()), and so there
				// is no entry in symbolMap, and so a becomes NULL. This is not an error.
				// Stack already has a definition for a (as just the bare local)
				if (a && a->getMemDepth() == memDepth)
					Stack[a->clone()].push(S);
			}
		}
	}

	// For each successor Y of block n
	std::vector<PBB>& outEdges = bb->getOutEdges();
	unsigned numSucc = outEdges.size();
	for (unsigned succ = 0; succ < numSucc; succ++) {
		PBB Ybb = outEdges[succ];
		// Suppose n is the jth predecessor of Y
		int j = Ybb->whichPred(bb);
		// For each phi-function in Y
		Statement* S;
		for (S = Ybb->getFirstStmt(rit, sit); S; S = Ybb->getNextStmt(rit, sit)) {
			PhiAssign* pa = dynamic_cast<PhiAssign*>(S);
			// if S is not a phi function, then quit the loop (no more phi's)
			// Wrong: do not quit the loop: there's an optimisation that turns a PhiAssign into an ordinary Assign.
			// So continue, not break.
			if (!pa) continue;
			// Suppose the jth operand of the phi is a
			// For now, just get the LHS
			Exp* a = pa->getLeft();
			// Only consider variables of the current memory depth
			if (a->getMemDepth() != memDepth) continue;
			Statement* def;
			if (Stack[a].empty())
				def = NULL;				// See comment above
			else
				def = Stack[a].top();
			// "Replace jth operand with a_i"
			pa->putAt(j, def, a);
		}
	}
	// For each child X of n
	// Note: linear search!
	unsigned numBB = cfg->getNumBBs();
	for (unsigned X=0; X < numBB; X++) {
		if (idom[X] == n)
			renameBlockVars(cfg, X, memDepth);
	}
	// For each statement S in block n
	for (S = bb->getFirstStmt(rit, sit); S; S = bb->getNextStmt(rit, sit)) {
		// For each definition of some variable a in S
		LocationSet defs;
		S->getDefinitions(defs);
		LocationSet::iterator dd;
		for (dd = defs.begin(); dd != defs.end(); dd++) {
			if ((*dd)->getMemDepth() == memDepth)
				Stack[*dd].pop();
		}
	}
}


void Collector::updateLocs(std::map<Exp*, std::stack<Statement*>, lessExpStar>& Stack) {
	std::map<Exp*, std::stack<Statement*>, lessExpStar>::iterator it;
	for (it = Stack.begin(); it != Stack.end(); it++) {
		if (it->second.size() == 0)
			continue;					// This variable's definition doesn't reach here
		RefExp* re = new RefExp(it->first->clone(), it->second.top());
		locs.insert(re);
	}
	initialised = true;
}

// Find the definition for e that reaches this Collector. If none reaches here, return NULL
RefExp* Collector::findDef(Exp* e) {
	RefExp re(e, (Statement*)-1);		// Wrap in a definition with a wild definition
	std::set<RefExp*, lessExpStar>::iterator it = locs.find(&re);
	if (it == locs.end())
		return NULL;					// Not explicitly defined here
	return *it;
}

void Collector::print(std::ostream& os) {
	std::set<RefExp*, lessExpStar>::iterator it;
	for (it=locs.begin(); it != locs.end(); ) {
		os << *it;
		it++;
		if (it != locs.end())
			os << ", ";
	}
}

char* Collector::prints() {
	std::ostringstream ost;
	print(ost);
	strncpy(debug_buffer, ost.str().c_str(), DEBUG_BUFSIZE-1);
	debug_buffer[DEBUG_BUFSIZE-1] = '\0';
	return debug_buffer;
}
