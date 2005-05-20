/*
 * Copyright (C) 2005, Mike Van Emmerik
 *
 * See the file "LICENSE.TERMS" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 *
 */

/*==============================================================================
 * FILE:       dataflow.h
 * OVERVIEW:   Interface for SSA based data flow analysis
 *============================================================================*/

/*
 * $Revision: 1.39.2.14 $
 * 15 Mar 05 - Mike: Separated from cfg.h
 */

#ifndef _DATAFLOW_H_
#define _DATAFLOW_H_

#include <vector>
#include <map>
#include <set>
#include <stack>

#include "exphelp.h"		// For lessExpStar, etc
#include "managed.h"		// For LocationSet

class Cfg;
class BasicBlock;
class Exp;
class RefExp;
class Statement;
class UserProc;

typedef BasicBlock* PBB;

class DataFlow {
	/******************** Dominance Frontier Data *******************/

		/* These first two are not from Appel; they map PBBs to indices */
		std::vector<PBB> BBs;				// Pointers to BBs from indices
		std::map<PBB, int> indices;			// Indices from pointers to BBs
		/*
		 * Calculating the dominance frontier
		 */
		// If there is a path from a to b in the cfg, then a is an ancestor of b
		// if dfnum[a] < denum[b]
		std::vector<int> dfnum;				// Number set in depth first search
		std::vector<int> semi;				// Semi dominators
		std::vector<int> ancestor;			// Defines the forest that becomes the
											// spanning tree
		std::vector<int> idom;				// Immediate dominator
		std::vector<int> samedom;			// ? To do with deferring
		std::vector<int> vertex;			// ?
		std::vector<int> parent;			// Parent in the dominator tree?
		std::vector<int> best;				// Improves ancestorWithLowestSemi
		std::vector<std::set<int> > bucket; // Deferred calculation?
		int			N;						// Current node number in algorithm
		std::vector<std::set<int> > DF;		// The dominance frontiers

		/*
		 * Inserting phi-functions
		 */
		// Array of sets of locations defined in BB n
		std::vector<std::set<Exp*, lessExpStar> > A_orig;
		// Map from expression to set of block numbers
		std::map<Exp*, std::set<int>, lessExpStar > defsites;
		// Array of sets of BBs needing phis
		std::map<Exp*, std::set<int>, lessExpStar> A_phi;
		// A Boomerang requirement: Statements defining particular subscripted locations
		std::map<Exp*, Statement*, lessExpStar> defStmts;

		/*
		 * Renaming variables
		 */
		// The stack which remembers the last definition of an expression.
		// A map from expression (Exp*) to a stack of (pointers to) Statements
		std::map<Exp*, std::stack<Statement*>, lessExpStar> Stacks;


public:
		/*
	 	 * Dominance frontier and SSA code
	 	 */
		void		DFS(int p, int n);
		void		dominators(Cfg* cfg);
		int			ancestorWithLowestSemi(int v);
		void		Link(int p, int n);
		void		computeDF(int n);
		void		placePhiFunctions(int memDepth, UserProc* proc);
		void		renameBlockVars(UserProc* proc, int n, int memDepth, bool clearStacks = false);
		bool		doesDominate(int n, int w);

		// For testing:
		int			pbbToNode(PBB bb) {return indices[bb];}
		std::set<int>& getDF(int node) {return DF[node];}
		PBB			nodeToBB(int node) {return BBs[node];} 
		int			getIdom(int node) {return idom[node];}
		int			getSemi(int node) {return semi[node];}
		std::set<int>& getA_phi(Exp* e) {return A_phi[e];}

		// For debugging:
		void		dumpStacks();

};

/**
 * Collector class. Common code and data for Def and Use Collectors.
 */
class Collector {
protected:
		/*
		 * True if initialised. When not initialised, callees should not subscript parameters inserted into the
		 * associated CallStatement
		 */
		bool		initialised;
		/**
		 * The set of locations. Use lessExpStar to compare properly
		 */
// FIXME: Consider: should DefLocations not be a set at all? The subscript makes the loookup capability of the set
// pretty much useless...
		LocationSet	locs;
public:
		/**
		 * Constructor
		 */
					Collector() : initialised(false) {}

		/**
		 * makeCloneOf(): clone the given Collector into this one
		 */
		void		makeCloneOf(Collector& other);

		/*
		 * Return true if initialised
		 */
		bool		isInitialised() {return initialised;}

		/*
		 * Clear the location set
		 */
		void		clear() {locs.clear(); initialised = false;}

		/*
		 * Insert a new member
		 */
		void		insert(Exp* e) {locs.insert(e);}
		/*
		 * Print the collected locations to stream os
		 */
		void		print(std::ostream& os);

		/*
		 * Print to string (for debugging)
		 */
		char*		prints();

		/*
		 * begin() and end() so we can iterate through the locations
		 */
		typedef LocationSet::iterator iterator;
		iterator	begin() {return locs.begin();}
		iterator	end()	 {return locs.end();}
		bool		exists(Exp* e)	{return locs.exists(e);}			// Note: probably want the NS version...
		bool		existsNS(Exp* e){return locs.findNS(e) != NULL;}	// No Subscripts version
		Exp*		findNS(Exp* e)	{return locs.findNS(e);}			// Find the expression (no subscripts)
		LocationSet& getLocSet() {return locs;}
};

/**
 * DefCollector class. This class collects all definitions that reach the statement that contains this collector.
 */
class DefCollector : public Collector{
public:
		/*
		 * Constructor
		 */
					DefCollector() { }

		/*
		 * Clone
		 */
		DefCollector* clone();

		/*
		 * Update the locations with the current set of reaching definitions
		 */
		void		updateLocs(std::map<Exp*, std::stack<Statement*>, lessExpStar>& Stacks);

		/**
		 * Find the definition for a location. If not found, return NULL
		 */
		RefExp*		findDefFor(Exp* e);

		/**
		 * Search and replace all occurrences
		 */
		void		searchReplaceAll(Exp* from, Exp* to, bool& change);
};

/**
 * UseCollector class. This class collects all uses (live variables) that will be defined by the statement that 
 * contains this collector.
 */
class UseCollector : public Collector {
public:
		/*
		 * Add a new use from Statement u
		 */
		void		updateLocs(Statement* u);

};


#endif	// _DATAFLOW_H_

