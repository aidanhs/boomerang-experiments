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
 * $Revision: 1.39.2.1 $
 * 15 Mar 05 - Mike: Separated from cfg.h
 */

#ifndef _DATAFLOW_H_
#define _DATAFLOW_H_

#include <vector>
#include <map>
#include <set>
#include <stack>

#include "exphelp.h"

class Cfg;
class BasicBlock;
class Exp;
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
		std::map<Exp*, std::stack<Statement*>, lessExpStar> Stack;


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
		void		renameBlockVars(Cfg* cfg, int n, int memDepth, bool clearStack = false);
		bool		doesDominate(int n, int w);

		// For testing:
		int			pbbToNode(PBB bb) {return indices[bb];}
		std::set<int>& getDF(int node) {return DF[node];}
		PBB			nodeToBB(int node) {return BBs[node];} 
		int			getIdom(int node) {return idom[node];}
		int			getSemi(int node) {return semi[node];}
		std::set<int>& getA_phi(Exp* e) {return A_phi[e];}

};

#endif	// _DATAFLOW_H_
