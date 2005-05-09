/*
 * Copyright (C) 2003, Mike Van Emmerik
 *
 * See the file "LICENSE.TERMS" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 *
 */

/*===============================================================================================
 * FILE:	   managed.h
 * OVERVIEW:   Definition of "managed" classes such as StatementSet, which feature makeUnion etc
 * CLASSES:		StatementSet
 *				StatementList
 *				StatementVec
 *				LocationSet
 *==============================================================================================*/

/*
 * 26/Aug/03 - Mike: Split off from statement.h
 */

#ifndef __MANAGED_H__
#define __MANAGED_H__

#include <list>
#include <set>
#include <vector>

#include "exphelp.h"		// For lessExpStar
#include "left.h"

class Statement;
class Assign;
class Exp;
class RefExp;
class Cfg;
class LocationSet;

// A class to implement sets of statements
// We may choose to implement these very differently one day
class StatementSet {
		std::set<Statement*> sset;			// For now, use use standard sets

public:
typedef std::set<Statement*>::iterator iterator;

virtual				~StatementSet() {}
		void		makeUnion(StatementSet& other);		// Set union
		void		makeDiff (StatementSet& other);		// Set difference
		void		makeIsect(StatementSet& other);		// Set intersection
		bool		isSubSetOf(StatementSet& other);	// Subset relation

		unsigned	size() {return sset.size();}		// Number of elements
		//Statement* getFirst(StmtSetIter& it);	  		// Get the first Statement
		//Statement* getNext (StmtSetIter& it);	  		// Get next
		iterator	begin()	{return sset.begin();}
		iterator	end()	{return sset.end();}
		
		void		insert(Statement* s) {sset.insert(s);}	// Insertion
		bool		remove(Statement* s);					// Removal; rets false if not found
		bool		removeIfDefines(Exp* given);			// Remove if given exp is defined
		bool		removeIfDefines(StatementSet& given);	// Remove if any given is def'd
		bool		exists(Statement* s);					// Search; returns false if !found
		bool		definesLoc(Exp* loc);					// Search; returns true if any
															// statement defines loc
		void		clear() {sset.clear();}					// Clear the set
		bool		operator==(const StatementSet& o) const	// Compare if equal
						{ return sset == o.sset;}
		bool		operator<(const StatementSet& o) const;	// Compare if less
		char*		prints();								// Print to std::cerr (for debug)
		void		print(std::ostream& os);				// Print to os
		void		printNums(std::ostream& os);			// Print statements as numbers
		//bool	isLast(StmtSetIter& it);					// returns true if it is at end
};		// class StatementSet

class StatementList {
		std::list<Statement*> slist;		  			// For now, use use standard list

public:
typedef std::list<Statement*>::iterator iterator;
typedef std::list<Statement*>::reverse_iterator reverse_iterator;
virtual				~StatementList() {}
		unsigned	size() {return slist.size();}		 // Number of elements
		iterator	begin()  {return slist.begin();}
		iterator	end()	  {return slist.end();}
		reverse_iterator rbegin() {return slist.rbegin();}
		reverse_iterator rend()	  {return slist.rend();}

		// A special intersection operator; this becomes the intersection of StatementList a (assumed to be a list of
		// Assignment*s) with the LocationSet b.
		// Used for calculating returns for a CallStatement
		void		makeIsect(StatementList& a, LocationSet& b);
		
		void		append(Statement* s) {slist.push_back(s);} // Insert at end
		void		append(StatementList& sl);			// Append whole StatementList
		void		append(StatementSet& sl);			// Append whole StatementSet
		bool		remove(Statement* s);				// Removal; rets false if not found
		// This one is needed where you remove in the middle of a loop
		// Use like this: it = mystatementlist.erase(it);
		iterator	erase(iterator it) {return slist.erase(it);}
		iterator	erase(iterator first, iterator last) {return slist.erase(first, last);}
		iterator	insert(iterator it, Statement* s) {return slist.insert(it, s);}
		bool		exists(Statement* s);				// Search; returns false if not found
		char*		prints();							// Print to string (for debugging)
		void		clear() { slist.clear(); }
		void		makeCloneOf(StatementList& o);		// Make this a clone of o
		bool		existsOnLeft(Exp* loc);				// True if loc exists on the LHS of any Assignment in this list
};		// class StatementList

class StatementVec {
		std::vector<Statement*> svec;			// For now, use use standard vector

public:
typedef std::vector<Statement*>::iterator iterator;
typedef std::vector<Statement*>::reverse_iterator reverse_iterator;
		unsigned	size() {return svec.size();}		 // Number of elements
		iterator	begin() { return svec.begin();}
		iterator	end()	 { return svec.end();}
		reverse_iterator rbegin() { return svec.rbegin();}
		reverse_iterator rend()	  { return svec.rend();}
		// Get/put at position idx (0 based)
		Statement* operator[](int idx) {return svec[idx];}
		void		putAt(int idx, Statement* s);
		iterator	remove(iterator it);
		char*		prints();						// Print to string (for debugging)
		void		printNums(std::ostream& os);
		void		clear() { svec.clear(); }
		bool		operator==(const StatementVec& o) const	// Compare if equal
						{ return svec == o.svec;}
		bool		operator<(const StatementVec& o) const		// Compare if less
						{ return svec < o.svec;}
		void		append(Statement* s) {svec.push_back(s);}
		void		erase(iterator it) {svec.erase(it);}
};	// class StatementVec

// For liveness, we need sets of locations (registers or memory)
class LocationSet {
		// We use a standard set, but with a special "less than" operator so that the sets are ordered
		// by expression value. If this is not done, then two expressions with the same value (say r[10])
		// but that happen to have different addresses (because they came from different statements)
		// would both be stored in the set (instead of the required set behaviour, where only one is stored)
		std::set<Exp*, lessExpStar> sset; 
public:
typedef std::set<Exp*, lessExpStar>::iterator iterator;
					LocationSet() {}						// Default constructor
virtual				~LocationSet() {}						// virtual destructor kills warning
					LocationSet(const LocationSet& o);		// Copy constructor
					LocationSet& operator=(const LocationSet& o); // Assignment
		void		makeUnion(LocationSet& other);			// Set union
		void		makeDiff (LocationSet& other);			// Set difference
		void		clear() {sset.clear();}					// Clear the set
		iterator	begin() {return sset.begin();}
		iterator	end()	 {return sset.end();}
		void		insert(Exp* loc) {sset.insert(loc);}	// Insert the given location
		void		remove(Exp* loc);						// Remove the given location
		void		remove(iterator ll);					// Remove location, given iterator
		void		removeIfDefines(StatementSet& given);	// Remove locs defined in given
		unsigned	size() const {return sset.size();}		// Number of elements
		bool		operator==(const LocationSet& o) const; // Compare
		void		substitute(Assign& a);					// Substitute the given assignment to all
		char*		prints();								// Print to cerr for debugging
		void		print(std::ostream& os);				// Print to os
		bool		exists(Exp* e); 						// Return true if the location exists in the set
		bool		existsNS(Exp* e);						// As above, but no subscripts
		// Return an iterator to the found item (or end() if not). Only really makes sense if e has a wildcard
		iterator	find(Exp* e) {return sset.find(e); }
		// Find a location with a different def, but same expression. For example, pass r28{10},
		// return true if r28{20} in the set. If return true, dr points to the first different ref
		bool		findDifferentRef(RefExp* e, Exp *&dr);
		void		addSubscript(Statement* def /* , Cfg* cfg */);		// Add a subscript to all elements
};	// class LocationSet

#endif	// #ifdef __MANAGED_H__
