/*
 * Copyright (C) 2004, Mike Van Emmerik and Trent Waddington
 */
/*==============================================================================
 * FILE:	   visitor.cpp
 * OVERVIEW:   Provides the implementation for the various visitor and modifier
 *			   classes.
 *============================================================================*/
/*
 * $Revision: 1.23.2.10 $
 *
 * 14 Jun 04 - Mike: Created, from work started by Trent in 2003
 */

#include "visitor.h"
#include "exp.h"
#include "statement.h"
#include "log.h"
#include "boomerang.h"		// For VERBOSE
#include "proc.h"
#include "signature.h"


// FixProcVisitor class

bool FixProcVisitor::visit(Location* l, bool& override) {
	l->setProc(proc);		// Set the proc, but only for Locations
	override = false;		// Use normal accept logic
	return true;
}

// GetProcVisitor class

bool GetProcVisitor::visit(Location* l, bool& override) {
	proc = l->getProc();
	override = false;
	return proc == NULL;		// Continue recursion only if failed so far
}

// SetConscripts class

bool SetConscripts::visit(Const* c) {
	if (!bInLocalGlobal) {
		if (bClear)
			c->setConscript(0);
		else
			c->setConscript(++curConscript);
	}
	bInLocalGlobal = false;
	return true;	   // Continue recursion
}

bool SetConscripts::visit(Location* l, bool& override) {
	OPER op = l->getOper();
	if (op == opLocal || op == opGlobal || op == opRegOf || op == opParam)
		bInLocalGlobal = true;
	override = false;
	return true;	   // Continue recursion
}

bool SetConscripts::visit(Binary* b, bool& override) {
	OPER op = b->getOper();
	if (op == opSize)
		bInLocalGlobal = true;
	override = false;
	return true;	   // Continue recursion
}


bool StmtVisitor::visit(RTL* rtl) {
	// Mostly, don't do anything at the RTL level
	return true;
} 

bool StmtConscriptSetter::visit(Assign* stmt) {
	SetConscripts sc(curConscript, bClear);
	stmt->getLeft()->accept(&sc);
	stmt->getRight()->accept(&sc);
	curConscript = sc.getLast();
	return true;
}
bool StmtConscriptSetter::visit(PhiAssign* stmt) {
	SetConscripts sc(curConscript, bClear);
	stmt->getLeft()->accept(&sc);
	curConscript = sc.getLast();
	return true;
}
bool StmtConscriptSetter::visit(ImplicitAssign* stmt) {
	SetConscripts sc(curConscript, bClear);
	stmt->getLeft()->accept(&sc);
	curConscript = sc.getLast();
	return true;
}

bool StmtConscriptSetter::visit(CallStatement* stmt) {
	SetConscripts sc(curConscript, bClear);
	StatementList& args = stmt->getArguments();
	StatementList::iterator ss;
	for (ss = args.begin(); ss != args.end(); ++ss)
		(*ss)->accept(this);
#if 0
	std::vector<Exp*>& impargs = stmt->getImplicitArguments();
	n = impargs.size();
	for (i=0; i < n; i++)
		impargs[i]->accept(&sc);
#endif
#if 0		// Note sure...
	n = stmt->getNumReturns();
	for (i=0; i < n; i++) {
		Exp* r = stmt->getReturnExp(i);
		if (r) r->accept(&sc);
	}
#endif
	curConscript = sc.getLast();
	return true;
}

bool StmtConscriptSetter::visit(CaseStatement* stmt) {
	SetConscripts sc(curConscript, bClear);
	SWITCH_INFO* si = stmt->getSwitchInfo();
	if (si) {
		si->pSwitchVar->accept(&sc);
		curConscript = sc.getLast();
	}
	return true;
}

bool StmtConscriptSetter::visit(ReturnStatement* stmt) {
	SetConscripts sc(curConscript, bClear);
	ReturnStatement::iterator rr;
	for (rr = stmt->begin(); rr != stmt->end(); ++rr)
		(*rr)->accept(this);
	curConscript = sc.getLast();
	return true;
}

bool StmtConscriptSetter::visit(BoolAssign* stmt) {
	SetConscripts sc(curConscript, bClear);
	stmt->getCondExpr()->accept(&sc);
	stmt->getLeft()->accept(&sc);
	curConscript = sc.getLast();
	return true;
}

bool StmtConscriptSetter::visit(BranchStatement* stmt) {
	SetConscripts sc(curConscript, bClear);
	stmt->getCondExpr()->accept(&sc);
	curConscript = sc.getLast();
	return true;
}

void PhiStripper::visit(PhiAssign* s, bool& recur) {
	del = true;
	recur = true;
}

Exp* CallRefsFixer::postVisit(RefExp* r) {
	Exp* ret = r;
	// If child was modified, simplify now
	if (!(unchanged & ~mask)) ret = r->simplify();
	mask >>= 1;
	// Note: r will always == ret here, so the below is safe
	Statement* def = r->getDef();
	CallStatement *call = dynamic_cast<CallStatement*>(def);
	if (call) {
		// Get the right had side of the proven expression (e.g. from r28 = r28 + 4, get r28 + 4)
		Exp *e = call->getProven(r->getSubExp1());
		if (e) {
			// Express e in terms of the definitions reaching the call
			e = e->clone();					// So that the expression in the Proc is not altered
			e = call->localiseExp(e);
			assert(e);
			if (VERBOSE)
				LOG << "fixcall refs replacing " << r << " with " << e << "\n";
			// e = e->simplify();	// No: simplify the parent
			unchanged &= ~mask;
			mod = true;
			return e;
		} else {
			Exp* subExp1 = r->getSubExp1();
			if (call->findDefine(subExp1) == -1) {
				if (VERBOSE && !subExp1->isPC()) {
					LOG << "nothing proven about " << subExp1 << " and yet it is referenced by stmt " <<
						enclosingStmt->getNumber() << ", and not in returns of " << "\n" << "	" << call << "\n";
				}
			}
		}
	}
	return ret;
}


Exp* CallRefsFixer::postVisit(Unary *e)	   {
	bool isAddrOfMem = e->isAddrOf() && e->getSubExp1()->isMemOf();
	if (isAddrOfMem) return e;
	Exp* ret = e;
	if (!(unchanged & mask)) ret = e->simplify();
	mask >>= 1;
	return ret;
}
Exp* CallRefsFixer::postVisit(Binary *e)	{
	Exp* ret = e;
	if (!(unchanged & mask)) ret = e->simplifyArith()->simplify();
	mask >>= 1;
	return ret;
}
Exp* CallRefsFixer::postVisit(Ternary *e)	 {
	Exp* ret = e;
	if (!(unchanged & mask)) ret = e->simplify();
	mask >>= 1;
	return ret;
}
Exp* CallRefsFixer::postVisit(TypedExp *e)	  {
	Exp* ret = e;
	if (!(unchanged & mask)) ret = e->simplify();
	mask >>= 1;
	return ret;
}
Exp* CallRefsFixer::postVisit(FlagDef *e)	 {
	Exp* ret = e;
	if (!(unchanged & mask)) ret = e->simplify();
	mask >>= 1;
	return ret;
}
Exp* CallRefsFixer::postVisit(Location *e)	  {
	Exp* ret = e;
	if (!(unchanged & mask)) ret = e->simplify();
	mask >>= 1;
	return ret;
}
Exp* CallRefsFixer::postVisit(Const *e)	   {
	mask >>= 1;
	return e;
}
Exp* CallRefsFixer::postVisit(TypeVal *e)	 {
	mask >>= 1;
	return e;
}
Exp* CallRefsFixer::postVisit(Terminal *e)	  {
	mask >>= 1;
	return e;
}

// Add used locations finder
bool UsedLocsFinder::visit(Location* e, bool& override) {
	used->insert(e);		// All locations visited are used
	if (e->isMemOf()) {
		// Example: m[r28{10} - 4]	we use r28{10}
		Exp* child = e->getSubExp1();
		child->accept(this);
	}
	override = false;
	return true;
}

bool UsedLocsFinder::visit(Terminal* e) {
	switch (e->getOper()) {
		case opPC:
		case opFlags:
		case opFflags:
		// Fall through
		// The carry flag can be used in some SPARC idioms, etc
		case opDF: case opCF: case opZF: case opNF: case opOF:	// also these
			used->insert(e);
		default:
			break;
	}
	return true;		// Always continue recursion
}

bool UsedLocsFinder::visit(RefExp* e, bool& override) {
	used->insert(e);		 // This location is used
	// However, e's subexpression is NOT used ...
	override = true;
	// ... unless that is a m[x], in which case x (not m[x]) is used
	Exp* refd = e->getSubExp1();
	if (refd->isMemOf()) {
		Exp* x = refd->getSubExp1();
		x->accept(this);
	}
	return true;
}

bool UsedLocsVisitor::visit(Assign* s, bool& override) {
	Exp* lhs = s->getLeft();
	Exp* rhs = s->getRight();
	if (rhs) rhs->accept(ev);
	// Special logic for the LHS. Note: PPC can have r[tmp + 30] on LHS
	if (lhs->isMemOf() || lhs->isRegOf()) {
		Exp* child = ((Location*)lhs)->getSubExp1();	// m[xxx] uses xxx
		child->accept(ev);
	} else if (lhs->getOper() == opArraySubscript || lhs->getOper() == opMemberAccess) {
		Exp* subExp1 = ((Binary*)lhs)->getSubExp1();	// array(base, index) and member(base, offset)?? use
		subExp1->accept(ev);							// base and index
		Exp* subExp2 = ((Binary*)lhs)->getSubExp2();
		subExp2->accept(ev);
	} else if (lhs->getOper() == opAt) {				// foo@[first:last] uses foo, first, and last
		Exp* subExp1 = ((Ternary*)lhs)->getSubExp1();
		subExp1->accept(ev);
		Exp* subExp2 = ((Ternary*)lhs)->getSubExp2();
		subExp2->accept(ev);
		Exp* subExp3 = ((Ternary*)lhs)->getSubExp3();
		subExp3->accept(ev);
	}
	override = true;				// Don't do the usual accept logic
	return true;					// Continue the recursion
}
bool UsedLocsVisitor::visit(PhiAssign* s, bool& override) {
	Exp* lhs = s->getLeft();
	// Special logic for the LHS
	if (lhs->isMemOf()) {
		Exp* child = ((Location*)lhs)->getSubExp1();
		child->accept(ev);
	} else if (lhs->getOper() == opArraySubscript || lhs->getOper() == opMemberAccess) {
		Exp* subExp1 = ((Binary*)lhs)->getSubExp1();
		subExp1->accept(ev);
		Exp* subExp2 = ((Binary*)lhs)->getSubExp2();
		subExp2->accept(ev);
	}
	PhiAssign::iterator uu;
	for (uu = s->begin(); uu != s->end(); uu++) {
		// Note: don't make the RefExp based on lhs, since it is possible that the lhs was renamed in fromSSA()
		// Use the actual expression in the PhiAssign
		RefExp* temp = new RefExp(uu->e, uu->def);
		temp->accept(ev);
	}

	override = true;				// Don't do the usual accept logic
	return true;					// Continue the recursion
}
bool UsedLocsVisitor::visit(ImplicitAssign* s, bool& override) {
	Exp* lhs = s->getLeft();
	// Special logic for the LHS
	if (lhs->isMemOf()) {
		Exp* child = ((Location*)lhs)->getSubExp1();
		child->accept(ev);
	} else if (lhs->getOper() == opArraySubscript || lhs->getOper() == opMemberAccess) {
		Exp* subExp1 = ((Binary*)lhs)->getSubExp1();
		subExp1->accept(ev);
		Exp* subExp2 = ((Binary*)lhs)->getSubExp2();
		subExp2->accept(ev);
	}
	override = true;				// Don't do the usual accept logic
	return true;					// Continue the recursion
}

bool UsedLocsVisitor::visit(CallStatement* s, bool& override) {
	Exp* pDest = s->getDest();
	if (pDest)
		pDest->accept(ev);
	StatementList::iterator it;
	StatementList& arguments = s->getArguments();
	for (it = arguments.begin(); it != arguments.end(); it++)
		(*it)->accept(this);
#if 0
	if (!final) {
		// Ignore the implicit arguments when final
		int n = s->getNumImplicitArguments();
		for (int i=0; i < n; i++)
			s->getImplicitArgumentExp(i)->accept(ev);
	}
#endif
	// For the final pass, also only consider the first return
#if 0
	int n = s->getNumReturns();
	if (final) {
		if (n != 0) {
			Exp* r = NULL;
			for (int i = 0; r == NULL && i < n; i++)
				r = s->getReturnExp(i);			
			// If of form m[x] then x is used
			if (r && r->isMemOf()) {
				Exp* x = ((Location*)r)->getSubExp1();
				x->accept(ev);
			}
		}
	} else {
		// Otherwise, consider all returns. If of form m[x] then x is used
		for (int i=0; i < n; i++) {
			Exp* r = s->getReturnExp(i);
			if (r && r->isMemOf()) {
				Exp* x = ((Location*)r)->getSubExp1();
				x->accept(ev);
			}
		} 
	}
#endif
	override = true;			// Don't do the normal accept logic
	return true;				// Continue the recursion
}

bool UsedLocsVisitor::visit(ReturnStatement* s, bool& override) {
	// For the final pass, only consider the first return
	int n = s->getNumReturns();
	ReturnStatement::iterator rr;
	if (final) {
		if (n != 0) {
			// Visit the first return
			(*s->begin())->accept(this);
		}
	} else {
		// Otherwise, consider all returns. If of form m[x] then x is used
		for (rr = s->begin(); rr != s->end(); ++rr)
			(*rr)->accept(this);
	}
	override = true;			// Don't do the normal accept logic
	return true;				// Continue the recursion
}

bool UsedLocsVisitor::visit(BoolAssign* s, bool& override) {
	Exp* pCond = s->getCondExpr();
	if (pCond)
		pCond->accept(ev);				// Condition is used
	Exp* lhs = s->getLeft();
	if (lhs && lhs->isMemOf()) {	// If dest is of form m[x]...
		Exp* x = ((Location*)lhs)->getSubExp1();
		x->accept(ev);					// ... then x is used
	} else if (lhs->getOper() == opArraySubscript || lhs->getOper() == opMemberAccess) {
		Exp* subExp1 = ((Binary*)lhs)->getSubExp1();
		subExp1->accept(ev);
		Exp* subExp2 = ((Binary*)lhs)->getSubExp2();
		subExp2->accept(ev);
	}
	override = true;			// Don't do the normal accept logic
	return true;				// Continue the recursion
}

//
// Expression subscripter
//
Exp* ExpSubscripter::preVisit(Location* e, bool& recur) {
	if (/* search == NULL || */ *e == *search) {
		recur = e->isMemOf();			// Don't double subscript unless m[...]
		return new RefExp(e, def);		// Was replaced by postVisit below
	}
	recur = true;
	return e;
}

#if 0
Exp* ExpSubscripter::postVisit(Location* e) {
	Exp* ret;
	if (search == NULL || *e == *search) {
		Statement* oldDef = cfg->preUpdate(e);
		if (search == NULL)
			ret = new RefExp(e, cfg->findImplicitAssign(e));
		else
			ret = new RefExp(e, def);
		cfg->postUpdate(ret, oldDef);
	}
	else
		ret = e;
	return ret;
}
#endif

Exp* ExpSubscripter::preVisit(Terminal* e) {
#if 0
	if (search == NULL)
		return new RefExp(e, cfg->findImplicitAssign(e));
	else
#endif
	if (*e == *search)
		return new RefExp(e, def);
	return e;
}

Exp* ExpSubscripter::preVisit(RefExp* e, bool& recur) {
	Exp* base = e->getSubExp1();
	if (*base == *search) {
		recur = false;		// Don't recurse; would double subscript
		e->setDef(def);
		return e;
	}
	recur = true;
	return e;
}

// The Statement subscripter class
void StmtSubscripter::visit(Assign* s, bool& recur) {
	Exp* rhs = s->getRight();
	s->setRight(rhs->accept(mod));
	// Don't subscript the LHS of an assign, ever
	Exp* lhs = s->getLeft();
	if (lhs->isMemOf() || lhs->isRegOf()) {
		Exp*& child = ((Location*)lhs)->refSubExp1();
		child = child->accept(mod);
	}
	recur = false;
}
void StmtSubscripter::visit(PhiAssign* s, bool& recur) {
	Exp* lhs = s->getLeft();
	if (lhs->isMemOf()) {
		Exp*& child = ((Location*)lhs)->refSubExp1();
		child = child->accept(mod);
	}
	recur = false;
}
void StmtSubscripter::visit(ImplicitAssign* s, bool& recur) {
	Exp* lhs = s->getLeft();
	if (lhs->isMemOf()) {
		Exp*& child = ((Location*)lhs)->refSubExp1();
		child = child->accept(mod);
	}
	recur = false;
}
void StmtSubscripter::visit(BoolAssign* s, bool& recur) {
	Exp* lhs = s->getLeft();
	if (lhs->isMemOf()) {
		Exp*& child = ((Location*)lhs)->refSubExp1();
		child = child->accept(mod);
	}
	Exp* rhs = s->getCondExpr();
	s->setCondExpr(rhs->accept(mod));
	recur = false;
}

void StmtSubscripter::visit(CallStatement* s, bool& recur) {
	Exp* pDest = s->getDest();
	if (pDest)
		s->setDest(pDest->accept(mod));
	// Subscript the ordinary arguments
	StatementList& arguments = s->getArguments();
	StatementList::iterator ss;
	for (ss = arguments.begin(); ss != arguments.end(); ++ss)
		(*ss)->accept(this);
#if 0
	// Subscript the implicit arguments
	std::vector<Exp*>& implicits = s->getImplicitArguments();
	n = implicits.size();
	for (int i=0; i < n; i++)
		implicits[i] = implicits[i]->accept(mod);
#endif
	// Returns are like the LHS of an assignment; don't subscript them directly (only if m[x], and then only subscript
	// the x's)
#if 0
	n = s->getNumReturns();
	for (int i=0; i < n; i++) {
		Exp* r = s->getReturnExp(i);
		if (r && r->isMemOf()) {
			Exp*& x = ((Location*)r)->refSubExp1();
			x = x->accept(mod);
		}
	}
#endif
	recur = false;			// Don't do the usual accept logic
}


// Size stripper
Exp* SizeStripper::preVisit(Binary* b, bool& recur) {
	recur = true;			// Visit the binary's children
	if (b->isSizeCast())
		// Could be a size cast of a size cast
		return b->getSubExp2()->stripSizes();
	return b;
}

Exp* ExpConstCaster::preVisit(Const* c) {
	if (c->getConscript() == num) {
		changed = true;
		return new TypedExp(ty, c);
	}
	return c;
}


// This is the code (apart from definitions) to find all constants in a Statement
bool ConstFinder::visit(Const* e) {
	lc.push_back(e);
	return true;
}
bool ConstFinder::visit(Location* e, bool& override) {
	if (e->isMemOf())
		override = false;		// We DO want to see constants in memofs
	else
		override = true;		// Don't consider register numbers, global names, etc
	return true;			
}

// This is in the POST visit function, because it's important to process any child expressions first.
// Otherwise, for m[r28{0} - 12]{0}, you could be adding an implicit assignment with a NULL definition for r28.
Exp* ImplicitConverter::postVisit(RefExp* e) {
	if (e->getDef() == NULL)
		e->setDef(cfg->findImplicitAssign(e->getSubExp1()));
	return e;
}

void StmtImplicitConverter::visit(PhiAssign* s, bool& recur) {
	// The LHS could be a m[x] where x has a null subscript; must do first
	s->setLeft(s->getLeft()->accept(mod));
	PhiAssign::iterator uu;
	for (uu = s->begin(); uu != s->end(); uu++)
		if (uu->def == NULL)
			uu->def = cfg->findImplicitAssign(uu->e);
	recur = false;		// Already done LHS
}

// Localiser. Subscript a location with the definitions that reach the call, or with {-} if none
Exp* Localiser::preVisit(RefExp* e, bool& recur) {
	recur = false;				// Don't recurse into already subscripted variables
	return e;
}

Exp* Localiser::preVisit(Location* e, bool& recur) {
	int d = e->getMemDepth();
	if (d <= depth)				// Don't recurse if depth already too low, or equal
		recur = false;
	return e;
}

Exp* Localiser::postVisit(Location* e) {
	int d = e->getMemDepth();
	if (d != depth && depth != -1) return e;	// Only subscript at the requested depth, or any if depth == -1
	Exp* r = call->findDefFor(e);
	Exp* ret;
	if (r)
		ret = r->clone();
	else
		ret = new RefExp(e, NULL);				// No definition reaches, so subscript with {-}
	return ret;
}

