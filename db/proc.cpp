/*
 * Copyright (C) 1997-2001, The University of Queensland
 * Copyright (C) 2000-2001, Sun Microsystems, Inc
 * Copyright (C) 2002, Trent Waddington
 *
 * See the file "LICENSE.TERMS" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 *
 */

/*==============================================================================
 * FILE:	   proc.cc
 * OVERVIEW:   Implementation of the Proc hierachy (Proc, UserProc, LibProc).
 *			   All aspects of a procedure, apart from the actual code in the
 *			   Cfg, are stored here
 *
 * Copyright (C) 1997-2001, The University of Queensland, BT group
 * Copyright (C) 2000-2001, Sun Microsystems, Inc
 *============================================================================*/

/*
 * $Revision: 1.238.2.21 $
 *
 * 14 Mar 02 - Mike: Fixed a problem caused with 16-bit pushes in richards2
 * 20 Apr 02 - Mike: Mods for boomerang
 * 31 Jan 03 - Mike: Tabs and indenting
 * 03 Feb 03 - Mike: removeStatement no longer linear searches for the BB
 */

/*==============================================================================
 * Dependencies.
 *============================================================================*/

#include <types.h>
#include <sstream>
#include <algorithm>		// For find()
#include "type.h"
#include "cluster.h"
#include "statement.h"
#include "exp.h"
#include "cfg.h"
#include "register.h"
#include "rtl.h"
#include "proc.h"
#include "prog.h"
#include "BinaryFile.h"
#include "frontend.h"
#include "util.h"
#include "signature.h"
#include "hllcode.h"
#include "boomerang.h"
#include "constraint.h"
#include "visitor.h"
#include "dataflow.h"

typedef std::map<Statement*, int> RefCounter;

extern char debug_buffer[];		// Defined in basicblock.cpp, size DEBUG_BUFSIZE

/************************
 * Proc methods.
 ***********************/

Proc::~Proc()
{}

/*==============================================================================
 * FUNCTION:		Proc::Proc
 * OVERVIEW:		Constructor with name, native address.
 * PARAMETERS:		uNative - Native address of entry point of procedure
 * RETURNS:			<nothing>
 *============================================================================*/
Proc::Proc(Prog *prog, ADDRESS uNative, Signature *sig)
	 : prog(prog), signature(sig), address(uNative), m_firstCaller(NULL) 
{cluster = prog->getRootCluster();}

/*==============================================================================
 * FUNCTION:		Proc::getName
 * OVERVIEW:		Returns the name of this procedure
 * PARAMETERS:		<none>
 * RETURNS:			the name of this procedure
 *============================================================================*/
const char* Proc::getName() {
	assert(signature);
	return signature->getName();
}

/*==============================================================================
 * FUNCTION:		Proc::setName
 * OVERVIEW:		Sets the name of this procedure
 * PARAMETERS:		new name
 * RETURNS:			<nothing>
 *============================================================================*/
void Proc::setName(const char *nam) {
	assert(signature);
	signature->setName(nam);
}


/*==============================================================================
 * FUNCTION:		Proc::getNativeAddress
 * OVERVIEW:		Get the native address (entry point).
 * PARAMETERS:		<none>
 * RETURNS:			the native address of this procedure (entry point)
 *============================================================================*/
ADDRESS Proc::getNativeAddress() {
	return address;
}

void Proc::setNativeAddress(ADDRESS a) {
	address = a;
}

/*==============================================================================
 * FUNCTION:	  Proc::containsAddr
 * OVERVIEW:	  Return true if this procedure contains the given address
 * PARAMETERS:	  address
 * RETURNS:		  true if it does
 *============================================================================*/
bool UserProc::containsAddr(ADDRESS uAddr) {
	BB_IT it;
	for (PBB bb = cfg->getFirstBB(it); bb; bb = cfg->getNextBB(it))
		if (bb->getRTLs() && bb->getLowAddr() <= uAddr && bb->getHiAddr() >= uAddr)
			return true;	
	return false;
}

void Proc::renameParam(const char *oldName, const char *newName) { 
	signature->renameParam(oldName, newName); 
}

void UserProc::renameParam(const char *oldName, const char *newName)
{
	oldName = strdup(oldName);
	Proc::renameParam(oldName, newName);
	//cfg->searchAndReplace(Location::param(strdup(oldName), this), Location::param(strdup(newName), this));
}

void UserProc::setParamType(const char* nam, Type* ty) {
	signature->setParamType(nam, ty);
}

void UserProc::renameLocal(const char *oldName, const char *newName) {
	Type *t = locals[oldName];
	Exp *e = expFromSymbol(oldName);
	locals.erase(oldName);
	//Exp *l = symbolMap[e];
	Exp *n = Location::local(strdup(newName), this);
	symbolMap[e] = n;
	locals[strdup(newName)] = t;
	//cfg->searchAndReplace(l, n);
}

bool UserProc::searchAll(Exp* search, std::list<Exp*> &result)
{
	return cfg->searchAll(search, result);	
}

void Proc::printCallGraphXML(std::ostream &os, int depth, bool recurse)
{
	if (!DUMP_XML)
		return;
	visited = true;
	for (int i = 0; i < depth; i++)
		os << "	  ";
	os << "<proc name=\"" << getName() << "\"/>\n";
}

void UserProc::printCallGraphXML(std::ostream &os, int depth, bool recurse)
{
	if (!DUMP_XML)
		return;
	bool wasVisited = visited;
	visited = true;
	int i;
	for (i = 0; i < depth; i++)
		os << "	  ";
	os << "<proc name=\"" << getName() << "\">\n";
	if (recurse) {
		for (std::list<Proc*>::iterator it = calleeList.begin(); it != calleeList.end(); it++) 
			(*it)->printCallGraphXML(os, depth+1, !wasVisited && !(*it)->isVisited());
	}
	for (i = 0; i < depth; i++)
		os << "	  ";
	os << "</proc>\n";
}

void Proc::printDetailsXML() {
	if (!DUMP_XML)
		return;
	std::ofstream out((Boomerang::get()->getOutputPath() + getName() + "-details.xml").c_str());
	out << "<proc name=\"" << getName() << "\">\n";
	unsigned i;
	for (i = 0; i < signature->getNumParams(); i++)
		out << "   <param name=\"" << signature->getParamName(i) << "\" " << "exp=\"" << signature->getParamExp(i)
			<< "\" " << "type=\"" << signature->getParamType(i)->getCtype() << "\"\n";
	for (i = 0; i < signature->getNumReturns(); i++)
		out << "   <return exp=\"" << signature->getReturnExp(i) << "\" "
			<< "type=\"" << signature->getReturnType(i)->getCtype() << "\"/>\n";
	out << "</proc>\n";
	out.close();
}

void UserProc::printDecodedXML()
{
	if (!DUMP_XML)
		return;
	std::ofstream out((Boomerang::get()->getOutputPath() + getName() + "-decoded.xml").c_str());
	out << "<proc name=\"" << getName() << "\">\n";
	out << "	<decoded>\n";
	std::ostringstream os;
	print(os);
	std::string s = os.str();
	escapeXMLChars(s);
	out << s;
	out << "	</decoded>\n";
	out << "</proc>\n";
	out.close();
}

void UserProc::printAnalysedXML()
{
	if (!DUMP_XML)
		return;
	std::ofstream out((Boomerang::get()->getOutputPath() + getName() + "-analysed.xml").c_str());
	out << "<proc name=\"" << getName() << "\">\n";
	out << "	<analysed>\n";
	std::ostringstream os;
	print(os);
	std::string s = os.str();
	escapeXMLChars(s);
	out << s;
	out << "	</analysed>\n";
	out << "</proc>\n";
	out.close();
}

void UserProc::printSSAXML()
{
	if (!DUMP_XML)
		return;
	std::ofstream out((Boomerang::get()->getOutputPath() + getName() + "-ssa.xml").c_str());
	out << "<proc name=\"" << getName() << "\">\n";
	out << "	<ssa>\n";
	std::ostringstream os;
	print(os);
	std::string s = os.str();
	escapeXMLChars(s);
	out << s;
	out << "	</ssa>\n";
	out << "</proc>\n";
	out.close();
}


void UserProc::printXML()
{
	if (!DUMP_XML)
		return;
	printDetailsXML();
	printSSAXML();
	prog->printCallGraphXML();
	printUseGraph();
}

void UserProc::printUseGraph()
{
	std::ofstream out((Boomerang::get()->getOutputPath() + getName() + "-usegraph.dot").c_str());
	out << "digraph " << getName() << " {\n";
	StatementList stmts;
	getStatements(stmts);
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		if (s->isPhi())
			out << s->getNumber() << " [shape=diamond];\n";
		LocationSet refs;
		s->addUsedLocs(refs);
		LocationSet::iterator rr;
		for (rr = refs.begin(); rr != refs.end(); rr++) {
			if (((Exp*)*rr)->isSubscript()) {
				RefExp *r = (RefExp*)*rr;
				if (r->getDef())
					out << r->getDef()->getNumber() << " -> " << s->getNumber() << ";\n";
			}
		}
	}
	out << "}\n";
	out.close();
}

/*==============================================================================
 * FUNCTION:		operator<<
 * OVERVIEW:		Output operator for a Proc object.
 * PARAMETERS:		os - output stream
 *					proc -
 * RETURNS:			os
 *============================================================================*/
//std::ostream& operator<<(std::ostream& os, Proc& proc) {
//	return proc.put(os);
//}


Proc *Proc::getFirstCaller() { 
	if (m_firstCaller == NULL && m_firstCallerAddr != NO_ADDRESS) {
		m_firstCaller = prog->findProc(m_firstCallerAddr);
		m_firstCallerAddr = NO_ADDRESS;
	}

	return m_firstCaller; 
}

/**********************
 * LibProc methods.
 *********************/

/*==============================================================================
 * FUNCTION:		LibProc::LibProc
 * OVERVIEW:		Constructor with name, native address.
 * PARAMETERS:		name - Name of procedure
 *					uNative - Native address of entry point of procedure
 * RETURNS:			<nothing>
 *============================================================================*/
LibProc::LibProc(Prog *prog, std::string& name, ADDRESS uNative) : Proc(prog, uNative, NULL) {
	Signature* sig = prog->getLibSignature(name.c_str());
	signature = sig;
}

LibProc::~LibProc()
{}

/*==============================================================================
 * FUNCTION:		LibProc::put
 * OVERVIEW:		Display on os.
 * PARAMETERS:		os -
 * RETURNS:			os
 *============================================================================*/
//std::ostream& LibProc::put(std::ostream& os) {
//	os << "library procedure `" << signature->getName() << "' resides at 0x";
//	return os << std::hex << address << std::endl;
//}

Exp *LibProc::getProven(Exp *left) {
	// Just use the signature information (all we have, after all)
	return signature->getProven(left);
}

bool LibProc::isPreserved(Exp* e) {
	return signature->isPreserved(e);
}

/**********************
 * UserProc methods.
 *********************/

/*==============================================================================
 * FUNCTION:		UserProc::UserProc
 * OVERVIEW:		Constructor with name, native address.
 * PARAMETERS:		name - Name of procedure
 *					uNative - Native address of entry point of procedure
 * RETURNS:			<nothing>
 *============================================================================*/
UserProc::UserProc() : Proc(), cfg(NULL), status(PROC_UNDECODED), maxDepth(-1),
		// decoded(false), analysed(false),
		nextLocal(0),	// decompileSeen(false), decompiled(false), isRecursive(false)
		theReturnStatement(NULL) {
}
UserProc::UserProc(Prog *prog, std::string& name, ADDRESS uNative) :
		// Not quite ready for the below fix:
		// Proc(prog, uNative, prog->getDefaultSignature(name.c_str())),
		Proc(prog, uNative, new Signature(name.c_str())),
		cfg(new Cfg()), status(PROC_UNDECODED), maxDepth(-1),
		// decoded(false), analysed(false),
		nextLocal(0), // decompileSeen(false), decompiled(false), isRecursive(false),
		theReturnStatement(NULL) {
	cfg->setProc(this);				 // Initialise cfg.myProc
}

UserProc::~UserProc() {
	if (cfg)
		delete cfg; 
}

/*==============================================================================
 * FUNCTION:		UserProc::deleteCFG
 * OVERVIEW:		Deletes the whole CFG for this proc object. Also clears the
 *					cfg pointer, to prevent strange errors after this is called
 * PARAMETERS:		<none>
 * RETURNS:			<nothing>
 *============================================================================*/
void UserProc::deleteCFG() {
	delete cfg;
	cfg = NULL;
}

class lessEvaluate : public std::binary_function<SyntaxNode*, SyntaxNode*, bool> {
public:
	bool operator()(const SyntaxNode* x, const SyntaxNode* y) const
	{
		return ((SyntaxNode*)x)->getScore() > ((SyntaxNode*)y)->getScore();
	}
};

SyntaxNode *UserProc::getAST()
{
	int numBBs = 0;
	BlockSyntaxNode *init = new BlockSyntaxNode();
	BB_IT it;
	for (PBB bb = cfg->getFirstBB(it); bb; bb = cfg->getNextBB(it)) {
		BlockSyntaxNode *b = new BlockSyntaxNode();
		b->setBB(bb);
		init->addStatement(b);
		numBBs++;
	}
	
	// perform a best first search for the nicest AST
	std::priority_queue<SyntaxNode*, std::vector<SyntaxNode*>, lessEvaluate > ASTs;
	ASTs.push(init);

	SyntaxNode *best = init;
	int best_score = init->getScore();
	int count = 0;
	while (ASTs.size()) {
		if (best_score < numBBs * 2) {
			LOG << "exit early: " << best_score << "\n";
			break;
		}

		SyntaxNode *top = ASTs.top();
		ASTs.pop();
		int score = top->evaluate(top);

		printAST(top); // debug

		if (score < best_score) {
			if (best && top != best)
				delete best;
			best = top;
			best_score = score;
		}

		count++;
		if (count > 100)
			break;

		// add successors
		std::vector<SyntaxNode*> successors;
		top->addSuccessors(top, successors);
		for (unsigned i = 0; i < successors.size(); i++) {
			//successors[i]->addToScore(top->getScore());	// uncomment for A*
			successors[i]->addToScore(successors[i]->getDepth()); // or this
			ASTs.push(successors[i]);
		}

		if (top != best)
			delete top;
	}

	// clean up memory
	while(ASTs.size()) {
		SyntaxNode *top = ASTs.top();
		ASTs.pop();
		if (top != best)
			delete top;
	}
	
	return best;
}

int count = 1;

void UserProc::printAST(SyntaxNode *a)
{
	char s[1024];
	if (a == NULL)
		a = getAST();
	sprintf(s, "ast%i-%s.dot", count++, getName());
	std::ofstream of(s);
	of << "digraph " << getName() << " {" << std::endl;
	of << "	 label=\"score: " << a->evaluate(a) << "\";" << std::endl;
	a->printAST(a, of);
	of << "}" << std::endl;
	of.close();
}

/*==============================================================================
 * FUNCTION:		UserProc::setDecoded
 * OVERVIEW:		
 * PARAMETERS:		
 * RETURNS:			
 *============================================================================*/
void UserProc::setDecoded() {
	status = PROC_DECODED;
	printDecodedXML();
}

/*==============================================================================
 * FUNCTION:		UserProc::unDecode
 * OVERVIEW:		
 * PARAMETERS:		
 * RETURNS:			
 *============================================================================*/
void UserProc::unDecode() {
	cfg->clear();
	status = PROC_UNDECODED;
}

/*==============================================================================
 * FUNCTION:	UserProc::getEntryBB
 * OVERVIEW:	Get the BB with the entry point address for this procedure
 * PARAMETERS:	
 * RETURNS:		Pointer to the entry point BB, or NULL if not found
 *============================================================================*/
PBB UserProc::getEntryBB() {
	return cfg->getEntryBB();
}

/*==============================================================================
 * FUNCTION:		UserProc::setEntryBB
 * OVERVIEW:		Set the entry BB for this procedure
 * PARAMETERS:		<none>
 * RETURNS:			<nothing>
 *============================================================================*/
void UserProc::setEntryBB() {
	std::list<PBB>::iterator bbit;
	PBB pBB = cfg->getFirstBB(bbit);		// Get an iterator to the first BB
	// Usually, but not always, this will be the first BB, or at least in the
	// first few
	while (pBB && address != pBB->getLowAddr()) {
		pBB = cfg->getNextBB(bbit);
	}
	cfg->setEntryBB(pBB);
}

/*==============================================================================
 * FUNCTION:		UserProc::addCallee
 * OVERVIEW:		Add this callee to the set of callees for this proc
 * PARAMETERS:		A pointer to the Proc object for the callee
 * RETURNS:			<nothing>
 *============================================================================*/
void UserProc::addCallee(Proc* callee) {
    // is it already in? (this is much slower than using a set)
    std::list<Proc*>::iterator cc;
	for (cc = calleeList.begin(); cc != calleeList.end(); cc++)
		if(*cc == callee)
            return; // it's already in

	calleeList.push_back(callee);
}

void UserProc::generateCode(HLLCode *hll) {
	assert(cfg);
	assert(getEntryBB());

	cfg->structure();
	replaceExpressionsWithGlobals();
	if (!Boomerang::get()->noLocals) {
//		  while (nameStackLocations())
//			  replaceExpressionsWithSymbols();

#if 0		// In back end now...
			while (nameRegisters())
				replaceExpressionsWithSymbols();
#else
			nameRegisters();
#endif
	}
	removeUnusedLocals();

	// Note: don't try to remove unused statements here; that requires the
	// RefExps, which are all gone now (transformed out of SSA form)!

	if (VERBOSE || Boomerang::get()->printRtl)
		printToLog();

	hll->AddProcStart(this);
	
	// Local variables; print everything in the locals map
	std::map<std::string, Type*>::iterator last = locals.end();
	if (locals.size()) last--;
	for (std::map<std::string, Type*>::iterator it = locals.begin(); it != locals.end(); it++) {
		Type* locType = it->second;
		if (locType == NULL || locType->isVoid())
			locType = new IntegerType();
		hll->AddLocal(it->first.c_str(), locType, it == last);
	}

	std::list<PBB> followSet, gotoSet;
	getEntryBB()->generateCode(hll, 1, NULL, followSet, gotoSet);
	
	hll->AddProcEnd();

	if (!Boomerang::get()->noRemoveLabels)
		cfg->removeUnneededLabels(hll);
}

// print this userproc, maining for debugging
void UserProc::print(std::ostream &out) {
	signature->print(out);
	printParams(out);
	cfg->print(out);
	out << "\n";
}

void UserProc::printParams(std::ostream& out) {
	out << "Parameters: ";
	bool first = true;
	for (StatementList::iterator pp = parameters.begin(); pp != parameters.end(); ++pp) {
		if (first)
			first = false;
		else
			out << ", ";
		out << ((Assign*)*pp)->getLeft();
	}
	out << "\n";
}

char* UserProc::prints() {
	std::ostringstream ost;
	print(ost);
	strncpy(debug_buffer, ost.str().c_str(), DEBUG_BUFSIZE);
	debug_buffer[DEBUG_BUFSIZE] = '\0';
	return debug_buffer;
}

void UserProc::dump() {
	print(std::cerr);
}

void UserProc::printToLog() {
	signature->printToLog();
	std::ostringstream ost;
	printParams(ost);
	LOG << ost.str().c_str();
	symbolMapToLog();
	for (std::map<std::string, Type*>::iterator it = locals.begin(); it != locals.end(); it++) {
		LOG << it->second->getCtype() << " " << it->first.c_str() << " ";
		Exp *e = expFromSymbol((*it).first.c_str());
		// Beware: for some locals, expFromSymbol() returns NULL (? No longer?)
		if (e)
			LOG << e << "\n";
		else
			LOG << "-\n";
	}
	LOG << "Live variables: ";
	std::ostringstream ost2;
	col.print(ost2);
	LOG << ost2.str().c_str() << "\n";
	cfg->printToLog();
	LOG << "\n";
}

// initialise all statements
void UserProc::initStatements() {
	BB_IT it;
	BasicBlock::rtlit rit; StatementList::iterator sit;
	for (PBB bb = cfg->getFirstBB(it); bb; bb = cfg->getNextBB(it)) {
		for (Statement* s = bb->getFirstStmt(rit, sit); s; s = bb->getNextStmt(rit, sit)) {
			s->setProc(this);
			s->setBB(bb);
			CallStatement* call = dynamic_cast<CallStatement*>(s);
			if (call) {
				call->setSigArguments();
			}
#if 0
			ReturnStatement *ret = dynamic_cast<ReturnStatement*>(s);
			if (ret)
				ret->setSigArguments();
#endif
		}
	}
}

void UserProc::numberStatements(int& stmtNum) {
	BB_IT it;
	BasicBlock::rtlit rit; StatementList::iterator sit;
	for (PBB bb = cfg->getFirstBB(it); bb; bb = cfg->getNextBB(it)) {
		for (Statement* s = bb->getFirstStmt(rit, sit); s; s = bb->getNextStmt(rit, sit))
			if (!s->isImplicit())		// Don't renumber implicits (remain number 0)
				s->setNumber(++stmtNum);
	}
}

void UserProc::numberPhiStatements(int& stmtNum) {
	BB_IT it;
	BasicBlock::rtlit rit; StatementList::iterator sit;
	for (PBB bb = cfg->getFirstBB(it); bb; bb = cfg->getNextBB(it)) {
		for (Statement* s = bb->getFirstStmt(rit, sit); s; s = bb->getNextStmt(rit, sit))
			if (s->isPhi() && s->getNumber() == 0)
				s->setNumber(++stmtNum);
	}
}


// get all statements
// Get to a statement list, so they come out in a reasonable and consistent order
void UserProc::getStatements(StatementList &stmts) {
	BB_IT it;
	for (PBB bb = cfg->getFirstBB(it); bb; bb = cfg->getNextBB(it)) {
		std::list<RTL*> *rtls = bb->getRTLs();
		if (rtls) {
			for (std::list<RTL*>::iterator rit = rtls->begin(); rit != rtls->end(); rit++) {
				RTL *rtl = *rit;
				for (std::list<Statement*>::iterator it = rtl->getList().begin(); it != rtl->getList().end(); it++) {
					if ((*it)->getBB() == NULL)
						(*it)->setBB(bb);
					if ((*it)->getProc() == NULL)
						(*it)->setProc(this);
					stmts.append(*it);
				}
			}
		}
	}
}

// Remove a statement. This is somewhat inefficient - we have to search the whole BB for the statement.
// Should use iterators or other context to find out how to erase "in place" (without having to linearly search)
void UserProc::removeStatement(Statement *stmt) {
	// remove anything proven about this statement
	for (std::set<Exp*, lessExpStar>::iterator it = proven.begin(); it != proven.end(); it++) {
		LocationSet refs;
		(*it)->addUsedLocs(refs);
		LocationSet::iterator rr;
		bool usesIt = false;
		for (rr = refs.begin(); rr != refs.end(); rr++) {
			Exp* r = *rr;
			if (r->isSubscript() && ((RefExp*)r)->getDef() == stmt) {
				usesIt = true;
				break;
			}
		}
		if (usesIt) {
			if (VERBOSE)
				LOG << "removing proven exp " << (*it) << " that uses statement being removed.\n";
			proven.erase(it);
			// it = proven.begin();
			continue;
		}
	}

	// remove from BB/RTL
	PBB bb = stmt->getBB();			// Get our enclosing BB
	std::list<RTL*> *rtls = bb->getRTLs();
	for (std::list<RTL*>::iterator rit = rtls->begin(); rit != rtls->end(); rit++) {
		std::list<Statement*>& stmts = (*rit)->getList();
		for (std::list<Statement*>::iterator it = stmts.begin(); it != stmts.end(); it++) {
			if (*it == stmt) {
				stmts.erase(it);
				return;
			}
		}
	}
}

#if 1
void UserProc::insertAssignAfter(Statement* s, Exp* left, Exp* right) {
	std::list<Statement*>::iterator it;
	std::list<Statement*>* stmts;
	if (s == NULL) {
		// This means right is supposed to be a parameter. We can insert the assignment at the start of the entryBB
		PBB entryBB = cfg->getEntryBB();
		std::list<RTL*> *rtls = entryBB->getRTLs();
		assert(rtls->size());		// Entry BB should have at least 1 RTL
		stmts = &rtls->front()->getList();
		it = stmts->begin();
	} else {
		// An ordinary definition; put the assignment at the end of s's BB
		PBB bb = s->getBB();		 // Get the enclosing BB for s
		std::list<RTL*> *rtls = bb->getRTLs();
		assert(rtls->size());		// If s is defined here, there should be
									// at least 1 RTL
		stmts = &rtls->back()->getList();
		it = stmts->end();			// Insert before the end
	}
	Assign* as = new Assign( left, right);
	stmts->insert(it, as);
	return;
}
#endif

void UserProc::insertStatementAfter(Statement* s, Statement* a) {
	// Note: this procedure is designed for the front end, where enclosing BBs are not set up yet
	// So this is an inefficient linear search!
	BB_IT bb;
	for (bb = cfg->begin(); bb != cfg->end(); bb++) {
		std::list<RTL*>::iterator rr;
		std::list<RTL*>* rtls = (*bb)->getRTLs();
		for (rr = rtls->begin(); rr != rtls->end(); rr++) {
			std::list<Statement*>& stmts = (*rr)->getList();
			std::list<Statement*>::iterator ss;
			for (ss = stmts.begin(); ss != stmts.end(); ss++) {
				if (*ss == s) {
					ss++;		// This is the point to insert before
					stmts.insert(ss, a);
					return;
				}
			}
		}
	}
	assert(false);			// Should have found this statement in this BB
}

// Decompile this UserProc
std::set<UserProc*>* UserProc::decompile() {
	// Initial decompile: transform to SSA, propagation, initial returns and parameters
	std::set<UserProc*>* ret = initialDecompile();
	if (ret == NULL)
		return NULL;

	// For recursion, need much other processing here

	// Final decompile
	finalDecompile();

	return ret;
}

std::set<UserProc*>* UserProc::initialDecompile() {
	// Prevent infinite loops when there are cycles in the call graph
	if (status >= PROC_FINAL) return NULL;
	if (status < PROC_DECODED) return NULL;

	// Sort by address, so printouts make sense
	cfg->sortByAddress();

	// Initialise statements
	initStatements();

	// We have visited this proc "on the way down"
	status = PROC_VISITED;



	/*	*	*	*	*	*	*	*	*	*	*	*
	 *											*
	 *	R e c u r s e   t o   c h i l d r e n	*
	 *											*
	 *	*	*	*	*	*	*	*	*	*	*	*/

	std::set<UserProc*>* cycleSet = new std::set<UserProc*>;
	if (!Boomerang::get()->noDecodeChildren) {
		// Recurse to children first, to perform a depth first search
		BB_IT it;
		// Look at each call, to do the DFS
		for (PBB bb = cfg->getFirstBB(it); bb; bb = cfg->getNextBB(it)) {
			if (bb->getType() == CALL) {
				// The call Statement will be in the last RTL in this BB
				CallStatement* call = (CallStatement*)bb->getRTLs()->back()->getHlStmt();
				UserProc* destProc = (UserProc*)call->getDestProc();
				if (destProc->isLib()) continue;
				if (destProc->status >= PROC_VISITED && destProc->status < PROC_FINAL)
					// We have discovered a cycle in the call graph
					cycleSet->insert(destProc);
					// Don't recurse into the loop
				else {
					// Recurse to this child (in the call graph)
					std::set<UserProc*>* childSet = destProc->decompile();
					call->setCalleeReturn(destProc->getTheReturnStatement());
					// Union this child's set into cycleSet
					if (childSet)
						cycleSet->insert(childSet->begin(), childSet->end());
				}
			}
		}

		// isRecursive = cycleSet->size() != 0;
		// Remove self from the cycle list
		cycleSet->erase(this);
	}

	Boomerang::get()->alert_start_decompile(this);
	if (VERBOSE) LOG << "decompiling: " << getName() << "\n";

	/*	*	*	*	*	*	*	*	*	*	*	*
	 *											*
	 *		D e c o m p i l e   p r o p e r		*
	 *											*
	 *	*	*	*	*	*	*	*	*	*	*	*/


	// Compute dominance frontier
	df.dominators(cfg);

	// Number the statements
	int stmtNumber = 0;
	numberStatements(stmtNumber); 

	printXML();

	if (Boomerang::get()->noDecompile) {
		status = PROC_FINAL;				// ??!
		return cycleSet;
	}

	if (VERBOSE) {
		LOG << "--- Debug Initial Print after decoding " << getName() << " ---\n";
		printToLog();
		LOG << "=== End Initial Debug Print after decoding " << getName() << " ===\n\n";
	}

	// Update the defines in the calls
	updateCallDefines();

	// For each memory depth (1). First path is just to do the initial propagation (especially for the stack pointer),
	// place phi functions, number statements, initial propagation
	maxDepth = findMaxDepth() + 1;
	if (Boomerang::get()->maxMemDepth < maxDepth)
		maxDepth = Boomerang::get()->maxMemDepth;
	int depth;
	for (depth = 0; depth <= maxDepth; depth++) {

		if (VERBOSE)
			LOG << "placing phi functions at depth " << depth << "\n";
		// Place the phi functions for this memory depth
		df.placePhiFunctions(depth, this);

		if (VERBOSE)
			LOG << "numbering phi statements at depth " << depth << "\n";
		// Number them
		numberPhiStatements(stmtNumber);

		if (VERBOSE)
			LOG << "renaming block variables (1) at depth " << depth << "\n";
		// Rename variables
		df.renameBlockVars(this, 0, depth);
		if (VERBOSE) {
			LOG << "\n--- After rename (1) for " << getName() << " at depth " << depth << ": ---\n";
			printToLog();
			LOG << "\n=== Done after rename (1) for " << getName() << " at depth " << depth << ": ===\n\n";
		}

		propagateStatements(depth, -1);
		if (VERBOSE) {
			LOG << "\n--- After propagation (1) for " << getName() << " at depth " << depth << ": ---\n";
			printToLog();
			LOG << "\n=== Done after propagation (1) for " << getName() << " at depth " << depth << ": ===\n\n";
		}

		// The call bypass logic should be staged as well. For example, consider m[r1{11}]{11} where 11 is a call.
		// The first stage bypass yields m[r1{2}]{11}, which needs another round of propagation to yield m[r1{-}-32]{11}
		// (which can safely be processed at depth 1).
		fixCallBypass();
		propagateStatements(depth, -1);
		if (VERBOSE) {
			LOG << "\n--- After fixCallBypass (1) of " << getName() << " at depth " << depth << ": ---\n";
			printToLog();
			LOG << "\n=== Done after fixCallBypass (1) of " << getName() << " at depth " << depth << ": ===\n\n";
		}
	}

//	trimReturns();		// Also does proofs (preserveds)
						// FIXME: Are there any returns to trim yet? Needs a rename
	
	if (!Boomerang::get()->noPromote)
		// We want functions other than main to be promoted
		promoteSignature();

	// Update the arguments for calls
	updateArguments();

	// For each memory depth (2)
	for (depth = 0; depth <= maxDepth; depth++) {
		// Redo the renaming process to take into account the arguments
		if (VERBOSE)
			LOG << "Renaming block variables (2) at depth " << depth << "\n";
		// Rename variables
		df.renameBlockVars(this, 0, depth);						// E.g. for new arguments

		// Seed the return statement with reaching definitions
		if (theReturnStatement)
			theReturnStatement->copyReachingDefs(depth);		// Everything including new arguments reaching ret

		// This gets called so much... it would be great to be able to "repair" the names...
		df.renameBlockVars(this, 0, depth);						// E.g. update call use collectors with return uses

		printXML();

		// Print if requested
		if (VERBOSE) {		// was if debugPrintSSA
			LOG << "--- Debug Print SSA for " << getName() << " at depth " << depth << " (no propagations) ---\n";
			printToLog();
			LOG << "=== End Debug Print SSA for " << getName() << " at depth " << depth << " ===\n\n";
		}
		
		Boomerang::get()->alert_decompile_SSADepth(this, depth);

		if (depth == maxDepth) {
			fixCallBypass();			// FIXME: needed?
			if (processConstants()) {
				for (int i = 0; i <= maxDepth; i++) {
					df.renameBlockVars(this, 0, i, true);	// Needed if there was an indirect call to an ellipsis
															// function
					propagateStatements(i, -1);
				}
			}
			removeRedundantPhis();
		}
		fixCallBypass();				// FIXME: needed? If so, document why
		// recognising globals early prevents them from becoming parameters
		if (depth == maxDepth)		// Else Sparc problems... MVE
			replaceExpressionsWithGlobals();
#if 0 	// No: recovering parameters must be late: after removal of unused statements, so don't get phantom parameters
		// from preserveds. Note that some preserveds could be prarameters.
		if (depth > 0 && !Boomerang::get()->noChangeSignatures) {
			addNewParameters();
			//trimParameters(depth);
		}
#endif

#if 0	// No need to do a whole propagation phase any more; addArguments does the propagation now
		// if we've added new parameters, need to do propagations up to this depth.  it's a recursive function thing.
		if (nparams != signature->getNumParams()) {
			for (int depth_tmp = 0; depth_tmp < depth; depth_tmp++) {
				// Propagate at this memory depth
				for (int td = maxDepth; td >= 0; td--) {
					if (VERBOSE)
						LOG << "parameter propagating at depth " << depth_tmp << " to depth " << td << "\n";
					propagateStatements(depth_tmp, td);
					for (int i = 0; i <= depth_tmp; i++)
						df.renameBlockVars(this, 0, i, true);
				}
			}
			df.renameBlockVars(this, 0, depth, true);
			printXML();
			if (VERBOSE) {
				LOG << "=== Debug Print SSA for " << getName() << " at memory depth " << depth
					<< " (after adding new parameters) ===\n";
				printToLog();
				LOG << "=== End Debug Print SSA for " << getName() << " at depth " << depth << " ===\n\n";
			}
		}
#endif

		// replacing expressions with Parameters as we go
		if (!Boomerang::get()->noParameterNames) {
			replaceExpressionsWithParameters(df, depth);
			df.renameBlockVars(this, 0, depth, true);
		}

		// recognising locals early prevents them from becoming returns
		// But with indirect procs in a loop, the propagation is not yet complete
		// replaceExpressionsWithLocals(depth == maxDepth);
		if (!Boomerang::get()->noChangeSignatures) {
			// addNewReturns(depth);
			df.renameBlockVars(this, 0, depth, true);
			printXML();
			if (VERBOSE) {
				LOG << "--- Debug Print SSA for " << getName() << " at memory depth " << depth <<
					" (after adding new returns) ---\n";
				printToLog();
				LOG << "=== End Debug Print SSA for " << getName() << " at depth " << depth << " ===\n\n";
			}
			trimReturns();
			updateReturnTypes();
			fixCallBypass();
		}

		printXML();
		// Print if requested
		if (VERBOSE) {		// was if debugPrintSSA
			LOG << "--- Debug Print SSA for " << getName() << " at memory depth " << depth <<
				" (after trimming return set) ---\n";
			printToLog();
			LOG << "=== End Debug Print SSA for " << getName() << " at depth " << depth << " ===\n\n";
		}

		Boomerang::get()->alert_decompile_beforePropagate(this, depth);

		// Propagate at this memory depth
		bool convert;			// True when indirect call converted to direct
		do {
			convert = false;
			for (int td = maxDepth; td >= 0; td--) {
				if (VERBOSE)
					LOG << "propagating at depth " << depth << " to depth " << td << "\n";
				convert |= propagateStatements(depth, td);
				if (convert)
					break;			// Just calling renameBlockVars now can cause problems
				for (int i = 0; i <= depth; i++)
					df.renameBlockVars(this, 0, i, true);
			}
			// If you have an indirect to direct call conversion, some propagations that were blocked by
			// the indirect call might now succeed, and may be needed to prevent alias problems
			if (VERBOSE && convert)
				LOG << "\nAbout to restart propagations and dataflow at depth " << depth <<
					" due to conversion of indirect to direct call(s)\n\n";
			if (convert) {
				depth = 0;		// Start again from depth 0
				// FIXME: is this needed?
				df.renameBlockVars(this, 0, 0, true);	 // Initial dataflow level 0
				LOG << "\nAfter rename (2) of " << getName() << ":\n";
				printToLog();
				LOG << "\nDone after rename (2) of " << getName() << ":\n\n";
			}
		} while (convert);

		printXML();
		if (VERBOSE) {
			LOG << "--- After propagate for " << getName() << " at memory depth " << depth << " ---\n";
			printToLog();
			LOG << "=== End propagate for " << getName() << " at depth " << depth << " ===\n\n";
		}

		Boomerang::get()->alert_decompile_afterPropagate(this, depth);

	}

	// Check for indirect jumps or calls not already removed by propagation of constants
	if (cfg->decodeIndirectJmp(this)) {
		// There was at least one indirect jump or call found and decoded. That means that most of what has been
		// done to this function so far is invalid. So redo everything. Very expensive!!
		LOG << "=== About to restart decompilation of " << getName() <<
			" because indirect jumps or calls have been removed\n\n";
		//assignProcsToCalls();		// Surely this should be done at decode time!
		return initialDecompile();	 // Restart decompiling this proc
	}

	// Trim returns, calculate preserveds
	trimReturns();

	// Used to be later...
	if (!Boomerang::get()->noParameterNames) {
		for (int i = maxDepth; i >= 0; i--) {
			//replaceExpressionsWithParameters(df, i);
			replaceExpressionsWithLocals(true);
			//cfg->renameBlockVars(this, 0, i, true);		// MVE: is this really needed?
		}
		trimReturns();		// FIXME: is this necessary here?
		fixCallBypass();		// FIXME: surely this is not necessary now?
		trimParameters();	// FIXME: surely there aren't any parameters to trim yet?
		if (VERBOSE) {
			LOG << "--- After replacing expressions, trimming params and returns ---\n";
			printToLog();
			LOG << "=== End after replacing expressions, trimming params and returns ===\n";
			LOG << "===== End after replacing params =====\n\n";
		}
	}

	return cycleSet;
}

void UserProc::finalDecompile() {

	// A temporary hack to remove %CF = %CF{7} when 7 isn't a SUBFLAGS
	if (theReturnStatement)
		theReturnStatement->specialProcessing();

	/*	*	*	*	*	*	*	*	*	*	*	*	*	*
	 *													*
	 *	R e m o v e   u n u s e d   s t a t e m e n t s	*
	 *													*
	 *	*	*	*	*	*	*	*	*	*	*	*	*	*/

	// Only remove unused statements after decompiling as much as possible of the proc
	for (int depth = 0; depth <= maxDepth; depth++) {
		// Remove unused statements
		RefCounter refCounts;			// The map
		// Count the references first
		countRefs(refCounts);
		// Now remove any that have no used
		if (!Boomerang::get()->noRemoveNull)
			removeUnusedStatements(refCounts, depth);

		// Remove null statements
		if (!Boomerang::get()->noRemoveNull)
			removeNullStatements();

		printXML();
		if (VERBOSE && !Boomerang::get()->noRemoveNull) {
			LOG << "--- After removing unused and null statements depth " << depth << " ---\n";
			printToLog();
			LOG << "=== End after removing unused statements ===\n\n";
		}
		Boomerang::get()->alert_decompile_afterRemoveStmts(this, depth);
	}

	addNewParameters();
	if (!Boomerang::get()->noParameterNames) {
		for (int i = maxDepth; i >= 0; i--) {
			replaceExpressionsWithParameters(df, i);
			//cfg->renameBlockVars(this, 0, i, true);		// MVE: is this really needed?
		}
		trimReturns();		// FIXME: is this necessary here?
		fixCallBypass();		// FIXME: surely this is not necessary now?
		//trimParameters();	// FIXME: check
		if (VERBOSE) {
			LOG << "--- After adding new parameters ---\n";
			printToLog();
			LOG << "=== End after adding new parameters ===\n";
		}
	}

	// Data flow based type analysis
	// Want to be after all propagation, but before converting expressions to locals etc
	if (DFA_TYPE_ANALYSIS) {
		if (VERBOSE || DEBUG_TA)
			LOG << "--- Start Data-flow-based Type Analysis for " << getName() << " ---\n";

		// Now we need to add the implicit assignments. Doing this earlier is extremely problematic, because
		// of all the m[...] that change their sorting order as their arguments get subscripted or propagated into
		addImplicitAssigns();

		bool first = true;
		do {
			if (!first)
				propagateAtDepth(df, maxDepth);		// HACK: Can sometimes be needed, if call was indirect
													// MVE: Check if still needed
			first = false;
			dfaTypeAnalysis();
		} while (ellipsisProcessing());
		if (VERBOSE || DEBUG_TA)
			LOG << "=== End Type Analysis for " << getName() << " ===\n";
	}

#if 0		// Want this earlier, before remove unused statements, so assignments to locals can work
	if (!Boomerang::get()->noParameterNames) {
		for (int i = maxDepth; i >= 0; i--) {
			replaceExpressionsWithParameters(df, i);
			replaceExpressionsWithLocals(true);
			//cfg->renameBlockVars(this, 0, i, true);		// MVE: is this really needed?
		}
		trimReturns();
		fixCallBypass();
		trimParameters();
		if (VERBOSE) {
			LOG << "=== After replacing expressions, trimming params and returns ===\n";
			printToLog();
			LOG << "=== End after replacing expressions, trimming params and returns ===\n";
			LOG << "===== End after replacing params =====\n\n";
		}
	}
#endif

	processConstants();
	//sortParameters();

//	if (DFA_TYPE_ANALYSIS)
		// This has to be done fairly late, e.g. after trimming returns. This seems to be something to do with
		// call statements not defining local variables properly. It is now delayed until code generation.
//		replaceRegistersWithLocals();

	printXML();

	status = PROC_FINAL;		// Now fully decompiled (apart from one final pass, and transforming out of SSA form)
	Boomerang::get()->alert_end_decompile(this);
}

void UserProc::updateBlockVars(DataFlow& df)
{
	int depth = findMaxDepth() + 1;
	for (int i = 0; i <= depth; i++)
		df.renameBlockVars(this, 0, i, true);
}

void UserProc::propagateAtDepth(DataFlow& df, int depth)
{
	propagateStatements(depth, -1);
	for (int i = 0; i <= depth; i++)
		df.renameBlockVars(this, 0, i, true);
}

int UserProc::findMaxDepth() {
	StatementList stmts;
	getStatements(stmts);
	int maxDepth = 0;
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		// Assume only need to check assignments
		if (s->getKind() == STMT_ASSIGN) {
			int depth = ((Assign*)s)->getMemDepth();
			if (depth > maxDepth) maxDepth = depth;
		}
	}
	return maxDepth;
}

void UserProc::removeRedundantPhis() {
	if (VERBOSE || DEBUG_UNUSED_STMT)
		LOG << "removing redundant phi statements" << "\n";

	// some phis are just not used
	RefCounter refCounts;
	countRefs(refCounts);

	StatementList stmts;
	getStatements(stmts);
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		if (s->isPhi()) {
			bool unused = false;
			PhiAssign *pa = (PhiAssign*)s;
			if (refCounts[s] == 0)
				unused = true;
			else if (refCounts[s] == 1) {
				/* This looks pretty good, if all the statements in a phi are either NULL or a call to this proc, then 
				 * the phi is redundant.  However, we only remove it if the only use is in the return statement.
				 */
				RefExp *r = new RefExp(pa->getLeft()->clone(), s);
				bool usedInRet = false;
				if (theReturnStatement)
					usedInRet = theReturnStatement->usesExp(r);
				delete r;
				if (usedInRet) {
					bool allZeroOrSelfCall = true;
					PhiAssign::iterator it1;
					for (it1 = pa->begin(); it1 != pa->end(); it1++) {
						Statement* s1 = it1->def;
						if (s1 && (!s1->isCall() || ((CallStatement*)s1)->getDestProc() != this))
							allZeroOrSelfCall = false;
					}
					if (allZeroOrSelfCall) {
						if (VERBOSE || DEBUG_UNUSED_STMT)
							LOG << "removing phi using shakey hack:\n";
						unused = true;
						removeReturn(pa->getLeft());
					}
				}
			} else {
#if 0		// NO! This essentially knocks out ALL the phis (I think). MVE
			// More thought required.
				// Check to see if all the refs are to either the same thing
				// or to NULL. If so, they will (would have been?) removed in
				// the fromSSA code. Removing them here allows some pesky
				// statements (e.g. assignments to %pc) to be removed
				LocationSet refs;
				p->addUsedLocs(refs);
				Exp* first = *refs.begin();
				bool same = true;
				LocationSet::iterator rr;
				for (rr = refs.begin(); rr != refs.end(); rr++) {
					if (!(**rr *= *first)) {	   // Ref-insensitive compare
						same = false;
						break;
					}
				}
				if (same) {
					// Is the left of the phi assignment the same base variable
					// as all the operands (or the operand is NULL) ?
					if (*s->getLeft() *= *first) {
						if (DEBUG_UNUSED_STMT)
							LOG << "Removing phi: left and all refs same or 0: " << s << "\n";
						// Just removing the refs will work, or removing the whole phi
						removeStatement(s);
					}
				}
#endif
			}
			if (unused) {
				if (DEBUG_UNUSED_STMT)
					LOG << "removing redundant phi " << s << "\n";
				removeStatement(s);
			}
		}
	}

#if 0
	stmts.clear();
	getStatements(stmts);

	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = (Statement*)*it;
		if (s->isPhi()) {
			if (VERBOSE) {
				LOG << "checking " << s << "\n";
			}
			// if we can prove that all the statements in the phi define equal values then we can replace the phi
			// with any one of the values, but there's not much point if they're all calls
			PhiExp *p = (PhiExp*)s->getRight();
			bool hasphi = false;
			StatementVec::iterator it;
			for (it = p->begin(); it != p->end(); it++)
				if (*it && (*it)->isPhi()) {
					hasphi = true;
				}
			if (hasphi) {
				if (VERBOSE)
					LOG << "contains a ref to a phi statement (skipping)\n";
				// Do we need this?
				//continue;
			}
			bool allsame = true;
			it = p->begin();
			assert(it != p->end());
			Statement* s1 = *it;
			Statement* noncall = s1;
			if (it != p->end())
				for (it++; it != p->end(); it++) {
					Statement* s2 = *it;
					if (noncall && s2 && 
						(noncall->isCall()
								|| s2->getNumber() < noncall->getNumber()) && 
						!s2->isCall() && s2 != s)
						noncall = s2;
					Exp *e = new Binary(opEquals, 
								 new RefExp(s->getLeft()->clone(), s1),
								 new RefExp(s->getLeft()->clone(), s2));
					if (!prove(e)) {
						allsame = false; break;
					}
				}
			if (allsame && (noncall == NULL || !noncall->isCall())) {
				s->searchAndReplace(s->getRight(), 
				   new RefExp(s->getLeft(), noncall));
			}
		}
	}
#endif
}

void UserProc::trimReturns() {
	std::set<Exp*> removes;
	bool stdsp = false;
	bool stdret = false;

	if (VERBOSE)
		LOG << "Trimming return set for " << getName() << "\n";

	// Note: need this non-virtual version most of the time, since nothing proved yet
	int sp = signature->getStackRegister(prog);

	for (int n = 0; n < 2; n++) {
		// may need to do multiple times due to dependencies FIXME: efficiency!

		// Special case for 32-bit stack-based machines (e.g. Pentium).
		// RISC machines generally preserve the stack pointer (so no special case required)
		for (int p = 0; !stdsp && p < 8; p++) {
			if (DEBUG_PROOF)
				LOG << "attempting to prove sp = sp + " << p*4 << " for " << getName() << "\n";
			stdsp = prove(new Binary(opEquals,
						Location::regOf(sp),
						new Binary(opPlus,
							Location::regOf(sp),
							new Const(p * 4))));
		}

		// Prove that pc is set to the return value
		if (DEBUG_PROOF)
			LOG << "attempting to prove %pc = m[sp]\n";
		stdret = prove(new Binary(opEquals, new Terminal(opPC), Location::memOf(Location::regOf(sp))));

#if 0
		// prove preservation for each return
		for (unsigned u = 0; u < signature->getNumReturns(); u++) {
			Exp *p = signature->getReturnExp(u);
			Exp *e = new Binary(opEquals, p->clone(), p->clone());
			if (DEBUG_PROOF)
				LOG << "attempting to prove " << p << " is preserved by " << getName() << "\n";
			if (prove(e)) {
				removes.insert(p);	
			}
		}
#else
		// prove preservation for all definitions reaching the exit
		DefCollector* defCol = theReturnStatement->getCollector();
		Collector::iterator rd;
		for (rd = defCol->begin(); rd != defCol->end(); ++rd) {
			Exp* base = ((RefExp*)*rd)->getSubExp1();
			Exp* equation = new Binary(opEquals, base, base);
			if (DEBUG_PROOF)
				LOG << "attempting to prove " << equation << " is preserved by " << getName() << "\n";
			if (prove(equation)) {
				removes.insert(equation);	
			}
		}
#endif
	}

	if (stdsp) {
		// I've been removing sp from the return set as it makes the output look better, but this only works for
		// recursive procs (because no other proc calls them and fixCallBypass can replace refs to the call with a valid
		// expression). Not removing sp will make basically every procedure that doesn't preserve sp return it, and take
		// it as a parameter.  Maybe a later pass can get rid of this.	Trent 22/8/2003
		// We handle this now by doing a final pass which removes any unused returns of a procedure.  So r28 will remain
		// in the returns set of every procedure in the program until such time as this final pass is made, and then
		// they will be removed.	 Trent 22/9/2003
		// Instead, what we can do is replace r28 in the return statement of this procedure with what we've proven it to
		// be.
		//removeReturn(regsp);
		// also check for any locals that slipped into the returns
#if 0
		Unary *regsp = Location::regOf(sp);
		for (int i = 0; i < signature->getNumReturns(); i++) {
			Exp *e = signature->getReturnExp(i);
			if (signature->isStackLocal(prog, e))
				removes.insert(e);
			else if (*e == *regsp) {
				assert(theReturnStatement);
				Exp *e = getProven(regsp)->clone();
				// Make sure that the regsp in this expression is subscripted with a proper implicit assignment.
				// Note that trimReturns() is sometimes called before and after implicit assignments are created,
				// hence call findTHEimplicitAssign()
				// NOTE: This assumes simple functions of regsp, e.g. regsp + K, not involving other locations that
				// need to be subscripted
				e = e->clone();			// So don't subscript the proven equation in the Proc
				e = e->expSubscriptVar(regsp, cfg->findTheImplicitAssign(regsp));

				if (!(*e == *theReturnStatement->getReturnExp(i))) {
					if (VERBOSE)
						LOG << "replacing in return statement " << theReturnStatement->getReturnExp(i) << " with " <<
							e << "\n";
					theReturnStatement->setReturnExp(i, e);
				}
			}
		}
#else	// Assume that sp doesn't need special processing, so just remove any locals
		if (theReturnStatement) {
			ReturnStatement::iterator rr;
			for (rr = theReturnStatement->begin(); rr != theReturnStatement->end(); ++rr) {
				Exp* loc = ((Assignment*)*rr)->getLeft();
				if (signature->isStackLocal(prog, loc)) {
					if (VERBOSE)
						LOG << "Removing local " << loc << " from theReturnStatement\n";
					theReturnStatement->removeReturn(loc);
				}
			}
		}
#endif
	}
	if (!signature->isFullSignature()) {
		if (stdret)
			removeReturn(new Terminal(opPC));
		for (std::set<Exp*>::iterator it = removes.begin(); it != removes.end(); it++) {
			// removes are of the form r29{9} = r29{-}
			removeReturn(((Binary*)*it)->getSubExp1());
		}
	}

	if (DEBUG_PROOF) {
		LOG << "Proven for procedure " << getName() << ":\n";
		for (std::set<Exp*, lessExpStar>::iterator it = proven.begin(); it != proven.end(); it++)
			LOG << *it << "\n";
		LOG << "End proven for procedure " << getName() << "\n\n";
	}

	removeRedundantPhis();
}

void UserProc::updateReturnTypes()
{
	if (theReturnStatement == NULL)
		return;
	ReturnStatement::iterator rr;
	for (rr = theReturnStatement->begin(); rr != theReturnStatement->end(); ++rr) {
		Exp *e = ((Assignment*)*rr)->getLeft();
		Type *ty = e->getType();
		if (ty && !ty->isVoid()) {
			// UGH! Remove when ad hoc TA is removed!
			int n = signature->getNumReturns();
			for (int i=0; i < n; i++) {
				if (*signature->getReturnExp(i) == *e) {
					signature->setReturnType(n, ty->clone());
					break;
				}
			}
		}
	}
}

// FIXME: Consider moving this functionality inside the propagation logic (propagateTo()).
void UserProc::fixCallBypass()
{
	if (VERBOSE)
		LOG << "\nfixCallBypass for " << getName() << "\n";
	StatementList stmts;
	getStatements(stmts);
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		s->fixCallBypass();
	}
	simplify();
	if (VERBOSE)
		LOG << "end fixCallBypass for " << getName() << "\n\n";
}

/*
 * Find the procs the calls point to.
 * To be called after decoding all procs.
 * was in: analyis.cpp
 */
void UserProc::assignProcsToCalls()
{
	std::list<PBB>::iterator it;
	PBB pBB = cfg->getFirstBB(it);
	while (pBB)
	{
    	std::list<RTL*>* rtls = pBB->getRTLs();
    	if (rtls == NULL)
    		continue;
    	for (std::list<RTL*>::iterator it2 = rtls->begin(); it2 != rtls->end(); it2++) {
    		if (!(*it2)->isCall()) continue;
    		CallStatement* call = (CallStatement*)(*it2)->getList().back();
    		if (call->getDestProc() == NULL && !call->isComputed()) {
    			Proc *p = prog->findProc(call->getFixedDest());
    			if (p == NULL) {
    				std::cerr << "Cannot find proc for dest " << call->getFixedDest() << " in call at "
    					<< (*it2)->getAddress() << "\n";
    				assert(p);
    			}
    			call->setDestProc(p);
    		}
    		call->setSigArguments();
    	}

		pBB = cfg->getNextBB(it);
	}
}

/*
 * Perform final simplifications
 * was in: analyis.cpp
 */
void UserProc::finalSimplify()
{
	std::list<PBB>::iterator it;
	PBB pBB = cfg->getFirstBB(it);
	while (pBB)
	{
    	std::list<RTL*>* pRtls = pBB->getRTLs();
    	if (pRtls == NULL)
    		continue;
    	std::list<RTL*>::iterator rit;
    	for (rit = pRtls->begin(); rit != pRtls->end(); rit++) {
    		for (int i=0; i < (*rit)->getNumStmt(); i++) {
    			Statement* rt = (*rit)->elementAt(i);
    			rt->simplifyAddr();
    			// Also simplify everything; in particular, stack offsets are
    			// often negative, so we at least canonicalise [esp + -8] to [esp-8]
    			rt->simplify();
    		}
    	}
		pBB = cfg->getNextBB(it);
	}
}

#if 0			// Now done by dataflow
void UserProc::addNewReturns(int depth) {

	if (signature->isFullSignature())
		return;

	if (VERBOSE)
		LOG << "Adding new returns for " << getName() << "\n";

	StatementList stmts;
	getStatements(stmts);

	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = (Statement*)*it;
		LocationSet defs;
		s->getDefinitions(defs);
		LocationSet::iterator ll;
		for (ll = defs.begin(); ll != defs.end(); ll++) {
			Exp *left = *ll;
			bool allZero = true;
			Exp *e = left->clone()->removeSubscripts(allZero);
			if (allZero && signature->findReturn(e) == -1 &&
				getProven(e) == NULL) {
				if (e->getOper() == opLocal) {
					if (VERBOSE)
						LOG << "ignoring local " << e << "\n";
					continue;
				}
				if (e->getOper() == opGlobal) {
					if (VERBOSE)
						LOG << "ignoring global " << e << "\n";
					continue;
				}
				if (e->getOper() == opRegOf && 
					e->getSubExp1()->getOper() == opTemp) {
					if (VERBOSE)
						LOG << "ignoring temp " << e << "\n";
					continue;
				}
				if (e->getOper() == opFlags) {
					if (VERBOSE)
						LOG << "ignoring flags " << e << "\n";
					continue;
				}
				if (e->getMemDepth() != depth) {
					continue;
				}
				if (VERBOSE)
					LOG << "Found new return " << e << "\n";
				addReturn(e);
			}
		}
	}
}
#endif

// m[WILD]{-}
static RefExp *memOfWild = new RefExp(
	Location::memOf(new Terminal(opWild)), NULL);
// r[WILD INT]{-}
static RefExp* regOfWild = new RefExp(
	Location::regOf(new Terminal(opWildIntConst)), NULL);

// Search for expressions without explicit definitions (i.e. WILDCARD{-}), which represent parameters (use before define).
// Note: this identifies saved and restored locations as parameters (e.g. ebp in most Pentium procedures).
// Would like to avoid this if possible. Marking the statements involved in saving and restoring, and ignoring these
// here, does not work in all cases, e.g. this contrived procedure:
// f1: a = a xor b
//	c = a         // Keep a copy of a xor b
//	b = a xor b
//	a = a xor b
//	print a       // Print original value of b
//	print c       // Print (original a) xor (original b)
//	a = ...
//	a = a xor b
//	b = a xor b
//	a = a xor b
//	ret

void UserProc::addNewParameters() {

	if (signature->isFullSignature())
		return;

	if (VERBOSE)
		LOG << "Adding new parameters for " << getName() << "\n";

	StatementList stmts;
	getStatements(stmts);

	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		// For now, assume that all parameters will be m[]{-} or r[]{-} (Seems pretty reasonable)
		std::list<Exp*> results;
		s->searchAll(memOfWild, results);
		s->searchAll(regOfWild, results);
		while (results.size()) {
			bool allZero;
			Exp *e = results.front()->clone()->removeSubscripts(allZero);
			results.erase(results.begin());		// Remove current (=first) result from list
			if (allZero && signature->findParam(e) == -1
					// ? Often need to transfer from implit to explicit:
				 	// && signature->findImplicitParam(e) == -1
					) {
				if (signature->isStackLocal(prog, e) || e->getOper() == opLocal) {
					if (VERBOSE)
						LOG << "ignoring local " << e << "\n";
					continue;
				}
				if (e->getOper() == opGlobal) {
					if (VERBOSE)
						LOG << "ignoring global " << e << "\n";
					continue;
				}
				if (e->getMemDepth() > 1) {
					if (VERBOSE)
						LOG << "ignoring complex " << e << "\n";
					continue;
				}
				if (e->getOper() == opMemOf && e->getSubExp1()->getOper() == opGlobal) {
					if (VERBOSE)
						LOG << "ignoring m[global] " << e << "\n";
					continue;
				}
				if (e->getOper() == opMemOf && e->getSubExp1()->getOper() == opParam) {
					if (VERBOSE)
						LOG << "ignoring m[param] " << e << "\n";
					continue;
				}
				if (e->getOper() == opMemOf &&
					e->getSubExp1()->getOper() == opPlus &&
					e->getSubExp1()->getSubExp1()->getOper() == opGlobal &&
					e->getSubExp1()->getSubExp2()->getOper() == opIntConst) {
					if (VERBOSE)
						LOG << "ignoring m[global + int] " << e << "\n";
					continue;
				}
#if 0
				// Has to be r[] or m[esp+K]. NOTE: PENTIUM SPECIFIC!
				if ((e->getOper() != opMemOf ||
						e->getSubExp1()->getOper() != opPlus ||
						!(*e->getSubExp1()->getSubExp1() == *Location::regOf(28)) ||
						e->getSubExp1()->getSubExp2()->getOper() != opIntConst) && e->getOper() != opRegOf) {
					if (VERBOSE)
						LOG << "ignoring non pentium " << e << "\n";
					continue;
				}
#endif
				if (VERBOSE)
					LOG << "Found new parameter " << e << "\n";
				// Add this parameter to the signature (for now; creates parameter names)
				addParameter(e);
				// Insert it into the parameters StatementList
				insertParameter(e);
			}
		}
	}
}

void UserProc::trimParameters(int depth) {

	if (signature->isFullSignature())
		return;

	if (VERBOSE)
		LOG << "Trimming parameters for " << getName() << "\n";

	StatementList stmts;
	getStatements(stmts);

	// find parameters that are referenced (ignore calls to this)
	int nparams = signature->getNumParams();
	// int totparams = nparams + signature->getNumImplicitParams();
int totparams = nparams;
	std::vector<Exp*> params;
	bool referenced[64];
	assert(totparams <= (int)(sizeof(referenced)/sizeof(bool)));
	int i;
	for (i = 0; i < nparams; i++) {
		referenced[i] = false;
		// Push parameters implicitly defined e.g. m[r28{0}+8]{0}, (these are the parameters for the current proc)
		params.push_back(signature->getParamExp(i)->clone()->expSubscriptAllNull());
	}
#if 0
	for (i = 0; i < signature->getNumImplicitParams(); i++) {
		referenced[i + nparams] = false;
		// Same for the implicit parameters
		params.push_back(signature->getImplicitParamExp(i)->clone()->expSubscriptAllNull());
	}
#endif

	std::set<Statement*> excluded;
	StatementList::iterator it;
#if 0		// MVE: CHECK THIS!!!!!!!!!!!!!!!!!!!!!!
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		if (s->isCall() && ((CallStatement*)s)->getDestProc() == this) {
			// A self-recursive call
			CallStatement *call = (CallStatement*)s;
			for (int i = 0; i < signature->getNumImplicitParams(); i++) {
				Exp *e = call->getImplicitArgumentExp(i);
				if (e->isSubscript()) {
					Statement *ref = ((RefExp*)e)->getDef();
					if (ref && !ref->isImplicit())
						excluded.insert(ref);
				}
			}
		}
	}
#endif
	
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		if (!s->isCall() || ((CallStatement*)s)->getDestProc() != this) {
			for (int i = 0; i < totparams; i++) {
				Exp *p, *pe;
				//if (i < nparams) {
					p = Location::param(signature->getParamName(i), this);
					pe = signature->getParamExp(i);
				//} else {
				//	p = Location::param(signature->getImplicitParamName( i - nparams), this);
				//	pe = signature->getImplicitParamExp(i - nparams);
				//}
				if (!referenced[i] && excluded.find(s) == excluded.end() && 
						// Search for the named parameter (e.g. param1), and just in case, also for the expression
						// (e.g. r8{0})
						(s->usesExp(p) || s->usesExp(params[i]))) {
					referenced[i] = true;
					if (DEBUG_UNUSED_RETS_PARAMS) {
						LOG << "Parameter " << p << " used by statement " << s->getNumber() << " : " << s->getKind() <<
							"\n";
					}
				}
				if (!referenced[i] && excluded.find(s) == excluded.end() &&
						s->isPhi() && *((PhiAssign*)s)->getLeft() == *pe) {
					if (DEBUG_UNUSED_RETS_PARAMS)
						LOG << "searching " << s << " for uses of " << params[i] << "\n";
					PhiAssign *pa = (PhiAssign*)s;
					PhiAssign::iterator it1;
					for (it1 = pa->begin(); it1 != pa->end(); it1++)
						if (it1->def == NULL) {
							referenced[i] = true;
							if (DEBUG_UNUSED_RETS_PARAMS)
								LOG << "Parameter " << p << " used by phi statement " << s->getNumber() << "\n";
							break;
						}
				}
				// delete p;
			}
		}
	}

	for (i = 0; i < totparams; i++) {
		if (!referenced[i] && (depth == -1 || params[i]->getMemDepth() == depth)) {
			bool allZero;
			Exp *e = params[i]->removeSubscripts(allZero);
			if (VERBOSE) 
				LOG << "removing unused parameter " << e << "\n";
			removeParameter(e);
		}
	}
}

void UserProc::removeReturn(Exp *e) {
	if (theReturnStatement)
		theReturnStatement->removeReturn(e);
}

void Proc::removeParameter(Exp *e) {
	int n = signature->findParam(e);
	if (n != -1) {
		signature->removeParameter(n);
		for (std::set<CallStatement*>::iterator it = callerSet.begin(); it != callerSet.end(); it++) {
			if (DEBUG_UNUSED_RETS_PARAMS)
				LOG << "removing argument " << e << " in pos " << n << " from " << *it << "\n";
			(*it)->removeArgument(n);
		}
	}
#if 0
	n = signature->findImplicitParam(e);
	if (n != -1) {
		signature->removeImplicitParameter(n);
		for (std::set<CallStatement*>::iterator it = callerSet.begin(); it != callerSet.end(); it++) {
			// Don't remove an implicit argument if it is also a return of this call.
			// Implicit arguments are needed for each return in fixCallBypass
			if (DEBUG_UNUSED_RETS_PARAMS)
				LOG << "removing implicit argument " << e << " in pos " << n << " from " << *it << "\n";
			(*it)->removeImplicitArgument(n);
		}
	}
#endif
}

void Proc::removeReturn(Exp *e)
{
	signature->removeReturn(e);
}

#if 0		// This is done by dataflow now
void Proc::addReturn(Exp *e)
{
	signature->addReturn(e);
}

void UserProc::addReturn(Exp *e)
{
	Exp *e1 = e->clone();
	if (e1->getOper() == opMemOf) {
		e1->refSubExp1() = e1->getSubExp1()->expSubscriptAllNull(/*cfg*/);
	}
	if (theReturnStatement)
		theReturnStatement->addReturn(e1);
	Proc::addReturn(e);
}
#endif

// Add the parameter to the signature. Used to say "and all known callers"; this will be handled by the recursion
// manager now
void UserProc::addParameter(Exp *e) {
	// In case it's already an implicit argument:
	removeParameter(e);

	// for (std::set<CallStatement*>::iterator it = callerSet.begin(); it != callerSet.end(); it++)
	//	(*it)->addArgument(e, this);
	signature->addParameter(e);
}

#if 0			// No, we are careful to sort parameters and returns now
void Proc::sortParameters() {
	// yes, this is a bubble sort
	for (int i = 0; i < signature->getNumParams() - 1; i++) {
		for (int j = 0; j < signature->getNumParams() - 1; j++) {
			for (int n = 0; n < signature->getNumParams() - 1; n++) {
				bool swapem = false;
				Exp *e = signature->getParamExp(n);
				Exp *f = signature->getParamExp(n + 1);
				if (e == NULL || f == NULL)
					break;
				if (e->getOper() == opMemOf && f->getOper() != opMemOf)
					swapem = true;
				else if (e->getOper() == opMemOf && f->getOper() == opMemOf) {
					if (e->getSubExp1()->getOper() == opPlus && f->getSubExp1()->getOper() == opPlus)
						if (e->getSubExp1()->getSubExp2()->isIntConst() && f->getSubExp1()->getSubExp2()->isIntConst())
							if (((Const*)e->getSubExp1()->getSubExp2())->getInt() >
									((Const*)f->getSubExp1()->getSubExp2())->getInt())
								swapem = true;
				}
				if (swapem) {
					const char *tmpname = strdup(signature->getParamName(n));
					Type *tmpty = signature->getParamType(n);
					signature->setParamName(n, signature->getParamName(n + 1));
					signature->setParamType(n, signature->getParamType(n + 1));
					signature->setParamExp(n, f);
					signature->setParamName(n + 1, tmpname);
					signature->setParamType(n + 1, tmpty);
					signature->setParamExp(n + 1, e);
					for (std::set<CallStatement*>::iterator it = callerSet.begin(); it != callerSet.end(); it++) {
						e = (*it)->getArgumentExp(n);
						(*it)->setArgumentExp(n, (*it)->getArgumentExp(n + 1));
						(*it)->setArgumentExp(n + 1, e);
					}
				}
			}
		}
	}
}
#endif

void UserProc::processFloatConstants()
{
	StatementList stmts;
	getStatements(stmts);

	Exp *match = new Ternary(opFsize,
						new Terminal(opWild), 
						new Terminal(opWild), 
						Location::memOf(new Terminal(opWild)));
	
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement *s = *it;

		std::list<Exp*> results;
		s->searchAll(match, results);
		for (std::list<Exp*>::iterator it1 = results.begin(); it1 != results.end(); it1++) {
			Ternary *fsize = (Ternary*) *it1;
			if (fsize->getSubExp3()->getOper() == opMemOf &&
					fsize->getSubExp3()->getSubExp1()->getOper() == opIntConst) {
				Exp *memof = fsize->getSubExp3();
				ADDRESS u = ((Const*)memof->getSubExp1())->getInt();
				bool ok;
				double d = prog->getFloatConstant(u, ok);
				if (ok) {
					LOG << "replacing " << memof << " with " << d << " in " << fsize << "\n";
					fsize->setSubExp3(new Const(d));
				}
			}
		}
		s->simplify();
	}
}

void UserProc::replaceExpressionsWithGlobals() {
	if (DFA_TYPE_ANALYSIS) {
		if (VERBOSE)
			LOG << "Not replacing expressions with globals because -Td in force\n";
		return;
	}
	StatementList stmts;
	getStatements(stmts);
	int sp = signature->getStackRegister(prog);

	if (VERBOSE)
		LOG << "replacing expressions with globals\n";

	// start with calls because that's where we have the most types
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		if ((*it)->isCall()) {
			CallStatement *call = (CallStatement*)*it;
			for (int i = 0; i < call->getNumArguments(); i++) {
				Type *ty = call->getArgumentType(i);
				Exp *e = call->getArgumentExp(i);
				// The below assumes that the address of a global is an integer constant
				if (ty && ty->resolvesToPointer() && e->getOper() == opIntConst) {
					Type *pty = ty->asPointer()->getPointsTo();
					if (pty->resolvesToArray() && pty->asArray()->isUnbounded()) {
						ArrayType *a = (ArrayType*)pty->asArray()->clone();
						pty = a;
						a->setLength(1024);		// just something arbitrary
						if (i+1 < call->getNumArguments()) {
							Type *nt = call->getArgumentType(i+1);
							if (nt->isNamed())
								nt = ((NamedType*)nt)->resolvesTo();
							if (nt->isInteger() && call->getArgumentExp(i+1)->isIntConst())
								a->setLength(((Const*)call->getArgumentExp(i+1))->getInt());
						}
					}
					ADDRESS u = ((Const*)e)->getInt();
					prog->globalUsed(u);
					const char *gloName = prog->getGlobalName(u);
					if (gloName) {
						ADDRESS r = u - prog->getGlobalAddr((char*)gloName);
						Exp *ne;
						if (r) {
							Location *g = Location::global(strdup(gloName), this);
							ne = new Binary(opPlus,
								new Unary(opAddrOf, g),
								new Const(r));
						} else {
							prog->setGlobalType((char*)gloName, pty);
							Location *g = Location::global(strdup(gloName), this);
							ne = new Unary(opAddrOf, g);
						}
						call->setArgumentExp(i, ne);
						if (VERBOSE)
							LOG << "replacing argument " << e << " with " << ne << " in " << call << "\n";
					}
				}
			}
		}
	}


	// replace expressions with globals
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;

		// (a) Definitions
		LocationSet defs;
		s->getDefinitions(defs);
		LocationSet::iterator rr;
		for (rr = defs.begin(); rr != defs.end(); rr++) {
			if ((*rr)->getOper() == opMemOf && (*rr)->getSubExp1()->getOper() == opIntConst) {
				Exp *memof = *rr;
				ADDRESS u = ((Const*)memof->getSubExp1())->getInt();
				prog->globalUsed(u);
				const char *gloName = prog->getGlobalName(u);
				if (gloName) {
					ADDRESS r = u - prog->getGlobalAddr((char*)gloName);
					Exp *ne;
					if (r) {
						Location *g = Location::global(strdup(gloName), this);
						ne = Location::memOf(
							new Binary(opPlus,
								new Unary(opAddrOf, g),
								new Const(r)), this);
					} else {
						Type *ty = prog->getGlobalType((char*)gloName);
						if (s->isAssign() && ((Assign*)s)->getType()) {
							int bits = ((Assign*)s)->getType()->getSize();
							if (ty == NULL || ty->getSize() == 0)
								prog->setGlobalType((char*)gloName, new IntegerType(bits));
						}
						ty = prog->getGlobalType((char*)gloName);
						Location *g = Location::global(strdup(gloName), this);
						if (ty && ty->isArray()) 
							ne = new Binary(opArraySubscript, g, new Const(0));
						else 
							ne = g;
					}
					s->searchAndReplace(memof->clone(), ne);
				}
			}
		}

		// (b) Uses
		LocationSet refs;
		s->addUsedLocs(refs);
		for (rr = refs.begin(); rr != refs.end(); rr++) {
			if (((Exp*)*rr)->isSubscript()) {
				Statement *ref = ((RefExp*)*rr)->getDef();
				Exp *r1 = (*rr)->getSubExp1();
				if (symbolMap.find(r1) != symbolMap.end())
					continue;					// Ignore locals, etc
				// look for m[exp + K]{0}, replace it with m[exp * 1 + K]{0} in the hope that it will get picked 
				// up as a global array.
				if (ref == NULL && r1->getOper() == opMemOf && r1->getSubExp1()->getOper() == opPlus &&
						r1->getSubExp1()->getSubExp2()->getOper() == opIntConst) {
					r1->getSubExp1()->setSubExp1(new Binary(opMult, r1->getSubExp1()->getSubExp1(), new Const(1)));
				}
				// Is it m[CONSTANT]{-}
				if (ref == NULL && r1->getOper() == opMemOf && r1->getSubExp1()->getOper() == opIntConst) {
					Exp *memof = r1;
					ADDRESS u = ((Const*)memof->getSubExp1())->getInt();
					prog->globalUsed(u);
					const char *gloName = prog->getGlobalName(u);
					if (gloName) {
						ADDRESS r = u - prog->getGlobalAddr((char*)gloName);
						Exp *ne;
						if (r) {
							Unary *g = Location::global(strdup(gloName), this);
							ne = Location::memOf(
								new Binary(opPlus,
									new Unary(opAddrOf, g),
									new Const(r)), this);
						} else {
							Type *ty = prog->getGlobalType((char*)gloName);
							Unary *g = Location::global(strdup(gloName), this);
							if (ty && ty->isArray() && ty->getSize() > 0) 
								ne = new Binary(opArraySubscript,
									g,
									new Const(0));
							else 
								ne = g;
						}
						s->searchAndReplace(memof->clone(), ne);
					}
				// look for m[(blah * K1 + K2)]
				} else if (ref == NULL && r1->getOper() == opMemOf && r1->getSubExp1()->getOper() == opPlus &&
						r1->getSubExp1()->getSubExp1()->getOper() == opMult &&
						r1->getSubExp1()->getSubExp1()->getSubExp2() ->getOper() == opIntConst &&
						r1->getSubExp1()->getSubExp2()->getOper() == opIntConst) {
					Exp* blah = r1->getSubExp1()->getSubExp1()->getSubExp1();
					if (blah->isSubscript())
						blah = ((RefExp*)blah)->getSubExp1();
					if (blah->isRegN(sp))
						continue;					// sp can't base an array
					Exp *memof = r1;
					// K1 is the stride
					int stride = ((Const*)memof->getSubExp1()->getSubExp1()->getSubExp2())->getInt();
					// u is K2
					ADDRESS u = ((Const*)memof->getSubExp1()->getSubExp2())->getInt();
					if (VERBOSE)
						LOG << "detected array ref with stride " << stride << "\n";
					prog->globalUsed(u);
					const char *gloName = prog->getGlobalName(u);
					if (gloName) {
						ADDRESS r = u - prog->getGlobalAddr((char*)gloName);
						Exp *ne = NULL;
						if (r) {
							// TOO HARD
						} else {
							Type *ty = prog->getGlobalType((char*)gloName);
							Location *g = Location::global(strdup(gloName), this);
							if (ty == NULL || ty->getSize() == 0) {
								LOG << "setting type of global to array\n";
								ty = new ArrayType(new IntegerType(stride*8),1);
								prog->setGlobalType((char*)gloName, ty);
							}

							if (ty && VERBOSE)
								LOG << "got type: " << ty->getCtype() << "\n";

							if (ty && ty->isArray() && ty->asArray()->getBaseType()->getSize() != stride*8) {
								if (VERBOSE)
									LOG << "forcing array base type size to stride\n";
								ty->asArray()->setLength(ty->asArray()->getLength() *
									ty->asArray()->getBaseType()->getSize() / (stride * 8));
								ty->asArray()->setBaseType(new IntegerType(stride*8));
								prog->setGlobalType((char*)gloName, ty);
							}

							if (ty && VERBOSE)
								LOG << "got type: " << ty->getCtype() << "\n";

							if (ty && ty->isArray() && ty->asArray()->getBaseType()->getSize() == stride*8) {
								LOG << "setting new exp to array ref\n";
								ne = new Binary(opArraySubscript,
									g, 
									memof->getSubExp1()->getSubExp1()->getSubExp1() ->clone());
								LOG << "set to " << ne << "\n";
							}
							/* else 
								ne = Location::memOf(new Binary(opPlus, 
										new Unary(opAddrOf, g), 
										memof->getSubExp1()->getSubExp1()->clone()
										)); */
						}
						if (ne)
							s->searchAndReplace(memof->clone(), ne);
					}
				}
			}
		}

		s->simplify();
	}
}

void UserProc::replaceExpressionsWithSymbols() {
#if 0			// Done in the back ends now
	StatementList stmts;
	getStatements(stmts);

	// replace expressions in regular statements with symbols
	// Note: O(MN) where M is the number of symbols, and N is the number of statements
	// FIXME: Should use the map properly
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		for (std::map<Exp*, Exp*,lessExpStar>::iterator it1 = symbolMap.begin(); it1 != symbolMap.end(); it1++) {
			bool ch = s->searchAndReplace((*it1).first, (*it1).second);
			if (ch && VERBOSE) {
				LOG << "std stmt: replace " << (*it1).first << " with " << (*it1).second << " result " << s << "\n";
			}
		}
	}
#endif
}

void UserProc::replaceExpressionsWithParameters(DataFlow& df, int depth) {
	StatementList stmts;
	getStatements(stmts);

	if (VERBOSE)
		LOG << "replacing expressions with parameters at depth " << depth << "\n";

	bool found = false;
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		if ((*it)->isCall()) {
			CallStatement *call = (CallStatement*)*it;
			for (int i = 0; i < call->getNumArguments(); i++) {
				Type *ty = call->getArgumentType(i);
				Exp *e = call->getArgumentExp(i);
				if (ty && ty->resolvesToPointer() && e->getOper() != opAddrOf && e->getMemDepth() == 0) {
					// Check for an expression representing the address of a local variable. NOTE: machine dependent
					if (signature->isAddrOfStackLocal(prog, e)) {
						// don't do locals here!
						continue;
					}

					Exp* ne;
					if (DFA_TYPE_ANALYSIS)
						ne = e;		// No a[m[e]]
					else {
						// Do the a[m[e]] hack
						Location *pe = Location::memOf(e, this);
						ne = new Unary(opAddrOf, pe);
					}
					if (VERBOSE)
						LOG << "replacing argument " << e << " with " << ne << " in " << call << "\n";
					call->setArgumentExp(i, ne);
					found = true;
				}
			}
		}
	}
	if (found) {
		// Must redo all the subscripting, just for the a[m[...]] thing!
		for (int d=0; d <= depth; d++)
			df.renameBlockVars(this, 0, d /* Memory depth */, true);
	}

	// replace expressions in regular statements with parameters
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		for (unsigned i = 0; i < signature->getNumParams(); i++) {
			if (depth < 0 || signature->getParamExp(i)->getMemDepth() == depth) {
				Exp *r = signature->getParamExp(i)->clone();
				r = r->expSubscriptAllNull();
				// Remove the outer {0}, for where it appears on the LHS, and because we want to have param1{0}
				assert(r->isSubscript());	// There should always be one
				// if (r->getOper() == opSubscript)
				r = r->getSubExp1();
				Location* replace = Location::param( strdup((char*)signature->getParamName(i)), this);
				Exp *n;
				if (s->search(r, n)) {
					if (VERBOSE)
						LOG << "replacing " << r << " with " << replace << " in " << s << "\n";
#if 0
					s->searchAndReplace(r, replace);
					if (VERBOSE)
						LOG << "after: " << s << "\n";
#else
					symbolMap[r] = replace;			// Add to symbol map
					// Note: don't add to locals, since otherwise the back end will declare it twice
#endif
				}
			}
		}
	}
}

// Return an expression that is equivilent to e in terms of symbols. Creates new symbols as needed.
Exp *UserProc::getSymbolExp(Exp *le, Type *ty, bool lastPass) {
	// Expression r[sp] (build just once per call)
	Exp* regSP = Location::regOf(signature->getStackRegister(prog));
	// The implicit definition for r[sp], if any
	Statement* defSP = cfg->findTheImplicitAssign(regSP);
	// The expression r[sp]{0}
	RefExp* refSP0 = new RefExp(regSP, defSP);

	Exp *e = NULL;
	if (symbolMap.find(le) == symbolMap.end()) {
		if (le->getOper() == opMemOf && signature->isOpCompatStackLocal(le->getSubExp1()->getOper()) &&
				*le->getSubExp1()->getSubExp1() == *refSP0 &&
				le->getSubExp1()->getSubExp2()->isIntConst()) {
			int le_n = ((Const*)le->getSubExp1()->getSubExp2())->getInt();
			// now test all the locals to see if this expression is an alias to one of them (for example,
			// a member of a compound typed local)
			// NOTE: Not efficient!
			for (SymbolMapType::iterator it = symbolMap.begin(); it != symbolMap.end(); it++) {
				Exp *base = it->first;
				assert(base);
				Location *local = (Location*)it->second;
				assert((local->isLocal() || local->isParam()) && local->getSubExp1()->getOper() == opStrConst);
				std::string name = ((Const*)local->getSubExp1())->getStr();
				std::map<std::string, Type*>::iterator ff = locals.find(name);
				if (ff == locals.end())
					continue;			// This symbol is not a local (e.g. a parameter); ignore
				Type *ty = ff->second;
				assert(ty);
				int size = ty->getSize() / 8;	 // getSize() returns bits!
				if (base->getOper() == opMemOf && signature->isOpCompatStackLocal(base->getSubExp1()->getOper()) &&
						*base->getSubExp1()->getSubExp1() == *refSP0 &&
						base->getSubExp1()->getSubExp2()->getOper() == opIntConst) {
					int base_n = ((Const*)base->getSubExp1()->getSubExp2()) ->getInt();
					if (le->getSubExp1()->getOper() == opPlus)
						// We have m[sp{0} + base_n]. It is probably not what we want...
						base_n = 0 - base_n;			// Treat m[sp{0} + 20] as m[sp{0} - -20]
					if (le_n <= base_n && le_n > base_n-size) {
						if (VERBOSE)
							LOG << "found alias to " << name.c_str() << ": " << le << "\n";
						int n = base_n - le_n;
						return new TypedExp(ty,
							Location::memOf(
								new Binary(opPlus, 
									new Unary(opAddrOf, 
										local->clone()),
									new Const(n)), this));
#if 0
						if (ty->resolvesToCompound()) {
							CompoundType *compound = ty->asCompound();
							return new Binary(opMemberAccess, local->clone(), 
							   new Const((char*)compound->getNameAtOffset(
							   (base_n - le_n)*8)));
						} else
							assert(false);
#endif
					}
				}
			}
		}
		
		if (ty == NULL && lastPass)
			ty = new IntegerType();

		if (ty) {
			// the default of just assigning an int type is bad..  if the locals is not an int then assigning it this
			// type early results in aliases to this local not being recognised 
			e = newLocal(ty->clone());
			symbolMap[le->clone()] = e;
			//e->clone();				// ? Suppsed to be e = e->clone()?
		}
	} else {
		e = symbolMap[le]->clone();
		if (e->getOper() == opLocal && e->getSubExp1()->getOper() == opStrConst) {
			std::string name = ((Const*)e->getSubExp1())->getStr();
			Type *nty = ty;
			Type *ty = locals[name];
			assert(ty);
			if (nty && !(*ty == *nty) && nty->getSize() >= ty->getSize()) {
				if (VERBOSE)
					LOG << "getSymbolExp: updating type of " << name.c_str() << " to " << nty->getCtype() << "\n";
				ty = nty;
				locals[name] = ty;
			}
			if (ty->resolvesToCompound()) {
				CompoundType *compound = ty->asCompound();
				if (VERBOSE)
					LOG << "found reference to first member of compound " << name.c_str() << ": " << le << "\n";
				char* nam = (char*)compound->getName(0);
				if (nam == NULL) nam = "??";
				return new TypedExp(ty, new Binary(opMemberAccess, e, new Const(nam)));
			}
		}
	}
	return e;
}

void UserProc::replaceExpressionsWithLocals(bool lastPass) {
	StatementList stmts;
	getStatements(stmts);

	if (VERBOSE) {
		LOG << "replacing expressions with locals for " << getName();
		if (lastPass)
			LOG << " last pass";
		LOG << "\n";
	}

	int sp = signature->getStackRegister(prog);
	if (getProven(Location::regOf(sp)) == NULL) {
		if (VERBOSE)
			LOG << "Can't replace locals since sp unproven\n";
		return;		// can't replace if nothing proven about sp
	}

	// start with calls because that's where we have the most types
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		if ((*it)->isCall()) {
			CallStatement *call = (CallStatement*)*it;
			for (int i = 0; i < call->getNumArguments(); i++) {
				Type *ty = call->getArgumentType(i);
				Exp *e = call->getArgumentExp(i);
				// If a pointer type and e is of the form m[sp{0} - K]:
				if (ty && ty->resolvesToPointer() && signature->isAddrOfStackLocal(prog, e)) {
					Exp *olde = e->clone();
					Type *pty = ty->asPointer()->getPointsTo();
					if (pty->resolvesToArray() && pty->asArray()->isUnbounded()) {
						ArrayType *a = (ArrayType*)pty->asArray()->clone();
						pty = a;
						a->setLength(1024);		// just something arbitrary
						if (i+1 < call->getNumArguments()) {
							Type *nt = call->getArgumentType(i+1);
							if (nt->isNamed())
								nt = ((NamedType*)nt)->resolvesTo();
							if (nt->isInteger() && call->getArgumentExp(i+1)->isIntConst())
								a->setLength(((Const*)call->getArgumentExp(i+1)) ->getInt());
						}
					}
					e = getSymbolExp(Location::memOf(e->clone(), this), pty);
					if (e) {
						Exp *ne = new Unary(opAddrOf, e);
						if (VERBOSE)
							LOG << "replacing argument " << olde << " with " << ne << " in " << call << "\n";
						call->setArgumentExp(i, ne);
					}
				}
			}
		}
	}

	// normalize sp usage (turn WILD + sp{0} into sp{0} + WILD)
	Exp *nn = new Binary(opPlus, new Terminal(opWild), new RefExp(Location::regOf(sp), NULL));
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		std::list<Exp*> results;
		s->searchAll(nn, results);
		for (std::list<Exp*>::iterator it1 = results.begin(); it1 != results.end(); it1++) {
			Exp *wild = (*it1)->getSubExp1();
			(*it1)->setSubExp1((*it1)->getSubExp2());
			(*it1)->setSubExp2(wild);
		}
	}

	// look for array locals
	// l = m[(sp{0} + WILD1) - K2]
	Exp *l = Location::memOf(new Binary(opMinus, 
				new Binary(opPlus,
					new RefExp(Location::regOf(sp), NULL),
					new Terminal(opWild)),
				new Terminal(opWildIntConst)));
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		std::list<Exp*> results;
		s->searchAll(l, results);
		for (std::list<Exp*>::iterator it1 = results.begin(); it1 != results.end(); it1++) {
			Exp *result = *it1;
			// arr = m[sp{0} - K2]
			Location *arr = Location::memOf(
				new Binary(opMinus, 
					new RefExp(Location::regOf(sp), NULL),
					result->getSubExp1()->getSubExp2()->clone()));
			int n = ((Const*)result->getSubExp1()->getSubExp2())->getInt();
			arr->setProc(this);
			Type *base = new IntegerType();
			if (s->isAssign() && ((Assign*)s)->getLeft() == result)
				if(((Assign*)s)->getType()->getSize() != 0)
					base = ((Assign*)s)->getType()->clone();
			arr->setType(new ArrayType(base, n / (base->getSize() / 8)));
			if (VERBOSE)
				LOG << "found a local array using " << n << " bytes\n";
			Exp *replace = Location::memOf(
				new Binary(opPlus,
					new Unary(opAddrOf, arr),
					result->getSubExp1()->getSubExp1()->getSubExp2()->clone()), this);
			if (VERBOSE)
				LOG << "replacing " << result << " with " << replace << " in " << s << "\n";
			s->searchAndReplace(result->clone(), replace);
		}
	}

	// Stack offsets for local variables could be negative (most machines),
	// positive (PA/RISC), or both (SPARC)
	if (signature->isLocalOffsetNegative())
		searchRegularLocals(opMinus, lastPass, sp, stmts);
	if (signature->isLocalOffsetPositive())
		searchRegularLocals(opPlus, lastPass, sp, stmts);
	// Ugh - m[sp] is a special case: neither positive or negative.
	// SPARC uses this to save %i0
	if (signature->isLocalOffsetPositive() && signature->isLocalOffsetNegative())
		searchRegularLocals(opWild, lastPass, sp, stmts);

}

void UserProc::searchRegularLocals(OPER minusOrPlus, bool lastPass, int sp, StatementList& stmts) {
	// replace expressions in regular statements with locals
	Location* l;
	if (minusOrPlus == opWild)
		// l = m[sp{0}]
		l = Location::memOf(
			new RefExp(Location::regOf(sp), NULL));
	else
		// l = m[sp{0} +/- K]
		l = Location::memOf(
			new Binary(minusOrPlus, 
				new RefExp(Location::regOf(sp), NULL),
				new Terminal(opWildIntConst)));
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		std::list<Exp*> results;
		s->searchAll(l, results);
		for (std::list<Exp*>::iterator it1 = results.begin(); it1 != results.end(); it1++) {
			Exp *result = *it1;
			Type *ty = result->getType();
			if (s->isAssign() && ((Assign*)s)->getLeft() == result)
				ty = ((Assign*)s)->getType();
			Exp *e = getSymbolExp(result, ty, lastPass);
			if (e) {
				Exp* search = result->clone();
				if (VERBOSE)
					LOG << "replacing " << search << " with " << e << " in " << s << "\n";
				s->searchAndReplace(search, e);
			}
		}
		s->simplify();
	}
}

bool UserProc::nameStackLocations() {
	Exp *match = signature->getStackWildcard();
	if (match == NULL) return false;

	bool found = false;
	StatementList stmts;
	getStatements(stmts);
	// create a symbol for every memory reference
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		Exp *memref; 
		if (s->search(match, memref)) {
			if (symbolMap.find(memref) == symbolMap.end()) {
				if (VERBOSE)
					LOG << "stack location found: " << memref << "\n";
				symbolMap[memref->clone()] = newLocal(new IntegerType());
			}
			assert(symbolMap.find(memref) != symbolMap.end());
			Location* locl = (Location*)symbolMap[memref]->getSubExp1();
			std::string name = ((Const*)locl)->getStr();
			if (memref->getType() != NULL)
				locals[name] = memref->getType();
#if 0		// No: type analysis instead of guessing types
			locals[name] = s->updateType(memref, locals[name]);
			if (VERBOSE)
				LOG << "updating type of " << name.c_str() << " to " << locals[name]->getCtype() << "\n";
#endif
			found = true;
		}
	}
	delete match;
	return found;
}

// Deprecated. Eventually replace with replaceRegistersWithLocals()
bool UserProc::nameRegisters() {
	static Exp *regOfWild = Location::regOf(new Terminal(opWild));
	bool found = false;
	int sp = signature->getStackRegister(prog);

	StatementList stmts;
	getStatements(stmts);
	// create a symbol for every register
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		Exp *reg; 
		std::list<Exp*> li;
		std::list<Exp*>::iterator ll;
		s->searchAll(regOfWild, li);
		for (ll = li.begin(); ll != li.end(); ++ll) {
			reg = *ll;
			if (reg->isRegN(sp))
				continue;				// Never name the stack pointer
			if (symbolMap.find(reg) != symbolMap.end())
				continue;
			if (VERBOSE)
				LOG << "register found: " << reg << "\n";
			Type *ty = reg->getType();
			if (ty == NULL)
				ty = new IntegerType();
			symbolMap[reg->clone()] = newLocal(ty);
			assert(symbolMap.find(reg) != symbolMap.end());
			Location* locl = (Location*)symbolMap[reg]->getSubExp1();
			std::string name = ((Const*)locl)->getStr();
			if (reg->getType() != NULL)
				locals[name] = reg->getType();
			else {
				//locals[name] = s->updateType(reg, locals[name]);
				// For now; should only affect ad hoc type analysis:
				locals[name] = new IntegerType();
				if (VERBOSE)
					LOG << "updating type of named register " << name.c_str() << " to " << locals[name]->getCtype() <<
						"\n";
			}
			found = true;
		}
	}

	return found;
}

// Core of the register replacing logic
void UserProc::regReplaceList(std::list<Exp**>& li) {
	std::list<Exp**>::iterator it;
	for (it = li.begin(); it != li.end(); it++) {
		Exp* reg = ((RefExp*)**it)->getSubExp1();
		Statement* def = ((RefExp*)**it)->getDef();
		Type *ty = def->getTypeFor(reg);
		if (symbolMap.find(reg) == symbolMap.end()) {
			symbolMap[reg] = newLocal(ty);
			Location* locl = (Location*)symbolMap[reg]->getSubExp1();
			std::string name = ((Const*)locl)->getStr();
			locals[name] = ty;
			if (VERBOSE)
				LOG << "replacing all " << reg << " with " << name << ", type " << ty->getCtype() << "\n";
		}
		// Now replace it in the IR
		//**it = symbolMap[reg];
	}
}

void UserProc::replaceRegistersWithLocals() {
	StatementList stmts;
	getStatements(stmts);
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++)
		// Bounces back most times to UserProc::regReplaceList (above)
		(*it)->regReplace(this);
}

bool UserProc::removeNullStatements() {
	bool change = false;
	StatementList stmts;
	getStatements(stmts);
	// remove null code
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		if (s->isNullStatement()) {
			// A statement of the form x := x
			if (VERBOSE) {
				LOG << "removing null statement: " << s->getNumber() <<
				" " << s << "\n";
			}
			removeStatement(s);
#if 0
			// remove from reach sets
			StatementSet &reachout = s->getBB()->getReachOut();
			if (reachout.remove(s))
				cfg->computeReaches();		// Highly sus: do all or none!
				recalcDataflow();
#endif
			change = true;
		}
	}
	return change;
}

bool UserProc::processConstants() {
	if (DFA_TYPE_ANALYSIS) {
		if (VERBOSE)
			LOG << "Not processing constants since -Td in force\n";
		return false;
	}
	if (VERBOSE)
		LOG << "Process constants for " << getName() << "\n";
	StatementList stmts;
	getStatements(stmts);
	// process any constants in the statement
	StatementList::iterator it;
	bool paramsAdded = false;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		paramsAdded |= s->processConstants(prog);
	}
	return paramsAdded;
}

// Propagate statements, but don't remove
// Respect the memory depth (don't propagate FROM statements that have components of a higher memory depth than memDepth)
// Also don't propagate TO expressions of depth other than toDepth (unless toDepth == -1)
// Return true if an indirect call is converted to direct
bool UserProc::propagateStatements(int memDepth, int toDepth) {
	StatementList stmts;
	getStatements(stmts);
	// propagate any statements that can be
	StatementSet empty;
	StatementList::iterator it;
	bool convertedIndirect = false;
	bool limitPropagations = !Boomerang::get()->noLimitPropagations;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		if (s->isPhi()) continue;
		// We can propagate to ReturnStatements now, and "return 0"
		// if (s->isReturn()) continue;
        LOG << s << "\n";
		convertedIndirect |= s->propagateTo(memDepth, empty, toDepth, limitPropagations);
	}
	simplify();
	return convertedIndirect;
}

Statement *UserProc::getStmtAtLex(unsigned int begin, unsigned int end)
{
	StatementList stmts;
	getStatements(stmts);
	
	unsigned int lowest = begin;
	Statement *loweststmt = NULL;
	for (StatementList::iterator it = stmts.begin(); it != stmts.end(); it++)
		if (begin >= (*it)->getLexBegin() && begin <= lowest && begin <= (*it)->getLexEnd() &&
				(end == (unsigned)-1 || end < (*it)->getLexEnd())) {
			loweststmt = (*it);
			lowest = (*it)->getLexBegin();
		}
	return loweststmt;
}

void UserProc::promoteSignature() {
	signature = signature->promote(this);
}

Exp* UserProc::newLocal(Type* ty) {
	std::ostringstream os;
	os << "local" << nextLocal++;
	std::string name = os.str();
	locals[name] = ty;
	if (ty == NULL) {
		std::cerr << "null type passed to newLocal\n";
		assert(false);
	}
	if (VERBOSE)
		LOG << "assigning type " << ty->getCtype() << " to new " << name.c_str() << "\n";
	return Location::local(strdup(name.c_str()), this);
}

Type *UserProc::getLocalType(const char *nam)
{
	if (locals.find(nam) == locals.end())
		return NULL;
	return locals[nam];
}

void UserProc::setLocalType(const char *nam, Type *ty)
{
	locals[nam] = ty;
	if (VERBOSE)
		LOG << "setLocalType: updating type of " << nam << " to " << ty->getCtype() << "\n";
}

void UserProc::setExpSymbol(const char *nam, Exp *e, Type* ty)
{
	// NOTE: does not update symbols[]
	TypedExp *te = new TypedExp(ty, Location::local(strdup(nam), this));
	symbolMap[e] = te;
}

Exp *UserProc::expFromSymbol(const char *nam)
{
	for (SymbolMapType::iterator it = symbolMap.begin(); it != symbolMap.end(); it++) {
		Exp* e = it->second;
		if (e->isLocal() && !strcmp(((Const*)((Location*)e)->getSubExp1())->getStr(), nam))
			return it->first;
	}
	return NULL;
}

const char* UserProc::getLocalName(int n) { 
	int i = 0;
	for (std::map<std::string, Type*>::iterator it = locals.begin(); it != locals.end(); it++, i++)
		if (i == n)
			return it->first.c_str();
	return NULL;
}

char* UserProc::getSymbolName(Exp* e) {
	SymbolMapType::iterator it = symbolMap.find(e);
	if (it == symbolMap.end()) return NULL;
	Exp* loc = it->second;
	if (!loc->isLocal()) return NULL;
	return ((Const*)((Location*)loc)->getSubExp1())->getStr();
}


void UserProc::countRefs(RefCounter& refCounts) {
	StatementList stmts;
	getStatements(stmts);
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		if (s->isPhi()) {
			((PhiAssign*)s)->simplifyRefs();
			s->simplify();
		}
		if (DEBUG_UNUSED_STMT || DEBUG_UNUSED_RETS_PARAMS && s->isReturn())
			LOG << "counting references in " << s << "\n";
		LocationSet refs;
#define IGNORE_IMPLICITS 0
#if IGNORE_IMPLICITS
		s->addUsedLocs(refs, true);
#else
		s->addUsedLocs(refs);
#endif
		LocationSet::iterator rr;
		for (rr = refs.begin(); rr != refs.end(); rr++) {
			if (((Exp*)*rr)->isSubscript()) {
				Statement *ref = ((RefExp*)*rr)->getDef();
				if (ref && ref->getNumber()) {
					refCounts[ref]++;
					if (DEBUG_UNUSED_STMT || DEBUG_UNUSED_RETS_PARAMS && s->isReturn())
						LOG << "counted ref to " << *rr << "\n";
				}
			}
		}
	}
}

// Note: call the below after translating from SSA form
void UserProc::removeUnusedLocals() {
	if (VERBOSE)
		LOG << "Removing unused locals (final) for " << getName() << "\n";
	std::set<std::string> usedLocals;
	StatementList stmts;
	getStatements(stmts);
	// First count any uses of the locals
	StatementList::iterator ss;
	for (ss = stmts.begin(); ss != stmts.end(); ss++) {
		Statement* s = *ss;
		LocationSet refs;
		s->addUsedLocs(refs, true);
		LocationSet::iterator rr;
		for (rr = refs.begin(); rr != refs.end(); rr++) {
			Exp* r = *rr;
			//if (r->isSubscript())					// Presumably never seen now
			//	r = ((RefExp*)r)->getSubExp1();
			char* sym = findLocal(r);				// Look up raw expressions in the symbolMap, and check in symbols
			// Must be a real symbol, and not defined in this statement, unless it is a return statement (in which case
			// it is used outside this procedure). Consider local7 = local7+1 and return local7 = local7+1, where in
			// both cases, local7 is not used elsewhere outside this procedure. With the assign, it can be deleted,
			// but with the return statement, it can't.
			if (sym && (s->isReturn() || !s->definesLoc(r))) {
			// Must be a real symbol, and not defined in this statement
				std::string name(sym);				// (e.g. consider local7 = local7 & 0xFB, and never used elsewhere)
				usedLocals.insert(name);
				if (VERBOSE) LOG << "Counted local " << sym << " in " << s << "\n";
			}
		}
	}
	// Now record the unused ones in set removes
	std::map<std::string, Type*>::iterator it;
	std::set<std::string> removes;
#if 0
	int nextLocal = 0;
#endif
	for (it = locals.begin(); it != locals.end(); it++) {
		std::string& name = const_cast<std::string&>(it->first);
		// LOG << "Considering local " << name << "\n";
		if (usedLocals.find(name) == usedLocals.end()) {
			if (VERBOSE)
				LOG << "Removed unused local " << name.c_str() << "\n";
			removes.insert(name);
		}
#if 0	// Ugh - still have to rename the variables.
		else {
			if (name.substr(0, 5) == "local") {
				// Make the locals consequtive
				std::ostringstream os;
				os << "local" << nextLocal++;
				name = os.str();
			}
		}
#endif
	}
	// Remove any definitions of the removed locals
	for (ss = stmts.begin(); ss != stmts.end(); ++ss) {
		Statement* s = *ss;
		LocationSet ls;
		LocationSet::iterator ll;
		s->getDefinitions(ls);
		for (ll = ls.begin(); ll != ls.end(); ++ll) {
			char* name = findLocal(*ll);
			if (name == NULL) continue;
			std::string str(name);
			if (removes.find(str) != removes.end()) {
				// Remove it. If an assign, delete it; otherwise (call), remove the define
				if (s->isAssignment()) {
					removeStatement(s);
					break;				// Break to next statement
				} else if (s->isCall())
					// Remove just this define. May end up removing several defines from this call.
					((CallStatement*)s)->removeDefine(*ll);
				// else if a ReturnStatement, don't attempt to remove it. The definition is used *outside* this proc.
			}
		}
	}
	// Finally, remove them from locals, so they don't get declared
	for (std::set<std::string>::iterator it1 = removes.begin(); it1 != removes.end(); it1++)
		locals.erase(*it1);
}

// Note: if depth < 0, consider all depths
void UserProc::removeUnusedStatements(RefCounter& refCounts, int depth) {
	StatementList stmts;
	getStatements(stmts);
	bool change;
	do {
		change = false;
		StatementList::iterator ll = stmts.begin();
		while (ll != stmts.end()) {
			Statement* s = *ll;
#if 0			// No need to do this any more!
			if (s->isCall() && refCounts[s] == 0) {
				if (VERBOSE)
					LOG << "clearing return set of unused call " << s << "\n";
				CallStatement *call = (CallStatement*)s;
				for (int i = 0; i < call->getNumReturns(); i++)
					if (call->getReturnExp(i) && (depth < 0 || call->getReturnExp(i)->getMemDepth() <= depth))
						call->ignoreReturn(i);
				ll++;
				continue;
			}
#endif
			if (!s->isAssignment()) {
				// Never delete a statement other than an assignment (e.g. nothing "uses" a Jcond)
				ll++;
				continue;
			}
			Assignment* as = (Assignment*)s;
			Exp* asLeft = as->getLeft();
			if (asLeft && depth >= 0 && asLeft->getMemDepth() > depth) {
				ll++;
				continue;
			}
			if (asLeft && asLeft->getOper() == opGlobal) {
				// assignments to globals must always be kept
				ll++;
				continue;
			}
			if (asLeft->getOper() == opMemOf &&
					symbolMap.find(asLeft) == symbolMap.end() &&		// Real locals are OK
					(!as->isAssign() || !(*new RefExp(asLeft, NULL) == *((Assign*)s)->getRight()))) {
				// Looking for m[x] := anything but m[x]{-}
				// Assignments to memof anything-but-local must always be kept.
				ll++;
				continue;
			}
#if 0		// We actually want to remove this if no-one is using it, otherwise we'll create an interference that we
			// can't handle
			if (asLeft->getOper() == opParam) {
				ll++;
				continue;
			}
#endif
			if (asLeft->getOper() == opMemberAccess || asLeft->getOper() == opArraySubscript) {
				// can't say with these
				ll++;
				continue;
			}
			if (refCounts.find(s) == refCounts.end() || refCounts[s] == 0) {	// Care not to insert unnecessarily
				// First adjust the counts, due to statements only referenced by statements that are themselves unused.
				// Need to be careful not to count two refs as two; refCounts is a count of the number of statements
				// that use a definition, not the total number of refs
				StatementSet stmtsRefdByUnused;
				LocationSet components;
				s->addUsedLocs(components);
				LocationSet::iterator cc;
				for (cc = components.begin(); cc != components.end(); cc++) {
					if ((*cc)->isSubscript()) {
						stmtsRefdByUnused.insert(((RefExp*)*cc)->getDef());
					}
				}
				StatementSet::iterator dd;
				for (dd = stmtsRefdByUnused.begin(); dd != stmtsRefdByUnused.end(); dd++) {
					if (*dd == NULL) continue;
					if (DEBUG_UNUSED_STMT)
						LOG << "Decrementing ref count of " << (*dd)->getNumber() << " because "
							<< s->getNumber() << " is unused\n";
					refCounts[*dd]--;
				}
				if (DEBUG_UNUSED_STMT)
					LOG << "Removing unused statement " << s->getNumber() << " " << s << "\n";
				removeStatement(s);
				ll = stmts.erase(ll);	// So we don't try to re-remove it
				change = true;
				continue;				// Don't call getNext this time
			}
			ll++;
		}
	} while (change);
}

//
//	SSA code
//

void UserProc::fromSSAform() {
	if (VERBOSE||1)
		LOG << "transforming " << getName() << " from SSA\n";

	StatementList stmts;
	getStatements(stmts);
	igraph ig;

	// First split the live ranges where needed, i.e. when the type of a subscripted variable is different to
	// its previous type. Start at the top, because we don't want to rename parameters (e.g. argc)
	StatementList::iterator it;
	std::map<Exp*, Type*, lessExpStar> firstTypes;
	std::map<Exp*, Type*, lessExpStar>::iterator ff;
	// Start with the parameters. There is not always a use of every parameter, yet that location may be used with
	// a different type (e.g. envp used as int in test/sparc/fibo-O4)
	int n = signature->getNumParams();
	for (int i=0; i < n; i++) {
		Exp* namedParam = Location::param(signature->getParamName(i));
		firstTypes[namedParam] = signature->getParamType(i);
	}
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		LocationSet defs;
		s->getDefinitions(defs);
		LocationSet::iterator dd;
		for (dd = defs.begin(); dd != defs.end(); dd++) {
			Exp* base = *dd;
			Type* ty = s->getTypeFor(base);
			ff = firstTypes.find(base);
			if (ff == firstTypes.end()) {
				// There is no first type yet. Record it.
				firstTypes[base] = ty;
			} else if (ff->second && !ty->isCompatibleWith(ff->second)) {
				// There already is a type for base, and it is different to the type for this definition.
				// Record an "interference" so it will get a new variable
				RefExp* ref = new RefExp(base, s);
				//ig[ref] = newLocal(ty);
				ig[ref] = getSymbolExp(ref, ty);
			}
		}
	}
	// Find the interferences generated by more than one version of a variable being live at the same program point
	cfg->findInterferences(ig);


	if (DEBUG_LIVENESS) {
		LOG << "  ig Interference graph:\n";
		igraph::iterator ii;
		for (ii = ig.begin(); ii != ig.end(); ii++)
			LOG << "  ig " << ii->first << " -> " << ii->second << "\n";
	}

	// First rename the variables (including phi's, but don't remove).  The below could be replaced by
	//  replaceExpressionsWithSymbols() now, except that then references don't get removed.
	// NOTE: it is not possible to postpone renaming these locals till the back end, since the same base location
	// may require different names at different locations, e.g. r28{-} is local0, r28{16} is local1
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		s->fromSSAform(ig);
	}

	// Now remove the phis
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		if (!s->isPhi()) continue;
		// Check that the base variables are all the same
		PhiAssign* pa = (PhiAssign*)s;
		LocationSet refs;
		pa->addUsedLocs(refs);
		Exp* first = pa->begin()->e;
		bool phiParamsSame = true;
		if (pa->getNumDefs() > 1) {
			PhiAssign::iterator uu;
			for (uu = ++pa->begin(); uu != pa->end(); uu++) {
				if (!(*uu->e == *first)) {
					phiParamsSame = false;
					break;
				}
			}
		}
		if (phiParamsSame) {
			// Is the left of the phi assignment the same base variable as all the operands?
			if (*pa->getLeft() == *first) {
				if (DEBUG_LIVENESS || DEBUG_UNUSED_STMT)
					LOG << "Removing phi: left and all refs same or 0: " << s << "\n";
				// Just removing the refs will work, or removing the whole phi
				// NOTE: Removing the phi here may cause other statments to be not used. Soon I want to remove the
				// phi's earlier, so this code can be removed. - MVE
				removeStatement(s);
			} else
				// Need to replace the phi by an expression,
				// e.g. local0 = phi(r24{3}, r24{5}) becomes 
				//		local0 = r24
				pa->convertToAssign(first->clone());
		}
		else {
			// Need new local. Under certain circumstances (e.g. sparc/fromssa2) we needed a copy statement, but most
			// times that just creates more locals than is necessary.
			// TODO: find a safe way of doing the "optimisation" below
#if 1
			// Just replace all the definitions the phi statement refers to with tempLoc.
			// WRONG! There can be other uses of those definitions. Even if these are changed, what if we have to
			// change definition to two or more new variables? So really need copies, unless something clever is done.
			// Exp* tempLoc = newLocal(pa->getType());
			Exp* tempLoc = getSymbolExp(new RefExp(pa->getLeft(), pa), pa->getType());
			if (DEBUG_LIVENESS)
				LOG << "Phi statement " << s << " requires local, using " << tempLoc << "\n";
			// For each definition ref'd in the phi
			PhiAssign::iterator rr;
			for (rr = pa->begin(); rr != pa->end(); rr++) {
#if 0
				// Replace the LHS of the definitions (use setLeftFor, since some could be calls with more than one
				// return) with the new temporary
				rr->def->setLeftFor(rr->e, tempLoc);
#else
				insertAssignAfter(rr->def, tempLoc, rr->e);
#endif
			}
			// Replace the RHS of the phi with tempLoc
			pa->convertToAssign(tempLoc);
#else
			// We can often just use the LHS of the phiassign. The problem is that the variable at the left of the
			// phiassign can overlap with other versions of the same named variable. It may have something to do
			// with one of the arguments of the phi statement being in the interference graph.
			// The proper solution would be to change the igraph so it is obvious what interferes with what.
			// Replace the LHS of the definitions with the left of the PhiAssign
			Exp* left = pa->getLeft();
			if (DEBUG_LIVENESS)
				LOG << "Phi statement " << s << " requires back substitution, using " << left << "\n";
			// For each definition ref'd in the phi
			PhiAssign::iterator rr;
			for (rr = pa->begin(); rr != pa->end(); rr++) {
				// Replace the LHS of the definitions (use setLeftFor, since some could be calls with more than one
				// return) with left
				rr->def->setLeftFor(rr->e, left);
			}
#endif
		}
	}

	// Now remove subscripts from the symbol map
	SymbolMapType::iterator ss;
	SymbolMapType temp(symbolMap);		// Have to copy to a new map, since the ordering is changed by stripping subs!
	symbolMap.clear();
	for (ss = temp.begin(); ss != temp.end(); ++ss) {
		bool allZero;
		Exp* from = ss->first->removeSubscripts(allZero);
		if (allZero)
			symbolMap[from] = ss->second;		// Put the unsubscripted version into the symbolMap
		else
			// Put the subscripted version back, e.g. r24{39}, so it will be declared in the decompiled output (e.g.
			// local10 /* r24{39} */. This one will be the result of a liveness overlap, and the local is already
			symbolMap[ss->first] = ss->second;	// modified into the IR.
	}
}

void UserProc::insertArguments(StatementSet& rs) {
	cfg->insertArguments(rs);
}

bool inProve = false;

bool UserProc::canProveNow()
{
	return !inProve;
}

// this function is non-reentrant
bool UserProc::prove(Exp *query)
{
	if (inProve) {
		LOG << "attempted reentry of prove, returning false\n";
		return false;
	}
	inProve = true;
	if (proven.find(query) != proven.end()) {
		inProve = false;
		if (DEBUG_PROOF) LOG << "prove returns true\n";
		return true;
	}

	if (Boomerang::get()->noProve) {
		inProve = false;
		return false;
	}

	Exp *original = query->clone();

	assert(query->getOper() == opEquals);
	
	// subscript locs on the right with {-} (NULL reference)
	LocationSet locs;
	query->getSubExp2()->addUsedLocs(locs);
	LocationSet::iterator xx;
	for (xx = locs.begin(); xx != locs.end(); xx++) {
		query->refSubExp2() = query->getSubExp2()->expSubscriptValNull(*xx);
	}

	if (query->getSubExp1()->getOper() != opSubscript) {
		bool gotdef = false;
		// replace expression from return set with expression in the collector of the return 
		if (theReturnStatement) {
			RefExp* ref = theReturnStatement->findDefFor(query->getSubExp1());
			if (ref) {
				query->refSubExp1() = ref;
				gotdef = true;
			}
		}
		if (!gotdef && DEBUG_PROOF) {
			LOG << "not in return collector: " << query->getSubExp1() << "\n" << "prove returns false\n";
			inProve = false;
			return false;
		}
	}

	proven.insert(original);
	std::set<PhiAssign*> lastPhis;
	std::map<PhiAssign*, Exp*> cache;
	if (!prover(query, lastPhis, cache)) {
		proven.erase(original);
		//delete original;
		inProve = false;
		if (DEBUG_PROOF) LOG << "prove returns false\n";
		return false;
	}
	//delete query;
 
	inProve = false;
	if (DEBUG_PROOF) LOG << "prove returns true\n";
	return true;
}

bool UserProc::prover(Exp *query, std::set<PhiAssign*>& lastPhis, std::map<PhiAssign*, Exp*> &cache, PhiAssign* lastPhi)
{
	std::map<CallStatement*, Exp*> callwd;
	Exp *phiInd = query->getSubExp2()->clone();

	if (lastPhi && cache.find(lastPhi) != cache.end() && *cache[lastPhi] == *phiInd) {
		if (DEBUG_PROOF)
			LOG << "true - in the cache\n";
		return true;
	} 

	std::set<Statement*> refsTo;

	query = query->clone();
	bool change = true;
	bool swapped = false;
	while (change) {
		if (DEBUG_PROOF) {
			LOG << query << "\n";
		}
	
		change = false;
		if (query->getOper() == opEquals) {

			// same left and right means true
			if (*query->getSubExp1() == *query->getSubExp2()) {
				query = new Terminal(opTrue);
				change = true;
			}

			// move constants to the right
			Exp *plus = query->getSubExp1();
			Exp *s1s2 = plus ? plus->getSubExp2() : NULL;
			if (!change && plus->getOper() == opPlus && s1s2->isIntConst()) {
				query->refSubExp2() = new Binary(opPlus,
					query->getSubExp2(),
					new Unary(opNeg, s1s2->clone()));
				query->refSubExp1() = ((Binary*)plus)->becomeSubExp1();
				change = true;
			}
			if (!change && plus->getOper() == opMinus && s1s2->isIntConst()) {
				query->refSubExp2() = new Binary(opPlus, query->getSubExp2(), s1s2->clone());
				query->refSubExp1() = ((Binary*)plus)->becomeSubExp1();
				change = true;
			}


			// substitute using a statement that has the same left as the query
			if (!change && query->getSubExp1()->getOper() == opSubscript) {
				RefExp *r = (RefExp*)query->getSubExp1();
				Statement *s = r->getDef();
				CallStatement *call = dynamic_cast<CallStatement*>(s);
				if (call) {
					// See if we can prove something about this register.
					// Note: it could be the original register we are looking for ("proven" by induction)
					Exp *right = call->getProven(r->getSubExp1());
					if (right) {
						right = right->clone();
						if (callwd.find(call) != callwd.end() && *callwd[call] == *query) {
							LOG << "found call loop to " << call->getDestProc()->getName() << " " << query << "\n";
							query = new Terminal(opFalse);
							change = true;
						} else {
							callwd[call] = query->clone();
							if (DEBUG_PROOF)
								LOG << "using proven (or induction) for " << call->getDestProc()->getName() << " " 
									<< r->getSubExp1() << " = " << right << "\n";
							right = call->localiseExp(right);
							if (DEBUG_PROOF)
								LOG << "right with subs: " << right << "\n";
							query->setSubExp1(right);
							change = true;
						}
					}
				} else if (s && s->isPhi()) {
					// for a phi, we have to prove the query for every statement
					PhiAssign *pa = (PhiAssign*)s;
					PhiAssign::iterator it;
					bool ok = true;
					if (lastPhis.find(pa) != lastPhis.end() || pa == lastPhi) {
						if (DEBUG_PROOF)
							LOG << "phi loop detected ";
						ok = (*query->getSubExp2() == *phiInd);
						if (ok && DEBUG_PROOF)
							LOG << "(set true due to induction)\n";
						if (!ok && DEBUG_PROOF)
							LOG << "(set false " << query->getSubExp2() << " != " << phiInd << ")\n";
					} else {
						if (DEBUG_PROOF)
							LOG << "found " << s << " prove for each\n";
						for (it = pa->begin(); it != pa->end(); it++) {
							Exp *e = query->clone();
							RefExp *r1 = (RefExp*)e->getSubExp1();
							r1->setDef(it->def);
							if (DEBUG_PROOF)
								LOG << "proving for " << e << "\n";
							lastPhis.insert(lastPhi);
							if (!prover(e, lastPhis, cache, pa)) { 
								ok = false; 
								//delete e; 
								break; 
							}
							lastPhis.erase(lastPhi);
							//delete e;
						}
						if (ok)
							cache[pa] = query->getSubExp2()->clone();
					}
					if (ok)
						query = new Terminal(opTrue);
					else 
						query = new Terminal(opFalse);
					change = true;
				} else if (s && s->isAssign()) {
					if (s && refsTo.find(s) != refsTo.end()) {
						LOG << "detected ref loop " << s << "\n";
						assert(false);
					} else {
						refsTo.insert(s);
						query->setSubExp1(((Assign*)s)->getRight()->clone());
						change = true;
					}
				}
			}

			// remove memofs from both sides if possible
			if (!change && query->getSubExp1()->getOper() == opMemOf && query->getSubExp2()->getOper() == opMemOf) {
				query->refSubExp1() = ((Unary*)query->getSubExp1())->becomeSubExp1();
				query->refSubExp2() = ((Unary*)query->getSubExp2())->becomeSubExp1();
				change = true;
			}

			// is ok if both of the memofs is subscripted with NULL
			if (!change && query->getSubExp1()->getOper() == opSubscript &&
					query->getSubExp1()->getSubExp1()->getOper() == opMemOf &&
					((RefExp*)query->getSubExp1())->getDef() == NULL &&
					query->getSubExp2()->getOper() == opSubscript &&
					query->getSubExp2()->getSubExp1()->getOper() == opMemOf &&
					((RefExp*)query->getSubExp2())->getDef() == NULL) {
				query->refSubExp1() = ((Unary*)query->getSubExp1()->getSubExp1())->becomeSubExp1();
				query->refSubExp2() = ((Unary*)query->getSubExp2()->getSubExp1())->becomeSubExp1();
				change = true;
			}

			// find a memory def for the right if there is a memof on the left
			if (!change && query->getSubExp1()->getOper() == opMemOf) {
				StatementList stmts;
				getStatements(stmts);
				StatementList::iterator it;
				for (it = stmts.begin(); it != stmts.end(); it++) {
					Assign* s = (Assign*)*it;
					if (s->isAssign() && *s->getRight() == *query->getSubExp2() &&
							s->getLeft()->getOper() == opMemOf) {
						query->refSubExp2() = s->getLeft()->clone();
						change = true;
						break;
					}
				}
			}

			// last chance, swap left and right if havn't swapped before
			if (!change && !swapped) {
				Exp *e = query->getSubExp1();
				query->refSubExp1() = query->getSubExp2();
				query->refSubExp2() = e;
				change = true;
				swapped = true;
				refsTo.clear();
			}
		} else if (query->isIntConst()) {
			Const *c = (Const*)query;
			query = new Terminal(c->getInt() ? opTrue : opFalse);
		}

		Exp *old = query->clone();

		query = query->clone()->simplify();

		if (change && !(*old == *query) && DEBUG_PROOF) {
			LOG << old << "\n";
		}
		//delete old;
	}
	
	return query->getOper() == opTrue;
}

// Get the set of locations defined by this proc. In other words, the define set, currently called returns
void UserProc::getDefinitions(LocationSet& ls) {
	int n = signature->getNumReturns();
	for (int j=0; j < n; j++) {
		ls.insert(signature->getReturnExp(j));
	}
}

#if 0			// All going well, we don't need this any more
// "Local" member function, used below
void UserProc::doCountReturns(Statement* def, ReturnCounter& rc, Exp* loc)
{
	if (def == NULL) return;
	CallStatement* call = dynamic_cast<CallStatement*>(def);
	if (call == NULL) return;
	// We have a reference to a return of the call statement
	UserProc* proc = (UserProc*) call->getDestProc();
	//if (proc->isLib()) return;
	if (DEBUG_UNUSED_RETS_PARAMS) {
		LOG << " @@ Counted use of return location " << loc << " for call to ";
		if (proc) 
			LOG << proc->getName();
		else
			LOG << "(null)";
		LOG << " at " << def->getNumber() << " in " << getName() << "\n";
	}
	// We want to count the return that corresponds to this loc. This can be a different expression to loc because
	// replacements are done in the call's return list as part of decompilation
	int n = call->findReturn(loc);
	if (n != -1) {
		Exp *ret = NULL;
		if (proc)
			ret = proc->getSignature()->getReturnExp(n);
		else {
			assert(call->isComputed());
			std::vector<Exp*> &returns = getProg()->getDefaultReturns();
			assert(n < (int)returns.size());
			ret = returns[n];
		}
		rc[proc].insert(ret);
	}
}

void UserProc::countUsedReturns(ReturnCounter& rc) {
	if (DEBUG_UNUSED_RETS_PARAMS)
		LOG << " @@ Counting used returns in " << getName() << "\n";
	StatementList stmts;
	getStatements(stmts);
	StatementList::iterator ss;
	// For each statement this proc
	for (ss = stmts.begin(); ss != stmts.end(); ss++) {
		LocationSet used;
		(*ss)->addUsedLocs(used);
		LocationSet::iterator ll;
		// For each use this statement
		for (ll = used.begin(); ll != used.end(); ll++) {
			Statement* def;
			Exp* loc = *ll;
			if (loc->isLocal()) {
				// We want the raw expression here
				loc = getSymbolExp(((Const*)((Location*)loc)->getSubExp1())->getStr());
				if (loc == NULL) continue;		// Needed?
			}
			if (loc->isSubscript()) {
				// for this one reference
				def = ((RefExp*)loc)->getDef();
				doCountReturns(def, rc, ((RefExp*)loc)->getSubExp1());
#if 0
			} else if ((loc)->isPhi()) {
				StatementVec::iterator rr;
				PhiAssign& pa = (PhiAssign&)*loc;
				// for each reference this phi expression
				for (rr = pa.begin(); rr != pa.end(); rr++)
					doCountReturns(rr->def, rc, pa.getSubExp1());
#endif
			} 
		}
	}
}
#endif


bool UserProc::removeUnusedReturns(ReturnCounter& rc) {
	std::set<Exp*, lessExpStar> removes;	// Else iterators confused
	std::set<Exp*, lessExpStar>& useSet = rc[this];
	for (unsigned i = 0; i < signature->getNumReturns(); i++) {
		Exp *ret = signature->getReturnExp(i);
		if (useSet.find(ret) == useSet.end()) {
			removes.insert(ret);
		}
	}
	std::set<Exp*, lessExpStar>::iterator it;
	// Exp* stackExp = NULL;
	// if (signature->isPromoted())
		// stackExp = Location::regOf(signature->getStackRegister());
	bool removedOne = false;
	for (it = removes.begin(); it != removes.end(); it++) {
		// The logic here was doubly screwed. For one thing, there should not have been a not on isPromoted.
		// For another, when signatures are not promoted, getStackRegister() (the one that does NOT take a Prog
		// parameter) always returned 0 (!!), so for unpromoted procs, the SP was removed.
		// Plus, it turns out that somehow even full signature procs need this removal
	 	//if ( signature->isPromoted() && !(*stackExp == **it))
		//if (!signature->isPromoted() &&	 (*stackExp == **it))
		//if (**it == *stackExp && signature->isFullSignature())
			// If known to have a full signature, don't attempt to remove the SP
			// continue;
		if (DEBUG_UNUSED_RETS_PARAMS)
			LOG << " @@ Removing unused return " << *it << " in " << getName() << "\n";
		removeReturn(*it);
		removedOne = true;
	}
	return removedOne;
}

void Proc::addCallers(std::set<UserProc*>& callers) {
	std::set<CallStatement*>::iterator it;
	for (it = callerSet.begin(); it != callerSet.end(); it++) {
		UserProc* callerProc = (*it)->getProc();
		callers.insert(callerProc);
	}
}

void UserProc::addCallees(std::list<UserProc*>& callees) {
    // SLOW SLOW SLOW
    // this function is evil now... REALLY evil... hope it doesn't get called too often
	std::list<Proc*>::iterator it;
	for (it = calleeList.begin(); it != calleeList.end(); it++) {
		UserProc* callee = (UserProc*)(*it);
		if (callee->isLib()) continue;
		addCallee(callee);
	}
}

void UserProc::conTypeAnalysis() {
	if (DEBUG_TA)
		LOG << "Type Analysis for Procedure " << getName() << "\n";
	Constraints consObj;
	LocationSet cons;
	StatementList stmts;
	getStatements(stmts);
	StatementList::iterator ss;
	// For each statement this proc
	int conscript = 0;
	for (ss = stmts.begin(); ss != stmts.end(); ss++) {
		cons.clear();
		// So we can co-erce constants:
		conscript = (*ss)->setConscripts(conscript);
		(*ss)->genConstraints(cons);
		consObj.addConstraints(cons);
		if (DEBUG_TA)
			LOG << (*ss) << "\n" << &cons << "\n";
		// Remove the sizes immediately the constraints are generated.
		// Otherwise, x and x*8* look like different expressions
		(*ss)->stripSizes();
	}

	std::list<ConstraintMap> solns;
	bool ret = consObj.solve(solns);
	if (VERBOSE || DEBUG_TA) {
		if (!ret)
			LOG << "** Could not solve type constraints for proc " << getName() << "!\n";
		else if (solns.size() > 1)
			LOG << "** " << solns.size() << " solutions to type constraints for proc " << getName() << "!\n";
	}
		
	std::list<ConstraintMap>::iterator it;
	int solnNum = 0;
	ConstraintMap::iterator cc;
	if (DEBUG_TA) {
		for (it = solns.begin(); it != solns.end(); it++) {
			LOG << "Solution " << ++solnNum << " for proc " << getName() << "\n";
			ConstraintMap& cm = *it;
			for (cc = cm.begin(); cc != cm.end(); cc++)
				LOG << cc->first << " = " << cc->second << "\n";
			LOG << "\n";
		}
	}

	// Just use the first solution, if there is one
	Prog* prog = getProg();
	if (solns.size()) {
		ConstraintMap& cm = *solns.begin();
		for (cc = cm.begin(); cc != cm.end(); cc++) {
			// Ick. A workaround for now (see test/pentium/sumarray-O4)
			//assert(cc->first->isTypeOf());
if (!cc->first->isTypeOf()) continue;
			Exp* loc = ((Unary*)cc->first)->getSubExp1();
			assert(cc->second->isTypeVal());
			Type* ty = ((TypeVal*)cc->second)->getType();
			if (loc->isSubscript())
				loc = ((RefExp*)loc)->getSubExp1();
			if (loc->isGlobal()) {
				char* nam = ((Const*)((Unary*)loc)->getSubExp1())->getStr();
				prog->setGlobalType(nam, ty->clone());
			} else if (loc->isLocal()) {
				char* nam = ((Const*)((Unary*)loc)->getSubExp1())->getStr();
				setLocalType(nam, ty);
			} else if (loc->isIntConst()) {
				Const* con = (Const*)loc;
				int val = con->getInt();
				if (ty->isFloat()) {
					// Need heavy duty cast here
					// MVE: check this! Especially when a double prec float
					con->setFlt(*(float*)&val);
					con->setOper(opFltConst);
				} else if (ty->isPointer() && ((PointerType*)ty)->getPointsTo()->resolvesToChar()) {
					// Convert to a string
					char* str = prog->getStringConstant(val, true);
					if (str) {
						// Make a string
						con->setStr(str);
						con->setOper(opStrConst);
					}
				}
				else {
					if (ty->isInteger() && ty->getSize() && ty->getSize() != STD_SIZE)
						// Wrap the constant in a TypedExp (for a cast)
						castConst(con->getConscript(), ty);
				}
			}
		}
	}

	// Clear the conscripts. These confuse the fromSSA logic, causing infinite
	// loops
	for (ss = stmts.begin(); ss != stmts.end(); ss++) {
		(*ss)->clearConscripts();
	}
}




bool UserProc::searchAndReplace(Exp *search, Exp *replace)
{
	bool ch = false;
	StatementList stmts;
	getStatements(stmts);
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		Statement* s = *it;
		ch |= s->searchAndReplace(search, replace);	
	}
	return ch; 
}

unsigned fudge(StatementList::iterator x) {
	StatementList::iterator y = x;
	return *(unsigned*)&y;
}

Exp *UserProc::getProven(Exp *left) {
	// Note: proven information is in the form "r28 = (r28 + 4)"
	for (std::set<Exp*, lessExpStar>::iterator it = proven.begin(); it != proven.end(); it++) 
		if (*(*it)->getSubExp1() == *left)
			return (*it)->getSubExp2();
	// 	not found, try the signature
	// No! The below should only be for library functions!
	// return signature->getProven(left);
	return NULL;
}

bool UserProc::isPreserved(Exp* e) {
	RefExp	re(e, NULL);				// e{-}
	Binary equate(opEquals, e, &re);	// e == e{-}
	return proven.find(&equate) != proven.end();
}

void UserProc::castConst(int num, Type* ty) {
	StatementList stmts;
	getStatements(stmts);
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		if ((*it)->castConst(num, ty))
			break;
	}
}

// Process calls with ellipsis parameters. Return true if any signature parameter types added.
bool UserProc::ellipsisProcessing() {
	StatementList stmts;
	getStatements(stmts);
	StatementList::iterator it;
	bool ch = false;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		CallStatement* call = dynamic_cast<CallStatement*>(*it);
		if (call) ch |= call->ellipsisProcessing(prog);
	}
	return ch;
}

class LibProcMemo : public Memo {
public:
	LibProcMemo(int mId) : Memo(mId) { }

	bool visited;
	Prog *prog;
	Signature *signature;						// r
	ADDRESS address;
	Proc *m_firstCaller;
	ADDRESS m_firstCallerAddr;
	std::set<Exp*, lessExpStar> proven;			// r
	std::set<CallStatement*> callerSet;
	Cluster *cluster;
};

Memo *LibProc::makeMemo(int mId)
{
	LibProcMemo *m = new LibProcMemo(mId);
	m->visited = visited;
	m->prog = prog;
	m->signature = signature;
	m->address = address;
	m->m_firstCaller = m_firstCaller;
	m->m_firstCallerAddr = m_firstCallerAddr;
	m->proven = proven;
	m->callerSet = callerSet;
	m->cluster = cluster;

//	signature->takeMemo(mId);
//	for (std::set<Exp*, lessExpStar>::iterator it = proven.begin(); it != proven.end(); it++)
//		(*it)->takeMemo(mId);

	return m;
}

void LibProc::readMemo(Memo *mm, bool dec)
{
	LibProcMemo *m = dynamic_cast<LibProcMemo*>(mm);
	visited = m->visited;
	prog = m->prog;
	signature = m->signature;
	address = m->address;
	m_firstCaller = m->m_firstCaller;
	m_firstCallerAddr = m->m_firstCallerAddr;
	proven = m->proven;
	callerSet = m->callerSet;
	cluster = m->cluster;

//	signature->restoreMemo(m->mId, dec);
//	for (std::set<Exp*, lessExpStar>::iterator it = proven.begin(); it != proven.end(); it++)
//		(*it)->restoreMemo(m->mId, dec);
}

class UserProcMemo : public Memo {
public:
	UserProcMemo(int mId) : Memo(mId) { }

	bool visited;
	Prog *prog;
	Signature *signature;						// r
	ADDRESS address;
	Proc *m_firstCaller;
	ADDRESS m_firstCallerAddr;
	std::set<Exp*, lessExpStar> proven;			// r
	std::set<CallStatement*> callerSet;
	Cluster *cluster;

	Cfg* cfg;
	ProcStatus status;
	std::map<std::string, Type*> locals;		// r
	UserProc::SymbolMapType symbolMap;			// r
	std::list<Proc*> calleeList;
};

Memo *UserProc::makeMemo(int mId)
{
	UserProcMemo *m = new UserProcMemo(mId);
	m->visited = visited;
	m->prog = prog;
	m->signature = signature;
	m->address = address;
	m->m_firstCaller = m_firstCaller;
	m->m_firstCallerAddr = m_firstCallerAddr;
	m->proven = proven;
	m->callerSet = callerSet;
	m->cluster = cluster;

	m->cfg = cfg;
	m->status = status;
	m->locals = locals;
	m->symbolMap = symbolMap;
	m->calleeList = calleeList;

	signature->takeMemo(mId);
	for (std::set<Exp*, lessExpStar>::iterator it = proven.begin(); it != proven.end(); it++)
		(*it)->takeMemo(mId);

	for (std::map<std::string, Type*>::iterator it = locals.begin(); it != locals.end(); it++)
		(*it).second->takeMemo(mId);

	for (SymbolMapType::iterator it = symbolMap.begin(); it != symbolMap.end(); it++) {
		(*it).first->takeMemo(mId);
		(*it).second->takeMemo(mId);
	}

	return m;
}

void UserProc::readMemo(Memo *mm, bool dec)
{
	UserProcMemo *m = dynamic_cast<UserProcMemo*>(mm);
	visited = m->visited;
	prog = m->prog;
	signature = m->signature;
	address = m->address;
	m_firstCaller = m->m_firstCaller;
	m_firstCallerAddr = m->m_firstCallerAddr;
	proven = m->proven;
	callerSet = m->callerSet;
	cluster = m->cluster;

	cfg = m->cfg;
	status = m->status;
	locals = m->locals;
	symbolMap = m->symbolMap;
	calleeList = m->calleeList;

	signature->restoreMemo(m->mId, dec);
	for (std::set<Exp*, lessExpStar>::iterator it = proven.begin(); it != proven.end(); it++)
		(*it)->restoreMemo(m->mId, dec);

	for (std::map<std::string, Type*>::iterator it = locals.begin(); it != locals.end(); it++)
		(*it).second->restoreMemo(m->mId, dec);

	for (SymbolMapType::iterator it = symbolMap.begin(); it != symbolMap.end(); it++) {
		(*it).first->restoreMemo(m->mId, dec);
		(*it).second->restoreMemo(m->mId, dec);
	}
}

// Before Type Analysis, refs like r28{0} have a NULL Statement pointer. After this, they will point to an
// implicit assignment for the location. Thus, during and after type analysis, you can find the type of any
// location by following the reference to the definition
// Note: you need something recursive to make sure that child subexpressions are processed before parents
// Example: m[r28{0} - 12]{0} could end up adding an implicit assignment for r28{0} with a null reference!
void UserProc::addImplicitAssigns() {
	StatementList stmts;
	getStatements(stmts);
	StatementList::iterator it;
	ImplicitConverter ic(cfg);
	StmtImplicitConverter sm(&ic, cfg);
	for (it = stmts.begin(); it != stmts.end(); it++)
		(*it)->accept(&sm);
}

char* UserProc::lookup(Exp* e) {
	SymbolMapType::iterator it;
	it = symbolMap.find(e);
	if (it == symbolMap.end())
		return NULL;
	Exp* sym = it->second;
	assert(sym->isLocal() || sym->isParam());
	return ((Const*)((Location*)sym)->getSubExp1())->getStr();
}

void UserProc::symbolMapToLog() {
	SymbolMapType::iterator it;
	for (it = symbolMap.begin(); it != symbolMap.end(); it++)
		LOG << "  " << it->first << " maps to " << it->second << "\n";
}

void UserProc::dumpSymbolMap() {
	SymbolMapType::iterator it;
	for (it = symbolMap.begin(); it != symbolMap.end(); it++)
		std::cerr << "  " << it->first << " maps to " << it->second << "\n";
}

void UserProc::updateArguments() {
	StatementList stmts;
	getStatements(stmts);
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		CallStatement* call = dynamic_cast<CallStatement*>(*it);
		if (call == NULL) continue;
		call->updateArguments();
	}
}

void UserProc::updateCallDefines() {
	StatementList stmts;
	getStatements(stmts);
	StatementList::iterator it;
	for (it = stmts.begin(); it != stmts.end(); it++) {
		CallStatement* call = dynamic_cast<CallStatement*>(*it);
		if (call == NULL) continue;
		call->updateDefines();
	}
}

// Update the parameters, in case the signature and hence ordering and filtering has changed, or the locations in the
// collector have changed
void UserProc::insertParameter(Exp* e) {

	if (filterParams(e))
		return;						// Filtered out
	if (isPreserved(e))
		return;						// Also filter out preserveds (filterParams doesn't do it, since we don't want to
									// filter out preserveds for arguments)
			
	// Wrap it in an implicit assignment; DFA based TA should update the type later
	ImplicitAssign* as = new ImplicitAssign(e);
	// Insert as, in order, into the existing set of parameters
	StatementList::iterator nn;
	bool inserted = false;
	for (nn = parameters.begin(); nn != parameters.end(); ++nn) {
		// If the new assignment is less than the current one ...
		if (signature->argumentCompare(*as, *(Assign*)*nn)) {
			nn = parameters.insert(nn, as);		// ... then insert before this position
			inserted = true;
			break;
		}
	}
	if (!inserted)
		parameters.insert(parameters.end(), as);	// In case larger than all existing elements
}


// Filter out locations not possible as return locations. Return true to *remove* (filter *out*)
bool UserProc::filterReturns(Exp* e) {
	switch (e->getOper()) {
		if (isPreserved(e))
			return true;			// Don't keep preserved locations
		case opPC:	return true;
		case opTemp: return true;
		case opRegOf: {
			int sp;
			if (signature == NULL)
				sp = 999;
			else
				sp = signature->getStackRegister(prog);
			int r = ((Const*)((Location*)e)->getSubExp1())->getInt();
			return r == sp;
		}
		case opMemOf: {
			Exp* addr = ((Location*)e)->getSubExp1();
			if (addr->isIntConst())
				return true;			// Global memory location
			OPER op = addr->getOper();
			if (op == opPlus || op == opMinus) {
				// Check for local variable: m[sp +- K]
				int sp;
				if (signature == NULL)
					sp = 999;
				else
					sp = signature->getStackRegister(prog);
				Exp* s1 = ((Binary*)addr)->getSubExp1();
				if (s1->isSubscript())
					s1 = ((RefExp*)s1)->getSubExp1();
				if (!s1->isRegN(sp)) return false;
				Exp* s2 = ((Binary*)addr)->getSubExp2();
				if (!s2->isIntConst()) return false;
				// This will filter out sparc struct returns, at m[sp+64] (in the *parent's* stack)
				if (op == opPlus && ((Const*)s2)->getInt() == 64 && signature->getPlatform() == PLAT_SPARC)
					return false;
				return true;			// Don't allow local variables
			}
			return false;				// Might be some weird memory expression that is not a local
		}
		default:
			return false;
	}
	return false;
}

// Filter out locations not possible as parameters or arguments. Return true to remove
bool UserProc::filterParams(Exp* e) {
	switch (e->getOper()) {
		case opPC:	return true;
		case opTemp: return true;
		case opRegOf: {
			int sp = 999;
			if (signature) sp = signature->getStackRegister(prog);
			int r = ((Const*)((Location*)e)->getSubExp1())->getInt();
			return r == sp;
		}
		case opMemOf: {
			Exp* addr = ((Location*)e)->getSubExp1();
			if (addr->isIntConst())
				return true;			// Global memory location
			if (addr->isSubscript() && ((RefExp*)addr)->isImplicitDef()) {
				Exp* reg = ((RefExp*)addr)->getSubExp1();
				int sp = 999;
				if (signature) sp = signature->getStackRegister(prog);
				if (reg->isRegN(sp))
					return true;		// Filter out m[sp{-}] assuming it is the return address
			}
			return false;				// Might be some weird memory expression that is not a local
		}
		default:
			return false;
	}
	return false;
}

char* UserProc::findLocal(Exp* e) {
	if (e->isLocal())
		return ((Const*)((Unary*)e)->getSubExp1())->getStr();
	// Look it up in the symbol map
	char* name = lookup(e);
	if (name == NULL)
		return NULL;
	// Now make sure it is a local; some symbols (e.g. parameters) are in the symbol map but not locals
	std::string str(name);
	if (locals.find(str) != locals.end())
		return name;
	return NULL;
}

