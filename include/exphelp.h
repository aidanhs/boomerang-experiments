/*
 * A class for comparing Exp*s (comparing the actual expressions)
 * Type sensitive
 */

#ifndef __EXPHELP_H__
#define __EXPHELP_H__

#include	<map>

class Exp;
class Assignment;

// A helper file for comparing Exp*'s sensibly
class lessExpStar : public std::binary_function<Exp*, Exp*, bool> {
public:
	bool operator()(const Exp* x, const Exp* y) const;
};


/*
 * A class for comparing Exp*s (comparing the actual expressions)
 * Type insensitive
 */
class lessTI : public std::binary_function<Exp*, Exp*, bool> {
public:
	bool operator()(const Exp* x, const Exp* y) const;
};

// Compare assignments by their left hand sides (only). Implemented in statement.cpp
class lessAssignment : public std::binary_function<Assignment*, Assignment*, bool> {
public:
	bool operator()(const Assignment* x, const Assignment* y) const;
};

// A type for an "interference graph". Needed by various classes to implement the transforation out of SSA form.
typedef std::map<Exp*, Exp*, lessExpStar> igraph;

#endif		// __EXPHELP_H__
