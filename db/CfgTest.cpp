/*==============================================================================
 * FILE:	   CfgTest.cc
 * OVERVIEW:   Provides the implementation for the CfgTest class, which
 *				tests the Exp and derived classes
 *============================================================================*/
/*
 * $Revision: 1.14.2.1 $
 *
 * 17 Jul 03 - Mike: Created
 */

#include "CfgTest.h"
#include <sstream>
#include <string>
#include "BinaryFile.h"
#include "frontend.h"
#include "proc.h"
#include "prog.h"
#include "dataflow.h"

/*==============================================================================
 * FUNCTION:		CfgTest::registerTests
 * OVERVIEW:		Register the test functions in the given suite
 * PARAMETERS:		Pointer to the test suite
 * RETURNS:			<nothing>
 *============================================================================*/
#define MYTEST(name) \
suite->addTest(new CppUnit::TestCaller<CfgTest> ("CfgTest", \
	&CfgTest::name, *this))

void CfgTest::registerTests(CppUnit::TestSuite* suite) {
	// Oops - they were all for dataflow. Need some real Cfg tests!
}

int CfgTest::countTestCases () const
{ return 2; }	// ? What's this for?

/*==============================================================================
 * FUNCTION:		CfgTest::setUp
 * OVERVIEW:		Set up some expressions for use with all the tests
 * NOTE:			Called before any tests
 * PARAMETERS:		<none>
 * RETURNS:			<nothing>
 *============================================================================*/
void CfgTest::setUp () {
	//prog.setName("default name");
}

/*==============================================================================
 * FUNCTION:		CfgTest::tearDown
 * OVERVIEW:		Delete expressions created in setUp
 * NOTE:			Called after all tests
 * PARAMETERS:		<none>
 * RETURNS:			<nothing>
 *============================================================================*/
void CfgTest::tearDown () {
}

