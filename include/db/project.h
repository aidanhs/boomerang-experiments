/*
 * Copyright (C) 1997-2001, The University of Queensland
 * Copyright (C) 2001, Sun Microsystems, Inc
 * Copyright (C) 2002, Trent Waddington
 *
 * See the file "LICENSE.TERMS" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 *
 */

/*==============================================================================
 * Dependencies.
 *============================================================================*/
/* File: pproject.h
 * Desc: This file contains the 'root of the abstract class BinaryFile
*/

#ifndef __PROJECT_H__
#define __PROJECT_H__
#include "types.h"
#include "db/section_info.h"
class Stage0
{
	SectionInfo *m_pSections;				// The section info
public:
	// Lookup the address, return the name, or 0 if not found
	const char* SymbolByAddress(ADDRESS uNative);
	// Lookup the name, return the address. If not found, return NO_ADDRESS
	ADDRESS		GetAddressByName(const char* pName, bool bNoTypeOK = false);
	void		AddSymbol(ADDRESS uNative, const char *pName);

	// Section functions
	int				GetNumSections() const;		// Return number of sections
	SectionInfo *	GetSectionInfo(int idx) const; // Return section struct
	// Find section info given name, or 0 if not found
	SectionInfo *	GetSectionInfoByName(const char* sName);
	// Find the end of a section, given an address in the section
	SectionInfo *	GetSectionInfoByAddr(ADDRESS uEntry) const;


};
class Project
{
public:
	Stage0 *get_stage0();
};
#endif