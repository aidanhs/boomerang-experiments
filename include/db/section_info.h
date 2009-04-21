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

/* File: section_info.h
 * Desc: This file contains the definition of the abstract class BinaryFile
*/

#ifndef __SECTION_INFO_H__
#define __SECTION_INFO_H__

/*==============================================================================
 * Dependencies.
 *============================================================================*/

#include "types.h"
#include <string>

// SectionInfo structure. GetSectionInfo returns a pointer to an array of
// these structs. All information about the sections is contained in these
// structures.

struct SectionInfo {
    SectionInfo();		// Constructor
    virtual		~SectionInfo();		// Quell a warning in gcc

    // Windows's PE file sections can contain any combination of code, data and bss.
    // As such, it can't be correctly described by SectionInfo, why we need to override
    // the behaviour of (at least) the question "Is this address in BSS".
    virtual bool isAddressBss(ADDRESS a) const {
        return bBss != 0;
    }

    char*		pSectionName;		// Name of section
    ADDRESS		uNativeAddr;		// Logical or native load address
    ADDRESS		uHostAddr;			// Host or actual address of data
    ADDRESS		uSectionSize;		// Size of section in bytes
    ADDRESS		uSectionEntrySize;	// Size of one section entry (if applic)
    unsigned	uType;				// Type of section (format dependent)
	unsigned	bCode:1;			// Set if section contains instructions
	unsigned	bData:1;			// Set if section contains data
	unsigned	bBss:1;				// Set if section is BSS (allocated only)
	unsigned	bReadOnly:1;		// Set if this is a read only section
};

//typedef struct SectionInfo* PSectionInfo;

#endif		// #ifndef __BINARYFILE_H__
