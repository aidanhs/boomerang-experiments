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

#include "db/section_info.h"

// This struct used to be initialised with a memset, but now that overwrites the virtual table (if compiled under gcc
// and possibly others)
SectionInfo::SectionInfo() :
        pSectionName(NULL), uNativeAddr(0), uHostAddr(0), uSectionSize(0), uSectionEntrySize(0), uType(0),
        bCode(false), bData(false), bBss(0), bReadOnly(0) {}

SectionInfo::~SectionInfo() {}
