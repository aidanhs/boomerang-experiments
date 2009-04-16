#ifndef __LOADER_INTERFACE_H__
#define __LOADER_INTERFACE_H__

struct ILoader
{
		enum LOAD_FMT {LOADFMT_ELF, LOADFMT_PE, LOADFMT_PALM, LOADFMT_PAR, LOADFMT_EXE, LOADFMT_MACHO, LOADFMT_LX, LOADFMT_COFF};
		enum MACHINE {MACHINE_PENTIUM, MACHINE_SPARC, MACHINE_HPRISC, MACHINE_PALM, MACHINE_PPC, MACHINE_ST20, MACHINE_MIPS};

virtual bool		GetEntryPoints(const char* pEntry = "main") = 0;

//////////////////////////////////////////////////////////////////////////
// Parts of the interface that need thought
//    
// **** virtual std::list<const char *> getDependencyList();  ****
// Maybe convert to collectDependencies, and that would use registered boomerang services to inform
// the rest of engine about dependencies of the given binary.

};


#endif
