#include "boomerang.h"
#define GC_DEBUG 1		// Set to debug the garbage collector
#include "gc.h"

// FIXME: surely not needed here now?
#ifdef WIN32
#include <direct.h>		// For Windows mkdir
#endif

#ifdef SPARC_DEBUG
#include <signal.h>

void segv_handler(int a, siginfo_t *b, void *c)
{
	fprintf(stderr, "Boomerang has encounted a fatal error.\n");

	ucontext_t *uc = (ucontext_t *) c;

	fprintf(stderr, "\napproximate stack trace:\n");
	for (int i = 0; i < 100; i++) {
		unsigned int *sp = (unsigned int*)uc->uc_mcontext.gregs[REG_O6];
		if (sp[i] - (unsigned int)(sp+i) < 100)
			fprintf(stderr, "%08X\n", sp[++i]);
	}
   exit(0);
}

int main(int argc, const char* argv[]) {
	struct sigaction act;
	memset(&act, 0, sizeof(struct sigaction));
	act.sa_sigaction = segv_handler;
	act.sa_flags = (SA_SIGINFO | SA_ONSTACK);

	sigaction(SIGSEGV, &act, NULL);
#else

int main(int argc, const char* argv[]) {

#endif
	return Boomerang::get()->commandLine(argc, argv);
}

// Straight from the garbage collector's example (see doc/gcinterface.html in the gc distribution):
inline void * operator new(size_t n) { return GC_malloc(n); }
inline void operator delete(void *) {}
inline void * operator new[](size_t n) { return GC_malloc(n); }
inline void operator delete[](void *) {}


