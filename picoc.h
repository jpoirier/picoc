/* picoc external interface. This should be the only header you need to use if
 * you're using picoc as a library. Internal details are in interpreter.h */
#ifndef PICOC_H
#define PICOC_H


/* picoc version number */
#ifdef VER
/* VER, the git hash number, and TAG are obtained via the Makefile */
#define PICOC_VERSION TAG " r" VER
#else
#define PICOC_VERSION "v2.3.2"
#endif

#include "interpreter.h"

/* this has to be a macro, otherwise errors will occur due to
	the stack being corrupt */
#define PicocPlatformSetExitPoint(pc) setjmp((pc)->PicocExitBuf)


/* parse.c */
extern void PicocParse(Picoc *pc, const char *FileName, const char *Source,
	int SourceLen, int RunIt, int CleanupNow, int CleanupSource, int EnableDebugger);
extern void PicocParseInteractive(Picoc *pc);

/* platform.c */
extern void PicocCallMain(Picoc *pc, int argc, char **argv);
extern void PicocInitialize(Picoc *pc, int StackSize);
extern void PicocCleanup(Picoc *pc);
extern void PicocPlatformScanFile(Picoc *pc, const char *FileName);

/* include.c */
extern void PicocIncludeAllSystemHeaders(Picoc *pc);

#endif /* PICOC_H */
