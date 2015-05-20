#ifndef __PLATFORM_H
#define __PLATFORM_H

/**************************************************************************
  Hlavickovy soubor pro platform-dependent definice.
  (soucast programu TREE)

    Petr Kadlec (cviceni z OSY ve ctvrtek od 11:00)
 **************************************************************************/

/* ---- "detekce" platformy, na ktere se preklada ---- */
#ifdef __BORLANDC__
#define PLATFORM_DOS
#endif

#if (!defined(PLATFORM_DOS))
#define PLATFORM_UNIX
#endif

#if (defined(PLATFORM_DOS) && defined(PLATFORM_UNIX))
#error Unable to decide the target platform
#endif
/* --------------------------------------------------- */


#include <sys/stat.h>
#include <limits.h>

/* maximalni mozna delka cesty */
#if (!defined(PATH_MAX))
#define PATH_MAX 512
#endif

/* maximalni uroven rekurze, ktera je mozna */
#define MAXRECURSION (PATH_MAX/2)

/* oddelovac cesty a optionu */
#if (defined(PLATFORM_DOS))
#define PATH_CHAR '\\'
#define PATH_STR  "\\"
#define OPTION_CHAR '/'
#define OPTION_STR "/"
#elif (defined(PLATFORM_UNIX))
#define PATH_CHAR '/'
#define PATH_STR  "/"
#define OPTIONCHAR '-'
#define OPTION_STR "-"
#endif

/* funkce pro zjisteni informaci o souboru */
#ifdef PLATFORM_UNIX
#define get_stat(path,buf) lstat((path),(buf))
#elif (defined(PLATFORM_DOS))
#define get_stat(path,buf) stat((path),(buf))
#endif

#ifdef PLATFORM_DOS
#include <dos.h>
#endif

/* Pro non-DOS platformy se nepouziva case-insensitive strcmp */
#ifndef PLATFORM_DOS
#define stricmp(s1,s2) strcmp(s1,s2)
#endif

/* kontrola pristupovych prav */
#if (defined(PLATFORM_DOS))
/* vzdy vraci nulu */
#define check_access(path) ((path) != (path))
#elif (defined(PLATFORM_UNIX))
#include <unistd.h>
#define check_access(path) access((path), R_OK | X_OK)
#endif

#endif /* __PLATFORM_H */