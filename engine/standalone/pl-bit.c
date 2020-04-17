/*  File:    pl-bit.c
    Purpose: bitmap operations
             where a bitmap is a prolog string
    Author:  Martin Reinders & Bert Bredeweg
    Date:    August 1989
    Part-of: GARP (version 1.0)

    N.B: also includes screen_size & compile with cc -c pl-bit.c

    Copyright (c) 1989, University of Amsterdam. All rights reserved.

*/

#ifdef __APPLE__
#define __unix__
#endif

#include <stdio.h>
#ifdef __unix__
#ifdef __sun__
#ifdef __svr4__				/* Solaris */
#define HAVE_TERMIOS_H
#endif /*__svr4__*/
#endif /*__sun__*/
#ifdef HAVE_TERM_H
#include <term.h>
#endif
#include <sys/ioctl.h>
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
#else /*__unix__*/
#include <console.h>
#endif /*__unix__*/
#include <string.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <stdlib.h>
#include <SWI-Prolog.h>
#include <assert.h>

#define MASK 0x80
#define EOS 0
#define	ISSET(v, i) (v[(i)/7] & (1 << ((i)%7)))
#define CLEAR(v, i) (v[(i)/7] &= ~(1 << ((i)%7)))
#define SET(v, i)   (v[(i)/7] |= (1 << ((i)%7)))
			   
#ifndef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#endif
#ifndef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#endif

static void
checkset(char *set)
{ int l = strlen(set);

  if ( l > 0 )
  { unsigned char *s = (unsigned char *)set+l;

    assert(s[-1] != MASK);
  }
}


static int
unify_set(term_t t, char *set)
{ checkset(set);

  return  PL_unify_string_chars(t, set);
}


/*
 *  we assume that when an input argument is a string,
 *  each char has its 7th bit set
 *  thus input should only be output from these same functions
 *  or a list for list_map
 */

static foreign_t
map_union(term_t mp1, term_t mp2, term_t mp3)
{ char *s1, *s2;  
  int l1, l2, i;

  if ( PL_get_string(mp1, &s1, &l1) &&
       PL_get_string(mp2, &s2, &l2) )
  { int l = max(l1, l2);
    char *buf = alloca(l+1);

    memcpy(buf, s1, l1);
    if ( l2 > l1 )
      memset(&buf[l1], MASK, l2-l1);
    buf[l] = EOS;

    for(i=0; i<l2; i++)
      buf[i] |= s2[i];

    return unify_set(mp3, buf);
  }

  return PL_warning("map_union/3: instantiation fault");
}


foreign_t
map_difference(term_t mp1, term_t mp2, term_t mp3)
{ char *s1, *s2;  
  int l1, l2, i;

  if ( PL_get_string(mp1, &s1, &l1) &&
       PL_get_string(mp2, &s2, &l2) )
  { int l = max(l1, l2);
    char *buf = alloca(l+1);

    memcpy(buf, s1, l1);
    if ( l2 > l1 )
      memset(&buf[l1], MASK, l2-l1);
    buf[l] = EOS;

    for(i=0; i<l2; i++)
    { buf[i] ^= s2[i];
      buf[i] |= MASK;			/* set high-bit */
    }

    return unify_set(mp3, buf);
  }

  return PL_warning("map_difference/3: instantiation fault");
}


/* replace i1 with i2 in m1
   fail if i1 not in m1 */

foreign_t
map_replace(term_t i1, term_t i2, term_t m1, term_t m2)
{ int i, j, l;
  char *s;

  if ( PL_get_integer(i1, &i) &&
       PL_get_integer(i2, &j) &&
       PL_get_string(m1, &s, &l) )
  { if ( i <= l*7 )
    { int len = max(l, 1+j/7);
      unsigned char *buf = alloca(len+1);
  
      strcpy(buf, s);
      memset(&buf[l], MASK, len-l);
      buf[len] = EOS;
  
      if ( ISSET(buf, i) )
      { int ti;
  
	CLEAR(buf, i);
	SET(buf, j);
  
	for(ti=len; ti>0 && buf[ti-1] == MASK; ti-- )
	  ;
	buf[ti] = EOS;
  
	return unify_set(m2, buf);
      }
    }

    PL_fail;
  }

  return PL_warning("map_replace/4: instantiation fault");
}


/* like union, but succeed only if intersection is empty */

static foreign_t
map_union_unique(term_t mp1, term_t mp2, term_t mp3)
{ char *s1, *s2;  
  int l1, l2, i;

  if ( PL_get_string(mp1, &s1, &l1) &&
       PL_get_string(mp2, &s2, &l2) )
  { int l = max(l1, l2);
    char *buf = alloca(l+1);

    memcpy(buf, s1, l1);
    if ( l2 > l1 )
      memset(&buf[l1], MASK, l2-l1);
    buf[l] = EOS;

    for(i=0; i<l2; i++)
    { if ( buf[i] & s2[i] & ~MASK & 0xff )
	PL_fail;
      buf[i] |= s2[i];
    }

    return unify_set(mp3, buf);
  }

  return PL_warning("map_union_unique/3: instantiation fault");
}


foreign_t
map_intersection(term_t mp1, term_t mp2, term_t mp3)
{ char *s1, *s2;  
  int l1, l2, i;

  if ( PL_get_string(mp1, &s1, &l1) &&
       PL_get_string(mp2, &s2, &l2) )
  { int l = min(l1,l2);
    unsigned char *buf = alloca(l+1);

    memcpy(buf, s1, l);

    for(i=0; i<l; i++)
    { buf[i] &= s2[i];
    }
    while(i>0 && buf[i-1] == MASK)
      i--;
    buf[i] = EOS;

    return unify_set(mp3, buf);
  }

  return PL_warning("map_intersection/3: instantiation fault");
}

/* remove intersection of both maps from both maps;
   fail if no intersection */

foreign_t
map_without_intersection(term_t mp1, term_t mp2, term_t mp3, term_t mp4)
{ char *s1, *s2;
  int l1, l2, i;

  if ( PL_get_string(mp1, &s1, &l1) &&
       PL_get_string(mp2, &s2, &l2) )
  { int	l = max(l1, l2);
    unsigned char *b1 = alloca(l+1);
    unsigned char *b2 = alloca(l+1);
    int has_intersection = 0;

    memcpy(b1, s1, l1); memset(&b1[l1], MASK, l-l1); b1[l] = EOS;
    memcpy(b2, s2, l2); memset(&b2[l2], MASK, l-l2); b2[l] = EOS;
   
    for(i=0; i<l; i++)
    { int m = b1[i] & b2[i] & ~MASK;

      has_intersection |= m;
      b1[i] ^= m;
      b2[i] ^= m;
    }

    if ( !has_intersection )
      PL_fail;
      
    for(i=l-1; i>=0 && b1[i] == MASK; i--)
      b1[i] = EOS;
    for(i=l-1; i>=0 && b2[i] == MASK; i--)
      b2[i] = EOS;
    
    return unify_set(mp3, b1) &&
           unify_set(mp4, b2);
  }

  return PL_warning("map_without_intersection/4: instantiation fault");
}


foreign_t
list_map(term_t list, term_t mp)
{ int amax      = -1;
  int allocsize = 256;
  char *buf = alloca(allocsize);
  term_t a = PL_new_term_ref();
  term_t lst = PL_copy_term_ref(list);

  buf[0] = EOS;

  while(PL_get_list(lst, a, lst))
  { int i, am;

    if ( !PL_get_integer(a, &i) || i < 0 )
      PL_warning("list_map/2: bad element in set");

    if ( (am=i/7) > amax )
    { while ( am > allocsize-1 )
      { char *b2 = alloca(allocsize*2);
	memcpy(b2, buf, allocsize);
	allocsize *= 2;
	buf = b2;
      }
      memset(&buf[amax+1], MASK, am-amax);
      buf[am+1] = EOS;

      amax = am;
    }

    SET(buf, i);
  }

  if ( !PL_get_nil(lst) )
    return PL_warning("list_map/2: instantiation fault");

  return unify_set(mp, buf);
}
  

foreign_t
map_list(term_t mp, term_t lst)
{ char *s;
  int len;

  if ( PL_get_string(mp, &s, &len) )
  { term_t l = PL_copy_term_ref(lst);
    term_t a = PL_new_term_ref();
    int i, j = 0;

    for(i=0; i<len; i++)
    { int n;

      for (n=0; n<7; n++, j++)
      { if ( s[i] & (1<<n) )
	{ if ( !PL_unify_list(l, a, l) || !PL_unify_integer(a, j) )
	    PL_fail;
	}
      }
    }

    return PL_unify_nil(l);
  }

  return PL_warning("map_list/2: instantiation fault");
}


foreign_t
map_count(term_t mp, term_t cnt)
{ char *s;
  int l;

  if ( PL_get_string(mp, &s, &l) )
  { int i, c = 0;

    for(i=0; i<l; i++)
    { int n;

      for (n=0; n<7; n++)
      { if ( s[i] & (1<<n) )
	  c++;
      }
    }

    return PL_unify_integer(cnt, c);
  }

  return PL_warning("map_count/2: instantiation fault");
}


foreign_t
window_size(term_t r, term_t c)
{ int rows, cols;

#ifdef __unix__
  int iorval;

#ifdef TIOCGSIZE
  struct ttysize ws;
  iorval = ioctl(0, TIOCGSIZE, &ws);
	
  rows = ws.ts_lines;
  cols = ws.ts_cols;
#else
#ifdef TIOCGWINSZ
  struct winsize ws;
  iorval = ioctl(0, TIOCGWINSZ, &ws);

  rows = ws.ws_row;
  cols = ws.ws_col;
#else
  return PL_warning("window_size/2: don't know how to get window size");
#endif
#endif

  if ( iorval != 0 )
    return PL_warning("window_size/2: ioctl() failed");
#else /*__unix__*/
  rows = ScreenRows();
  cols = ScreenCols();
#endif /*__unix__*/

  return PL_unify_integer(r, rows) &&
	 PL_unify_integer(c, cols);
}


static PL_extension predicates[] = {
  { "list_map",                 2, list_map,                 0 },
  { "map_list",                 2, map_list,                 0 },
  { "map_union",                3, map_union,                0 },
  { "map_union_unique",         3, map_union_unique,         0 },
  { "map_intersection",         3, map_intersection,         0 },
  { "map_without_intersection", 4, map_without_intersection, 0 },
  { "map_count",                2, map_count,                0 },
  { "map_replace",              4, map_replace,              0 },
  { "map_difference",           3, map_difference,           0 },
  { "window_size",              2, window_size,              0 },
  { NULL,			0, NULL,		     0 }
};

install_t
init_bits()
{ PL_register_extensions(predicates);
}


#ifdef __SWI_EMBEDDED__
#define READLINE 1			/* use readline interface */

#ifdef READLINE
static void
install_readline(int argc, char**argv)
{ PL_install_readline();
}
#endif


main(int argc, char **argv)
{ 
#ifdef READLINE
  PL_initialise_hook(install_readline);
#endif

  init_bits();
  if ( !PL_initialise(argc, argv) )
    PL_halt(1);


  PL_halt(PL_toplevel() ? 0 : 1);

  return 0;
}

#endif
