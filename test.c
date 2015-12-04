#include "monotype.c"
#include <stdlib.h>
#include <stdio.h>

struct monotype rando(struct monotype a, struct monotype b, struct monotype c) {

	  if(!a.isint)
		exit(1);

	  if(a.i == 5) {
	    if(b.isint) {
	      return new_monotype(0, b.i, "", 0, 0.0, NULL);
	    } else if(b.ischar) {
	      return new_monotype(1, 0, b.s, 0, 0.0, NULL);
	    } else if(b.isbool) {
	      return new_monotype(2, 0, "", b.b, 0.0, NULL);
	    } else if(b.isdouble) {
	      return new_monotype(3, 0, "", 0, b.d, NULL);
	    }
	  } else {
	    if(c.isint) {
	      return new_monotype(0, c.i, "", 0, 0.0, NULL);
	    } else if(c.ischar) {
	      return new_monotype(1, 0, c.s, 0, 0.0, NULL);
	    } else if(c.isbool) {
	      return new_monotype(2, 0, "", c.b, 0.0, NULL);
	    } else if(c.isdouble) {
	      return new_monotype(3, 0, "", 0, c.d, NULL);
	    }
	}
}

void monotype_printer(struct monotype m)
{
	if(m.isint) {
		printf("%d\n", m.i);
		return;
	} else if(m.ischar) {
		printf("%s\n", m.s);
		return;
	} else if(m.isbool) {
		printf("%d\n", m.b);
		return;
	} else if(m.isdouble) {
		printf("%f\n", m.d);
		return;
	} else if(m.isarray) {
		printf("array");
		return;
	}

}

int main(int argc, char **argv)
{
	struct monotype a = new_monotype(0, atoi(argv[1]), "", 0, 0.0, NULL);
	struct monotype b = new_monotype(1, 0, "yo", 0, 0.0, NULL);
	struct monotype c = new_monotype(0, 420, "", 0, 0.0, NULL);

	struct monotype d = rando(a, b, c);

	struct monotype e = rando(d, b, c);

	monotype_printer(e);
}
