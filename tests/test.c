#include "monotype.c"
#include <stdlib.h>
#include <stdio.h>

struct monotype rando(struct monotype a, struct monotype b, struct monotype c) {

	  if(!a.isint)
		exit(1);

	  if(a.i == 5) {
	    if(b.isint) {
	      return new_monotype(0, b.i, "", 0, 0.0, NULL, 0);
	    } else if(b.ischar) {
	      return new_monotype(1, 0, b.s, 0, 0.0, NULL, 0);
	    } else if(b.isbool) {
	      return new_monotype(2, 0, "", b.b, 0.0, NULL, 0);
	    } else if(b.isfloat) {
	      return new_monotype(3, 0, "", 0, b.f, NULL, 0);
	    }
	  } else {
	    if(c.isint) {
	      return new_monotype(0, c.i, "", 0, 0.0, NULL, 0);
	    } else if(c.ischar) {
	      return new_monotype(1, 0, c.s, 0, 0.0, NULL, 0);
	    } else if(c.isbool) {
	      return new_monotype(2, 0, "", c.b, 0.0, NULL, 0);
	    } else if(c.isfloat) {
	      return new_monotype(3, 0, "", 0, c.f, NULL, 0);
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
	} else if(m.isfloat) {
		printf("%f\n", m.f);
		return;
	} else if(m.isarray) {
		printf("array");
		return;
	}

}

int main(int argc, char **argv)
{
	// struct monotype p[2] = {new_monotype(0, 1, "", 0, 0.0, NULL, 0), new_monotype(0, 2, "", 0, 0.0, NULL, 0)};
	struct monotype tmp[2] = {new_monotype(0, 1, "", 0, 0.0, NULL, 0), new_monotype(0, 2, "", 0, 0.0, NULL, 0)};
	struct monotype a = new_monotype(4, 0, "", 0, 0.0, tmp, 2);
	set_array_element(a, 0, new_monotype(0, 5, "", 0, 0.0, NULL, 0));

	struct monotype i = new_monotype(0, 20, "", 0, 0.0, NULL, 0);
	// set_array_element(i, 0, new_monotype(0, 40, "", 0, 0.0, NULL, 0));

	// monotype_printer(get_array_element(a, 0));

  int arrlen = 3;
  struct monotype *arr = malloc(sizeof(struct monotype) * arrlen);
  struct monotype stack_arr = new_monotype(4, 0, 0, 0, 0, arr, arrlen);

	struct monotype i1 = new_monotype(0, 1, "", 0, 0.0, NULL, 0);
	struct monotype i2 = new_monotype(0, 3, "", 0, 0.0, NULL, 0);
	struct monotype i3 = monotype_equal(i1, i2);

	monotype_printer(i3);

	// monotype_printer(e);
}
