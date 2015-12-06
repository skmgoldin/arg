#include <stdlib.h>
#include <stdio.h>

struct monotype {
  int isint;
  int i;

  int ischar;
  char *s;

  int isbool;
  int b;

  int isfloat;
  float f;

  int isarray;
  struct monotype* a;
  int a_len;
};

struct monotype new_monotype(int flag, int i, char *s, int b, float f, struct monotype* a, int a_len)
{
	struct monotype nm;

	if(flag == 0) {
		nm.i = i;

        nm.isint = 1;
		nm.ischar = 0;
		nm.isbool = 0;
		nm.isfloat = 0;
        nm.isarray = 0;
        nm.a_len = 0;
	} else if(flag == 1) {
		nm.s = s;

		nm.isint = 0;
		nm.ischar = 1;
		nm.isbool = 0;
		nm.isfloat = 0;
		nm.isarray = 0;
		nm.a_len = 0;
	} else if(flag == 2) {
		nm.b = b;

		nm.isint = 0;
		nm.ischar = 0;
		nm.isbool = 1;
		nm.isfloat = 0;
		nm.isarray = 0;
		nm.a_len = 0;
	} else if(flag == 3) {
		nm.f = f;

		nm.isint = 0;
		nm.ischar = 0;
		nm.isbool = 0;
		nm.isfloat = 1;
		nm.isarray = 0;
		nm.a_len = 0;
	} else if(flag == 4) {
        nm.a = malloc(sizeof(struct monotype) * a_len);
        nm.a = a;

        nm.isint = 0;
        nm.ischar = 0;
        nm.isbool = 0;
        nm.isfloat = 0;
        nm.isarray = 1;
        nm.a_len = a_len;
  }

	return nm;
}

void error(char *message) {
  printf("%s", message);
  printf("\n");
  exit(1);
}

struct monotype get_array_element(struct monotype a, int index) {
  if (a.isarray != 1) {
    printf("MONOTYPE IS NOT ARRAY!\n");
    exit(1);
  }

  return *(a.a + index);
}

struct monotype set_array_element(struct monotype a, int index, struct monotype element) {
  if (a.isarray != 1) {
    printf("MONOTYPE IS NOT ARRAY!\n");
    exit(1);
  }

  *(a.a + index) = element;
	return a;//fixing return warning, not sure if returning correct thing
}

struct monotype monotype_add(struct monotype a, struct monotype b) {
  if (a.isint && b.isint) {
    return new_monotype(0, a.i + b.i, "", 0, 0.0, NULL, 0);
  } else if (a.ischar && b.ischar) {
    error("can't add strings");
  } else if (a.isbool && b.isbool) {
    error("can't add bools");
  } else if (a.isfloat && b.isfloat) {
    return new_monotype(3, 0, "", 0, a.f + b.f, NULL, 0);
  } else if (a.isarray && b.isarray) {
    error("can't add arrays");
  } else {
    error("can't add vars of different types");
  }
}

struct monotype monotype_sub(struct monotype a, struct monotype b) {
  if (a.isint && b.isint) {
    return new_monotype(0, a.i - b.i, "", 0, 0.0, NULL, 0);
  } else if (a.ischar && b.ischar) {
    error("can't subtract strings");
  } else if (a.isbool && b.isbool) {
    error("can't subtract bools");
  } else if (a.isfloat && b.isfloat) {
    return new_monotype(3, 0, "", 0, a.f - b.f, NULL, 0);
  } else if (a.isarray && b.isarray) {
    error("can't subtract arrays");
  } else {
    error("can't subtract vars of different types");
  }
}

struct monotype monotype_mult(struct monotype a, struct monotype b) {
  if (a.isint && b.isint) {
    return new_monotype(0, a.i * b.i, "", 0, 0.0, NULL, 0);
  } else if (a.ischar && b.ischar) {
    error("can't multiply strings");
  } else if (a.isbool && b.isbool) {
    error("can't multiply bools");
  } else if (a.isfloat && b.isfloat) {
    return new_monotype(3, 0, "", 0, a.f * b.f, NULL, 0);
  } else if (a.isarray && b.isarray) {
    error("can't multiply arrays");
  } else {
    error("can't add vars of different types");
  }
}

struct monotype monotype_div(struct monotype a, struct monotype b) {
  if (a.isint && b.isint) {
    return new_monotype(0, a.i / b.i, "", 0, 0.0, NULL, 0);
  } else if (a.ischar && b.ischar) {
    error("can't divide strings");
  } else if (a.isbool && b.isbool) {
    error("can't divide bools");
  } else if (a.isfloat && b.isfloat) {
    return new_monotype(3, 0, "", 0, a.f / b.f, NULL, 0);
  } else if (a.isarray && b.isarray) {
    error("can't divide arrays");
  } else {
    error("can't add vars of different types");
  }
}

struct monotype monotype_equal(struct monotype a, struct monotype b) {
  if (a.isint && b.isint) {
    return new_monotype(2, 0, "", a.i == b.i, 0.0, NULL, 0);
  } else if (a.ischar && b.ischar) {
    return new_monotype(2, 0, "", strcmp(a.s, b.s) == 0, 0.0, NULL, 0);
  } else if (a.isbool && b.isbool) {
    return new_monotype(2, 0, "", a.b == b.b, 0.0, NULL, 0);
  } else if (a.isfloat && b.isfloat) {
    return new_monotype(2, 0, "", a.f == b.f, 0.0, NULL, 0);
  } else if (a.isarray && b.isarray) {
    error("can't == arrays");
  } else {
    error("can't == vars of different types");
  }
}

struct monotype monotype_neq(struct monotype a, struct monotype b) {
  if (a.isint && b.isint) {
    return new_monotype(2, 0, "", a.i != b.i, 0.0, NULL, 0);
  } else if (a.ischar && b.ischar) {
    return new_monotype(2, 0, "", strcmp(a.s, b.s) != 0, 0.0, NULL, 0);
  } else if (a.isbool && b.isbool) {
    return new_monotype(2, 0, "", a.b != b.b, 0.0, NULL, 0);
  } else if (a.isfloat && b.isfloat) {
    return new_monotype(2, 0, "", a.f != b.f, 0.0, NULL, 0);
  } else if (a.isarray && b.isarray) {
    error("can't != arrays");
  } else {
    error("can't != vars of different types");
  }
}

struct monotype monotype_less(struct monotype a, struct monotype b) {
  if (a.isint && b.isint) {
    return new_monotype(2, 0, "", a.i < b.i, 0.0, NULL, 0);
  } else if (a.ischar && b.ischar) {
    error("can't < strings");
  } else if (a.isbool && b.isbool) {
    error("can't < bools");
  } else if (a.isfloat && b.isfloat) {
    return new_monotype(2, 0, "", a.f < b.f, 0.0, NULL, 0);
  } else if (a.isarray && b.isarray) {
    error("can't < arrays");
  } else {
    error("can't < vars of different types");
  }
}

struct monotype monotype_greater(struct monotype a, struct monotype b) {
  if (a.isint && b.isint) {
    return new_monotype(2, 0, "", a.i > b.i, 0.0, NULL, 0);
  } else if (a.ischar && b.ischar) {
    error("can't > strings");
  } else if (a.isbool && b.isbool) {
    error("can't > bools");
  } else if (a.isfloat && b.isfloat) {
    return new_monotype(2, 0, "", a.f > b.f, 0.0, NULL, 0);
  } else if (a.isarray && b.isarray) {
    error("can't > arrays");
  } else {
    error("can't > vars of different types");
  }
}

struct monotype monotype_leq(struct monotype a, struct monotype b) {
  if (a.isint && b.isint) {
    return new_monotype(2, 0, "", a.i <= b.i, 0.0, NULL, 0);
  } else if (a.ischar && b.ischar) {
    error("can't <= strings");
  } else if (a.isbool && b.isbool) {
    error("can't <= bools");
  } else if (a.isfloat && b.isfloat) {
    return new_monotype(2, 0, "", a.f <= b.f, 0.0, NULL, 0);
  } else if (a.isarray && b.isarray) {
    error("can't <= arrays");
  } else {
    error("can't <= vars of different types");
  }
}

struct monotype monotype_geq(struct monotype a, struct monotype b) {
  if (a.isint && b.isint) {
    return new_monotype(2, 0, "", a.i >= b.i, 0.0, NULL, 0);
  } else if (a.ischar && b.ischar) {
    error("can't >= strings");
  } else if (a.isbool && b.isbool) {
    error("can't >= bools");
  } else if (a.isfloat && b.isfloat) {
    return new_monotype(2, 0, "", a.f >= b.f, 0.0, NULL, 0);
  } else if (a.isarray && b.isarray) {
    error("can't >= arrays");
  } else {
    error("can't >= vars of different types");
  }
}
