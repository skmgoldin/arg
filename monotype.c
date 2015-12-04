#include <stdlib.h>
#include <stdio.h>

struct monotype {
  int isint;
  int i;

  int ischar;
  char *s;

  int isbool;
  int b;

  int isdouble;
  double d;

  int isarray;
  struct monotype* a;
  int a_len;
};

struct monotype new_monotype(int flag, int i, char *s, int b, double d, struct monotype* a, int a_len)
{
	struct monotype nm;

	if(flag == 0) {
		nm.i = i;

		nm.isint = 1;
		nm.ischar = 0;
		nm.isbool = 0;
		nm.isdouble = 0;
    nm.isarray = 0;
    nm.a_len = 0;
	} else if(flag == 1) {
		nm.s = s;

		nm.isint = 0;
		nm.ischar = 1;
		nm.isbool = 0;
		nm.isdouble = 0;
    nm.isarray = 0;
    nm.a_len = 0;
	} else if(flag == 2) {
		nm.b = b;

		nm.isint = 0;
		nm.ischar = 0;
		nm.isbool = 1;
		nm.isdouble = 0;
    nm.isarray = 0;
    nm.a_len = 0;
	} else if(flag == 3) {
		nm.d = d;

		nm.isint = 0;
		nm.ischar = 0;
		nm.isbool = 0;
		nm.isdouble = 1;
    nm.isarray = 0;
    nm.a_len = 0;
	} else if(flag == 4) {
    nm.a = a;

    nm.isint = 0;
    nm.ischar = 0;
    nm.isbool = 0;
    nm.isdouble = 0;
    nm.isarray = 1;
    nm.a_len = a_len;
  }

	return nm;
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
}
