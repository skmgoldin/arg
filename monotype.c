struct monotype {
  int isint;
  int i;

  int ischar;
  char *s;

  int isbool;
  int b;
  
  int isdouble;
  double d;
}

/* ARG */
// a = 10;
// b = "hi";

static int main(int argc, char **argv)
{
	/* An integer */
	monotype a = new_monotype(0, 10, "", 0, 0.0);
	monotype b = new_monotype(1, 0, "hi", 0, 0.0);
}

monotype new_monotype(int flag, int i, char *s, int b, double d)
{
	monotype nm;

	if(flag == 0) {
		nm.i = i;
		
		nm.isint = 1;
		nm.ischar = 0;
		nm.isbool = 0;
		nm.isdouble = 0;
	} else if(flag == 1) {
		nm.s = s;

		nm.isint = 0;
		nm.ischar = 1;
		nm.isbool = 0;
		nm.isdouble = 0;
	} else if(flag == 2) {
		nm.b = b;

		nm.isint = 0;
		nm.ischar = 0;
		nm.isbool = 1;
		nm.isdouble = 0;
	} else if(flag == 3) {
		nm.d = d;

		nm.isint = 0;
		nm.ischar = 0;
		nm.isbool = 0;
		nm.isdouble = 1;
	}

	return nm;
}
