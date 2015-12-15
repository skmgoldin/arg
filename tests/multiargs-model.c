#include <stdio.h>

static void multi(int a,int b,int c,int d,int e,int f) {
	printf("%d\n", a);
	printf("%d\n", b);
	printf("%d\n", c);
	printf("%d\n", d);
	printf("%d\n", e);
	printf("%d\n", f);
}

int main() {
	multi(1,2,3,4,5,6);
	multi(2,2,2,2,2,2);
	multi(77,3,2,62,2,2);
	multi(23,21,22,32,2,92);
	return 0;
}
