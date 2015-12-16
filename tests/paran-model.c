#include <stdio.h>

int main() {
	int a = ((1-2)-3+5*9);
	int b = (((a - 2)));
	int c = ((0) - b);
	
	printf("%d\n", a);
	printf("%d\n", b);
	printf("%d\n", c);
}
