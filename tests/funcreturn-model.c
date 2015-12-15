#include <stdio.h> 

static int square(int val) {
	return val * val;
}

int main() {
	int a = 5;
	int b = square(a);
	printf("the square of 5 is %d\n", b);
}
