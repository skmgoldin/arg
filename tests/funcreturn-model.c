#include <stdio.h> 

static int square(int val) {
	return val * val;
}

int main() {
	int a = 5;
	int b = square(a);
	printf("%d\n", b);
}
