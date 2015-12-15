#include <stdio.h>

static int fib(int n) {
	if(n < 2) return 1;
	return fib(n-1) + fib(n-2);
}

int main() {
	printf("%d\n", fib(0));
	printf("%d\n", fib(1));
	printf("%d\n", fib(2));
	printf("%d\n", fib(3));
	printf("%d\n", fib(4));
	printf("%d\n", fib(5));
}
