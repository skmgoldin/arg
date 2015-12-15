#include <stdio.h>

static int fib(int n) {
	if(n < 2) return 1;
	return fib(n-1) + fib(n-2);
}

int main() {
	printf("fib(0) = %d\n", fib(0));
	printf("fib(1) = %d\n", fib(1));
	printf("fib(2) = %d\n", fib(2));
	printf("fib(3) = %d\n", fib(3));
	printf("fib(4) = %d\n", fib(4));
	printf("fib(5) = %d\n", fib(5));
}
