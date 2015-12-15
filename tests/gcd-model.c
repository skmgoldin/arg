#include <stdio.h>

static int gcd(int a, int b) {
	while(a != b) {
		if(a > b) a = a - b;
		else b = b - a;
	}
	return a;
}

int main() {
	printf("%d\n", gcd(2,10));
	printf("%d\n", gcd(4,100));
	printf("%d\n", gcd(3,15));
	printf("%d\n", gcd(100,250));
	return 0;
}
