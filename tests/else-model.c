#include <stdio.h>

int main() {
	int a = 1;
	int b = 0;

	if(a) {
		printf("a is true\n");
		if(b) {
			printf("b is true\n");
		}
		else printf("b is false\n");
	}
	else printf("a is false\n");


	return 0;
}

