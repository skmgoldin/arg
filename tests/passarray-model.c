#include <stdio.h>

static int printarray(int * arr, int len) {
	int i = 0;
	while(i < len) {
		printf("%d\n", arr[i]);
		i++;
	}
}

int main() {
	int a[5] = {1,2,3,4,5};
	int b[3] = {1,10, 100};
	printarray(a, 5);
	printarray(b, 3);
	return 0;
}
