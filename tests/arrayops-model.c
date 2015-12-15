#include <stdio.h>

static void printfunc(int i) {
	printf("%d\n", i);
}

int main() {
	int len = 10;
	int a[len];
	
	int i = 0;
	while(i < len) {
		a[i] = 10-i;
		i++;
	}
	while(i > 0) {
		printfunc(a[i]);
		i--;
	}



}
