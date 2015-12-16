#include <stdio.h>

int main() {
	int a = 10;
	int b = 0;
	while(a != b) {
		if(a > b) {
			a--;
		}
		if(a < b) {
			a++;
		}
	}	
	printf("%d\n", a);


	return 0;
}
