#include <stdio.h>

int main() {
	int len = 10;
	int arr[10] = {1,5,10,3,67,3,2,88,43,8};
	int i = 0;
	int temp = 0;
	int j = 0;
	while(i < len - 1) {
		j = 0;
		while(j < len - i - 1) {
			if(arr[j] > arr[j+1]) {
				temp = arr[j];
				arr[j] = arr[j+1];
				arr[j+1] = temp;
			}
			j++;
		}
		i++;
	}

	i = 0;
	while(i < len) {
		printf("%d\n", arr[i]);
		i++;
	}

	return 0;
}
