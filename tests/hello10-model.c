#include <stdio.h>

int main() {
    int i = 0;
    char *msg = "Hello, world!";

    while(i < 10) {
        printf("%d\n", msg); 
        i = i + 1;
    }
}
