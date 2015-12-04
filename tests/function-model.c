#include <stdio.h>

static void test(char *str) {
	printf("str: %s\n", str);
}

int main() {
	char *s1 = "test1";
	char *s2 = "line2";
	test(s1);
	test(s2);
}
