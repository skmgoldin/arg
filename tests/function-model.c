#include <stdio.h>

static void test(char *str) {
	printf("%s\n", str);
}

int main() {
	char *s1 = "line1";
	char *s2 = "line2";
	test(s1);
	test(s2);
}
