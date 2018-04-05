#include <stdio.h>

long hex2dec(char * a) {
	char c;
	long n = 0;
	
	while (*a) {
		c = *a++;

		if (c >= '0' && c <= '9') {
			c -= '0';
		}
		else if (c >= 'a' && c <= 'f') {
			c = (c - 'a') + 10;
		}
		else if (c >= 'A' && c <= 'F') {
			c = (c - 'A') + 10;
		}
		else {
			goto INVALID;
		}

		n = (n << 4) + c;
	}

	return n;

INVALID:
	return -1;
}