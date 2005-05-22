/* Compile with gcc -O4 -fno-inline -o ...  or cc -xO4 -xinline= -o ...  */
int c1(int x);
int c2(int y);
int main(int argc) {
	int res = c1(argc);
	printf("Result is %d\n", res);
	return 0;
}

int c1(int x) {
	int i, ret = 0;
	for (i=0; i < x; i++)
		ret += c2(i);
	return ret;
}

int c2(int y) {
	if (y & 1)
		return c1(y);
	else
		return y;
}
