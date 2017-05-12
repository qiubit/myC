int get_recd(int n) {
	if (n == 0 || n == 1) {
		return 1;
	}
	return n * get_recd(n-1);
}

print(get_recd(5));