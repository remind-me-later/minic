extern void print_char(int c);

void main() {
    print_char(fib(4)*4);
}

int fib(int n) {
    if (n <= 1) {
        return 1;
    }

    return n*fib(n-1);
}