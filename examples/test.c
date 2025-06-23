extern void print_char(int c);

void main() {
    print_char(fact(4)*3);
}

int fact(int n) {
    if (n <= 1) {
        return 1;
    }

    return n*fact(n-1);
}