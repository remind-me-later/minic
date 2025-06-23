extern void print_char(int c);

void main() {
    int t = fact(2);
    print_char(48+t);
}

int fact(int n) {
    if (n <= 1) {
        return 1;
    }

    return n*fact(n-1);
}