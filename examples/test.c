extern void print_char(int c);

void main() {
    print_char(48+fact(3));
}

void print_int(int n) {
    if (n < 10) {
        print_char(48+n);
    } else {
        print_int(n/10);
        print_char(48+(n%10));
    }
}

int fact(int n) {
    if (n <= 1) {
        return 1;
    }

    return n*fact(n-1);
}