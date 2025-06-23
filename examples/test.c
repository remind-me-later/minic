extern void print_char(int c);

void main() {
    print_int(fact(6));
    print_char(10);
}

// This function prints an integer to the console.
void print_int(int n) {
    int i= 0;
    if (n < 10) {
        print_char(48+n);
    } else {
        print_int(n/10);
        i = n%10;
        print_char(48+i);
    }
}

int fact(int n) {
    if (n <= 1) {
        return 1;
    }

    return n*fact(n-1);
}