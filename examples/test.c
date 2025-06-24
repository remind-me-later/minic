extern void print_char(int c);

// This function prints an integer to the console.
void print_int(int n) {
    int i = 0;
    if (n < 10) {
        print_char(48+n);
    } else {
        print_int(n/10);
        i = n%10;
        print_char(48+i);
    }
}

int fact(int n) {
    int result = 1;
    int i = 2;
    while (i <= n) {
        result = result * i;
        i = i+1;    
    }

    return result;
}

void main() {
    print_int(fact(5));
    print_char(10); // newline
}

