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
    int arr[3] = {10, 0, fact(5)};
    arr[1] = 20;
    // arr[0] = 10;
    print_int(arr[0]);
    print_char(32); // space
    print_int(arr[1]);
    print_char(32); // space
    // arr[2] = 3;
    print_int(arr[2]);
    print_char(10); // newline
}

