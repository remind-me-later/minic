// extern void print_char(int c);
extern void print(char c);
// This function prints an integer to the console.
// void print_int(int n) {
//     int i = 0;
//     if (n < 10) {
//         print_char(48+n);
//     } else {
//         print_int(n/10);
//         i = n%10;
//         print_char(48+i);
//     }
// }

// int fact(int n) {
//     int result = 1;
//     int i = 2;

//     for (i = 2; i <= n; i = i + 1) {
//         result = result * i;
//     }

//     return result;
// }

// void main() {
//     int arr[3] = {10, 0, fact(5)};
//     arr[1] = 20;
//     print_int(arr[0]);
//     print(' ');
//     print_int(arr[1]);
//     print(' ');
//     print_int(arr[2]);
//     print('\n');
// }

void main() {
    char a = 'A';
    char* ptr = &a;
    print(*ptr); 
    print('\n');
}