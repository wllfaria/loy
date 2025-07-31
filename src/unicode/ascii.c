#include <stdbool.h>

bool is_digit(char ch) {
    return ch >= '0' && ch <= '9';
}

bool is_ascii_whitespace(char ch) {
    return ch == '\t' || ch == '\n' || ch == '\x0C' || ch == '\r' || ch == ' ';
}

bool is_ascii_uppercase(char ch) {
    return ch >= 'A' && ch <= 'Z';
}

bool is_ascii_lowercase(char ch) {
    return ch >= 'a' && ch <= 'z';
}

bool is_ascii_alphanumeric(char ch) {
    return is_digit(ch) || is_ascii_uppercase(ch) || is_ascii_lowercase(ch);
}

bool is_ascii_alphabetic(char ch) {
    return is_ascii_uppercase(ch) || is_ascii_lowercase(ch);
}
