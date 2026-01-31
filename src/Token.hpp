#pragma once

#include <string>
#include "TokenType.hpp"

// Forward declaration of the helper function. We just promise it exists.
std::string to_string(TokenType type);

struct Token {
    TokenType type;
    std::string lexeme;
    int line_number; // The line where the token appears
    int col_number;  // The column where the token begins

    // The constructor is a good way to initialize the token.
    Token(TokenType type, std::string lexeme, int line, int col);

    // Just the declaration of the print method.
    void print() const;
};