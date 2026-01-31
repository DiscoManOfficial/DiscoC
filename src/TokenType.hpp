#pragma once

enum class TokenType {
    // Keywords
    KEYWORD_SWITCH,
    KEYWORD_CASE,
    KEYWORD_DEFAULT,
    KEYWORD_BREAK,
    KEYWORD_FAR,
    KEYWORD_SET,
    KEYWORD_STRUCT,
    KEYWORD_CONST,
    KEYWORD_ROM,
    KEYWORD_DRAW,
    KEYWORD_AT,
    KEYWORD_WITH,
    KEYWORD_COLOR,
    KEYWORD_SET_PLOT_OPTIONS,
    KEYWORD_FLUSH_PIXELS,
    KEYWORD_SET_COLOR,
    KEYWORD_PLOT_BEGIN,
    KEYWORD_PLOT_END,
    KEYWORD_PLOT,
    KEYWORD_UNSIGNED,
    KEYWORD_FOR,
    KEYWORD_WHILE,
    KEYWORD_IF,
    KEYWORD_ELSE,
    KEYWORD_WORD,
    KEYWORD_BYTE,
    KEYWORD_VOID,
    KEYWORD_RETURN,
    KEYWORD_CACHE,

    // Single-character tokens
    LPAREN, RPAREN, LBRACE, RBRACE, SEMICOLON,
    COLON,
    EQUAL,
    LBRACKET, RBRACKET,
    AMPERSAND,
    COMMA,
    DOT,

    // Operators
    PLUS, MINUS, STAR, SLASH, // For + - * /

    // Comparison Operators
    GREATER,         // >
    GREATER_EQUAL,   // >=
    LESS,            // <
    LESS_EQUAL,      // <=
    EQUAL_EQUAL,     // ==
    BANG_EQUAL,      // !=

    // Literals and Identifiers
    LITERAL_INTEGER,
    IDENTIFIER,

    // Special tokens
    END_OF_FILE,
    UNKNOWN
};