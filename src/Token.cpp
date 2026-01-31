#include "Token.hpp"
#include <iostream>

Token::Token(TokenType type, std::string lexeme, int line, int col)
    : type(type), lexeme(std::move(lexeme)), line_number(line), col_number(col) {}

void Token::print() const {
    std::cout << "Token( " << to_string(type)
              << ", Lexeme: '" << lexeme << "', Line: " << line_number << ", Col: " << col_number << " )" << std::endl;
}

std::string to_string(TokenType type) {
    switch (type) {
        case TokenType::KEYWORD_STRUCT: return "KEYWORD_STRUCT";
        case TokenType::KEYWORD_FAR: return "KEYWORD_FAR";
        case TokenType::KEYWORD_SET: return "KEYWORD_SET";
        case TokenType::KEYWORD_UNSIGNED: return "KEYWORD_UNSIGNED";
        case TokenType::KEYWORD_WORD: return "KEYWORD_WORD";
        case TokenType::KEYWORD_BYTE: return "KEYWORD_BYTE";
        case TokenType::KEYWORD_VOID: return "KEYWORD_VOID";
        case TokenType::KEYWORD_IF: return "KEYWORD_IF";
        case TokenType::KEYWORD_ELSE: return "KEYWORD_ELSE";
        case TokenType::KEYWORD_WHILE: return "KEYWORD_WHILE";
        case TokenType::KEYWORD_FOR: return "KEYWORD_FOR";
        case TokenType::KEYWORD_RETURN: return "KEYWORD_RETURN";
        case TokenType::KEYWORD_CACHE: return "KEYWORD_CACHE";
        case TokenType::LPAREN: return "LPAREN";
        case TokenType::RPAREN: return "RPAREN";
        case TokenType::LBRACE: return "LBRACE";
        case TokenType::RBRACE: return "RBRACE";
        case TokenType::LBRACKET: return "LBRACKET";
        case TokenType::RBRACKET: return "RBRACKET";
        case TokenType::SEMICOLON: return "SEMICOLON";
        case TokenType::DOT: return "DOT";
        case TokenType::COMMA: return "COMMA";
        case TokenType::EQUAL: return "EQUAL";
        case TokenType::PLUS: return "PLUS";
        case TokenType::MINUS: return "MINUS";
        case TokenType::STAR: return "STAR";
        case TokenType::SLASH: return "SLASH";
        case TokenType::GREATER: return "GREATER";
        case TokenType::GREATER_EQUAL: return "GREATER_EQUAL";
        case TokenType::LESS: return "LESS";
        case TokenType::LESS_EQUAL: return "LESS_EQUAL";
        case TokenType::EQUAL_EQUAL: return "EQUAL_EQUAL";
        case TokenType::BANG_EQUAL: return "BANG_EQUAL";
        case TokenType::LITERAL_INTEGER: return "LITERAL_INTEGER";
        case TokenType::IDENTIFIER: return "IDENTIFIER";
        case TokenType::END_OF_FILE: return "END_OF_FILE";
        case TokenType::UNKNOWN: return "UNKNOWN";
        default: return "TOKEN_TYPE(" + std::to_string(static_cast<int>(type)) + ")";
    }
}