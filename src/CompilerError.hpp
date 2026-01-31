#pragma once
#include <exception>
#include <string>

class CompilerError : public std::exception {
public:
    CompilerError(std::string message, int line, int col)
        : m_message(std::move(message)), m_line(line), m_col(col) {
        m_what_buffer = m_message + " (line: " + std::to_string(m_line) + ", col: " + std::to_string(m_col) + ")";
    }

    const char* what() const noexcept override {
        return m_what_buffer.c_str();
    }

    const std::string& getMessage() const { return m_message; }
    int getLine() const { return m_line; }
    int getCol() const { return m_col; }

private:
    std::string m_message;
    int m_line;
    int m_col;
    std::string m_what_buffer;
};