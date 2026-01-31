#include "DataSegment.hpp"
#include "CompilerError.hpp"
#include <stdexcept>
#include <sstream>
#include <iomanip>

// Helper to convert integer to hex string for errors
template <typename T>
std::string to_hex_string(T i) {
    std::stringstream stream;
    // Cast to an unsigned integer type to ensure it's treated as a number, not a character.
    // Then set the stream state to output in hexadecimal.
    stream << "0x" << std::hex << static_cast<unsigned int>(i);
    return stream.str();
}

const std::map<std::string, DataEntry>& DataSegmentManager::getEntries() const {
    return m_entries;
}

void DataSegmentManager::add(ConstDataStmt& stmt) {
    if (m_entries.count(stmt.token.lexeme)) {
        throw CompilerError("Data label '" + stmt.token.lexeme + "' already defined.", stmt.token.line_number, stmt.token.col_number);
    }
    DataEntry entry;
    entry.label = stmt.token.lexeme;
    entry.type = stmt.type;
    for (const auto& expr : stmt.initializers) {
        if (auto* literal = dynamic_cast<LiteralExpr*>(expr.get())) {
            long value = std::stol(literal->token.lexeme, nullptr, 0);
            if (stmt.type.base == BaseType::BYTE) {
                entry.bytes.push_back(static_cast<uint8_t>(value));
            } else {
                entry.bytes.push_back(value & 0xFF);
                entry.bytes.push_back((value >> 8) & 0xFF);
            }
        } else {
            throw CompilerError("ROM data initializers must be constant literals.", expr->token.line_number, expr->token.col_number);
        }
    }
    m_entries[entry.label] = entry;
}

bool DataSegmentManager::hasSymbol(const std::string& label) const {
    return m_entries.count(label);
}

Type DataSegmentManager::getSymbolType(const std::string& label) const {
    if (m_entries.find(label) == m_entries.end()) throw std::runtime_error("Linker Error: Undefined data label '" + label + "'.");
    return m_entries.at(label).type;
}