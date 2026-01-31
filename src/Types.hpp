#pragma once
#include <string>

enum class AddressSpace {
    NONE, // Default, uninitialized state
    RAM,
    ROM
};

// Represents the fundamental size of a type.
enum class BaseType { NONE, BYTE, WORD, VOID, STRUCT };

// Represents the full semantic type.
struct Type {
    BaseType base = BaseType::NONE;
    std::string structName; // Used if base is STRUCT
    int sizeInBytes = 0; // Filled in by Analyzer for all types
    bool is_unsigned = false;
    int pointer_level = 0;
    bool is_far = false;
    int array_size = 0;
    AddressSpace space = AddressSpace::NONE; // Default to NONE
};

// Information stored about a declared variable.
struct Symbol {
    Type type;
    int stackOffset;
    std::string initial_literal_value;
};

// Helper for debugging.
inline std::string to_string(const Type& type) {
    std::string s;
    if (type.space == AddressSpace::ROM) s += "rom ";
    if (type.is_unsigned) s += "unsigned ";
    switch (type.base) {
        case BaseType::BYTE: s += "byte"; break;
        case BaseType::WORD: s += "word"; break;
		case BaseType::STRUCT: s += "struct " + type.structName; break;
        case BaseType::VOID: s += "void"; break;
        default: break;
    }
    if (type.is_far) s += "far ";
    for (int i = 0; i < type.pointer_level; ++i) s += "*";
    if (type.array_size > 0) {
        s += "[" + std::to_string(type.array_size) + "]";
    }
    return s;
}