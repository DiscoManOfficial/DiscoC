#pragma once
#include <vector>
#include <cstdint>
#include <string>
#include <map>
#include "AST.hpp"
#include "Parser.hpp"

struct DataEntry {
    std::string label;
    std::vector<uint8_t> bytes;
    Type type;
};

class DataSegmentManager {
public:
    DataSegmentManager() = default;
    void add(ConstDataStmt& stmt);
    bool hasSymbol(const std::string& label) const;
    Type getSymbolType(const std::string& label) const;
    const std::map<std::string, DataEntry>& getEntries() const;

private:
    std::map<std::string, DataEntry> m_entries;
};