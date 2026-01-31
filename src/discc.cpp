#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include "Lexer.hpp"
#include "CompilerError.hpp"
#include "Parser.hpp"
#include "Analyzer.hpp"
#include "Optimizer.hpp"
#include "CodeGenerator.hpp"
#include "AssemblyGenerator.hpp"
#include "DataSegment.hpp"
#include "ASTPrinter.hpp"
#include "ObjectFile.hpp"

// Helper function to read a file's content into a string
std::string readFile(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open file: " + path);
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: discc [options] <file.dc> -o <outfile>" << std::endl;
        std::cerr << "Options:" << std::endl;
        std::cerr << "  --emit-asm    Output human-readable assembly (.s) instead of an object file (.o)." << std::endl;
		std::cerr << "  --emit-ast    Print the Abstract Syntax Tree (AST) and exit." << std::endl;
        std::cerr << "  -Wno-cache-overflow   Suppress warnings for cached blocks exceeding 512 bytes." << std::endl;
        return 1;
    }

    std::string in_filepath;
    std::string out_filepath;
    bool emit_asm = false;
	bool emit_ast = false;

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "-o") {
            if (i + 1 < argc) {
                out_filepath = argv[++i];
            } else {
                std::cerr << "Error: -o option requires an output filename." << std::endl;
                return 1;
            }
        } else if (arg == "--emit-asm") {
            emit_asm = true;
        } else if (arg == "--emit-ast") {
            emit_ast = true;
        } else if (arg == "-Wno-cache-overflow") {
            // This flag will be read later from the parser's config object.
        } else {
            if (!in_filepath.empty()) {
                std::cerr << "Error: Only one input file can be specified." << std::endl;
                return 1;
            }
            in_filepath = arg;
        }
    }

    if (in_filepath.empty()) {
        std::cerr << "Error: No input file specified." << std::endl;
        return 1;
    }

    if (emit_ast && !out_filepath.empty()) {
        std::cerr << "Warning: -o option is ignored when using --emit-ast." << std::endl;
    }

    // Re-parse args to set config flags on a temporary parser object
    {
        Lexer temp_lexer("");
        auto temp_tokens = temp_lexer.scanTokens();
        Parser temp_parser(temp_tokens);
        for (int i = 1; i < argc; ++i) {
            if (std::string(argv[i]) == "-Wno-cache-overflow") {
                temp_parser.getConfigForUpdate().warn_on_cache_overflow = false;
            }
        }
    }

    // Automatically determine output filename if not provided
    if (out_filepath.empty() && !emit_ast) {
        // Find the last dot to replace the extension
        size_t last_dot = in_filepath.find_last_of(".");
        std::string base = (last_dot == std::string::npos) ? in_filepath : in_filepath.substr(0, last_dot);
        out_filepath = base + (emit_asm ? ".s" : ".o");
    }
    
    try {
        // 1. Read Source Code from File
        std::string source = readFile(in_filepath);
        
        // 2. Frontend (Lexing & Parsing)
        Lexer lexer(source);
        auto tokens = lexer.scanTokens();
        Parser parser(tokens);
        // Now, handle the flag again for the actual parser instance
        for (int i = 1; i < argc; ++i) {
             if (std::string(argv[i]) == "-Wno-cache-overflow") {
                parser.getConfigForUpdate().warn_on_cache_overflow = false;
            }
        }
        auto program_ast = parser.parseProgram();

        // 3. Optimization
        Optimizer optimizer;
        optimizer.optimize(program_ast);

        // 3.5: Handle AST Printing
        if (emit_ast) {
            std::cout << "Abstract Syntax Tree" << std::endl;
            ASTPrinter printer;
            for (const auto& stmt : program_ast) {
                std::cout << printer.print(*stmt) << std::endl;
            }
            // Exit successfully without running the rest of the compiler.
            return 0;
        }

        // 4. Semantic Analysis (One Pass)
        DataSegmentManager data_manager;
        Analyzer analyzer(data_manager);
        analyzer.analyze(program_ast);

        // 5. Backend (Code Generation or Assembly Emission)
        if (emit_asm) {
            std::cout << "Emitting Assembly: " << in_filepath << " -> " << out_filepath << std::endl;
            AssemblyGenerator asm_gen(analyzer.getFunctionSymbols(), data_manager);
            std::string asm_output = asm_gen.generate(program_ast);
            
            std::ofstream outFile(out_filepath);
            if (!outFile) {
                throw std::runtime_error("Failed to open assembly file for writing: " + out_filepath);
            }
            outFile << asm_output;

        } else {
            std::cout << "Compiling to Object: " << in_filepath << " -> " << out_filepath << std::endl;
            CodeGenerator code_generator(
                analyzer.getAllLocalSymbols(),
                analyzer.getFunctionSymbols(),
                data_manager,
				parser.getConfig()
            );
            ObjectFile obj = code_generator.generate(program_ast);
            obj.write(out_filepath);
        }

        std::cout << "Successfully generated output." << std::endl;

    } catch (const CompilerError& e) {
        std::cerr << "\nCompilation Failed!\n" << in_filepath << ":" << e.getLine() << ":" << e.getCol() << ": error: " << e.getMessage() << std::endl;
        return 1;
    } catch (const std::runtime_error& e) {
        std::cerr << "\nInternal Compiler Error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}