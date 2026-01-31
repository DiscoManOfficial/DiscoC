#include "Optimizer.hpp"

// The main entry point for the optimization pass.
void Optimizer::optimize(std::vector<std::unique_ptr<Stmt>>& program) {
    for (auto& stmt : program) {
        stmt->accept(*this);
    }
}

// Visit a function and optimize its body.
void Optimizer::visit(FunctionDeclStmt& stmt) {
    auto bodyBlock = std::make_unique<BlockStmt>(std::move(stmt.body));
    bodyBlock->accept(*this);
    stmt.body = std::move(bodyBlock->statements);
}

// Visit an if statement and optimize its branches.
void Optimizer::visit(IfStmt& stmt) {
    stmt.thenBranch->accept(*this);
    if (stmt.elseBranch) {
        stmt.elseBranch->accept(*this);
    }
}

// Visit a while loop and optimize its body.
void Optimizer::visit(WhileStmt& stmt) {
    stmt.body->accept(*this);
}

// The core of the optimization logic lives here.
void Optimizer::visit(BlockStmt& block) {
    std::vector<std::unique_ptr<Stmt>> optimized_statements;
    for (size_t i = 0; i < block.statements.size(); ++i) {
        auto& current_stmt = block.statements[i];

        // PATTERN MATCHING LOGIC
        // A `for` loop is desugared into a BlockStmt containing an
        // initializer statement and a WhileStmt.
        auto* as_block = dynamic_cast<BlockStmt*>(current_stmt.get());
        if (!as_block || as_block->statements.size() != 2) {
            // It's not a desugared for-loop, or not the right shape.
            // Recursively optimize it and add to our new list.
            current_stmt->accept(*this);
            optimized_statements.push_back(std::move(current_stmt));
            continue;
        }
        
        // 1. Check for Initializer: `word i = N;`
        std::string loop_var_name;
        LiteralExpr* init_val = nullptr;

        if (auto* decl_init = dynamic_cast<VarDeclStmt*>(as_block->statements[0].get())) {
            // Case 1: for (word i = N; ...)
            if (!decl_init->initializer) continue; // Not an optimizable pattern
            init_val = dynamic_cast<LiteralExpr*>(decl_init->initializer.get()); 
            loop_var_name = decl_init->token.lexeme;
        } else if (auto* expr_init_stmt = dynamic_cast<ExpressionStmt*>(as_block->statements[0].get())) {
            // Case 2: i = N;
            auto* assign_init = dynamic_cast<AssignExpr*>(expr_init_stmt->expression.get());
            if (!assign_init) continue;
            auto* assign_target = dynamic_cast<VariableExpr*>(assign_init->name.get());
            if (!assign_target) continue;
            init_val = dynamic_cast<LiteralExpr*>(assign_init->value.get());
            loop_var_name = assign_target->token.lexeme;
        } else {
            // Initializer is neither a declaration nor an expression, so it can't match.
            continue; // This should not be reachable with a valid for-loop
        }

        if (!init_val) {
            current_stmt->accept(*this);
            optimized_statements.push_back(std::move(current_stmt));
            continue;
        }

        // 2. Check for Loop: `while (i > 0) { ...; i = i - 1; }`
        auto* loop = dynamic_cast<WhileStmt*>(as_block->statements[1].get());
        if (!loop) {
            current_stmt->accept(*this);
            optimized_statements.push_back(std::move(current_stmt));
            continue;
        }

        // 3. Check Condition: `i > 0`
        auto* condition = dynamic_cast<BinaryExpr*>(loop->condition.get());
        auto* cond_left = condition ? dynamic_cast<VariableExpr*>(condition->left.get()) : nullptr;
        auto* cond_right = condition ? dynamic_cast<LiteralExpr*>(condition->right.get()) : nullptr;
        if (!condition || !cond_left || !cond_right || cond_left->token.lexeme != loop_var_name ||
            condition->token.type != TokenType::GREATER || std::stol(cond_right->token.lexeme) != 0) {
            current_stmt->accept(*this);
            optimized_statements.push_back(std::move(current_stmt));
            continue;
        }

        // 4. Check Increment: `i = i - 1`
        auto* loop_body_block = dynamic_cast<BlockStmt*>(loop->body.get());
        if (!loop_body_block || loop_body_block->statements.empty()) {
            current_stmt->accept(*this);
            optimized_statements.push_back(std::move(current_stmt));
            continue;
        }
        auto* increment_stmt = dynamic_cast<ExpressionStmt*>(loop_body_block->statements.back().get());
        auto* assign_expr = increment_stmt ? dynamic_cast<AssignExpr*>(increment_stmt->expression.get()) : nullptr;
        auto* assign_target = assign_expr ? dynamic_cast<VariableExpr*>(assign_expr->name.get()) : nullptr;
        auto* assign_val = assign_expr ? dynamic_cast<BinaryExpr*>(assign_expr->value.get()) : nullptr;
        auto* sub_left = assign_val ? dynamic_cast<VariableExpr*>(assign_val->left.get()) : nullptr;
        auto* sub_right = assign_val ? dynamic_cast<LiteralExpr*>(assign_val->right.get()) : nullptr;

        if (!assign_target || assign_target->token.lexeme != loop_var_name ||
            !assign_val || assign_val->token.type != TokenType::MINUS ||
            !sub_left || sub_left->token.lexeme != loop_var_name ||
            !sub_right || std::stol(sub_right->token.lexeme) != 1) {
            current_stmt->accept(*this);
            optimized_statements.push_back(std::move(current_stmt));
            continue;
        }

        // SUCCESS! We have a match!
        // Remove the `i = i - 1` statement from the body.
        loop_body_block->statements.pop_back();

        // Create the new HardwareLoopStmt node.
        auto count_literal = std::make_unique<LiteralExpr>(init_val->token);
        auto new_loop = std::make_unique<HardwareLoopStmt>(std::move(count_literal), std::move(loop->body));
        
        // Recursively optimize the new body, just in case of nested loops.
        new_loop->body->accept(*this);
        
        optimized_statements.push_back(std::move(new_loop));
    }
    // Replace the old statements with the new, possibly optimized list.
    block.statements = std::move(optimized_statements);
}