#pragma once
#include "AST.hpp"
#include <vector>
#include <memory>

class Optimizer : public Visitor {
public:
    Optimizer() = default;
    void optimize(std::vector<std::unique_ptr<Stmt>>& program);

private:
    std::unique_ptr<Stmt> optimize(std::unique_ptr<Stmt> stmt);

    void visit(BlockStmt& stmt) override;
    void visit(FunctionDeclStmt& stmt) override;
    void visit(IfStmt& stmt) override;
    void visit(WhileStmt& stmt) override;
    void visit(SwitchStmt&) override {}
    void visit(CaseStmt&) override {}
    void visit(DefaultStmt&) override {}
    void visit(BreakStmt&) override {}
    void visit(LiteralExpr&, const Type*) override {}
    void visit(VariableExpr&, const Type*) override {}
    void visit(BinaryExpr&, const Type*) override {}
    void visit(AssignExpr&, const Type*) override {}
    void visit(UnaryExpr&, const Type*) override {}
    void visit(MemberAccessExpr&, const Type*) override {}
    void visit(ReturnStmt&) override {}
    void visit(VarDeclStmt&) override {}
    void visit(ExpressionStmt&) override {}
    void visit(PlotStmt&) override {}
    void visit(PlotBeginStmt&) override {}
    void visit(PlotEndStmt&) override {}
    void visit(SetColorStmt&) override {}
    void visit(CmodeStmt&) override {}
    void visit(RpixStmt&) override {}
    void visit(HardwareLoopStmt&) override {}
    void visit(StructDefStmt&) override {}
    void visit(ConstDataStmt&) override {}
    void visit(AddressOfExpr&, const Type*) override {}
    void visit(DereferenceExpr&, const Type*) override {}
    void visit(SubscriptExpr&, const Type*) override {}
    void visit(CallExpr&, const Type*) override {}
	void visit(CastExpr&, const Type*) override {}
};