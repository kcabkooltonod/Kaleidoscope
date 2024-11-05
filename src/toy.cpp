#include "../include/KaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
using namespace llvm::orc;
using namespace llvm::sys;

#include "../include/ExprAST.h"

//首先，肯定是构造词法分析器，将输入的字符流识别为一个个token

enum Token{
    token_eof = -1,

    //函数定义关键字
    token_def = -2,
    token_extern = -3,

    //标识符
    token_identifier = -4,
    //数值
    token_number = -5,

    //控制流语句关键字
    token_if = -6,
    token_then = -7,
    token_else = -8,

    token_for = -9,
    token_in = -10,

    //操作符定义关键字
    token_binary = -11,
    token_unary = -12,

    token_var = -13,
};


static std::string IdentifierStr;
static double NumVal;

static int gettoken(){
    static int LastChar = ' ';
    while(isspace(LastChar))
    LastChar = getchar();

// identifier: [a-zA-Z][a-zA-Z0-9]*
    if(isalpha(LastChar)){
        IdentifierStr = LastChar;
        while(isalnum((LastChar = getchar())))
            IdentifierStr += LastChar;
        
        if(IdentifierStr == "def"){
            return token_def;
        }
        if(IdentifierStr == "extern"){
            return token_extern;
        }
        if(IdentifierStr == "if"){
            return token_if;
        }
        if(IdentifierStr == "then"){
            return token_then;
        }
        if(IdentifierStr == "else"){
            return token_else;
        }
        if(IdentifierStr == "for"){
            return token_for;
        }
        if(IdentifierStr == "in"){
            return token_in;
        }
        if(IdentifierStr == "binary"){
            return token_binary;
        }
        if(IdentifierStr == "unary"){
            return token_unary;
        }
        if(IdentifierStr == "var"){
            return token_var;
        }
        return token_identifier;
    }

// number:[0-9.]+
    if(isdigit(LastChar) || LastChar == '.'){
        std::string NumStr;

        do{
            NumStr += LastChar;
            LastChar = getchar();
        }while(isdigit(LastChar) || LastChar == '.');
        // 将string转为double，第一个参数为字符串的地址，
        // 第二个参数可以保存转换过程中首次失败的指针位置，0表示不关心
        // std::cout<<NumStr<<std::endl;
        NumVal = strtod(NumStr.c_str(), nullptr);
        // fprintf(stderr, "NumVal: %f",NumVal);
        return token_number;
    }

// comment until end of line
    if(LastChar == '#'){
        do{
            LastChar = getchar();
        }while(LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if(LastChar != EOF){
            return gettoken();
        }
    }

// eof
    if(LastChar == EOF)
        return token_eof;

//otherwise, return the ascii number of the char
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}


//lexer结束，parser开始


static int CurToken;
// ascii || token_def || token_extern || token_identifier || token_number || token_eof
static int getNextToken(){
    return CurToken = gettoken();
}

// 错误日志打印，辅助函数
std::unique_ptr<ExprAST> LogError(const char *Str){
    fprintf(stderr, "Error: %s\n", Str);
    return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str){
    LogError(Str);
    return nullptr;
}

Value *LogErrorV(const char *Str){
    LogError(Str);
    return nullptr;
}


static std::unique_ptr<ExprAST> ParseExpression();


//接下来，会写三个方法去解析基础的表达式，包括数值、带括号的表达式和标识符

//解析数值
static std::unique_ptr<ExprAST> ParseNumberExpr(){

    auto Result = std::make_unique<NumberExprAST>(
        NumVal
    );
    getNextToken();
    return std::move(Result);
}


//'('+ expr ')'+
static std::unique_ptr<ExprAST> ParseParenExpr(){
    //eat the '('
    getNextToken();
    auto V = ParseExpression();
    if(!V){return nullptr;}

    if(CurToken != ')'){
        return LogError("expected ')'");
    }
    getNextToken();
    return V;
}


//id ('(' (,id)* ')')?
static std::unique_ptr<ExprAST> ParseIdentifierExpr(){
    std::string IdName = IdentifierStr;
    getNextToken();
    //simple variable
    if(CurToken != '('){
        return std::make_unique<VariableExprAST>(
            IdName
        );
    }
    //eat '('
    getNextToken();
    std::vector<std::unique_ptr<ExprAST>> Args;
    if(CurToken!=')'){
        while(true){
            if(auto Arg = ParseExpression())
                Args.push_back(std::move(Arg));
            else 
                return nullptr;
            
            if(CurToken == ')')
                break;
            
            if(CurToken != ',')
                return LogError("Expected ')' or ',' in argument list.");
            getNextToken();
        }
    }
    //eat ')'
    getNextToken();

    return std::make_unique<CallExprAST>(
        IdName, std::move(Args)
    );
}


//ifexpr ::= 'if' expr 'then' expr 'else' expr 
static std::unique_ptr<ExprAST> ParseIfExpr(){
    getNextToken();

    auto Cond = ParseExpression();
    if(!Cond)
        return nullptr;
    
    if(CurToken != token_then)
        return LogError("Expected then.");
    getNextToken();

    auto Then = ParseExpression();
    if(!Then)
        return nullptr;

    if(CurToken != token_else)
        return LogError("Expected else.");
    getNextToken();

    auto Else = ParseExpression();
    if(!Else)
        return nullptr;

    return std::make_unique<IfExprAST>(
        std::move(Cond), std::move(Then), std::move(Else)
    );
}

//forexpr ::= 'for' id '=' expr ',' expr  (',' expr)? 'in' expr
static std::unique_ptr<ExprAST> ParseForExpr(){
    //吃掉for
    getNextToken();

    if(CurToken != token_identifier) 
        return LogError("Expected identifier after for.");
    
    std::string IdName = IdentifierStr;
    getNextToken();

    if(CurToken != '=')
        return LogError("Expected '-' after for.");
    getNextToken();

    //记录start
    auto Start = ParseExpression();
    if(!Start)
        return nullptr;
    if(CurToken != ',')
        return LogError("Expected ',' after for start value.");
    getNextToken();

    //记录end
    auto End = ParseExpression();
    if(!End)
        return nullptr;

    //step为可选项
    std::unique_ptr<ExprAST> Step;
    if(CurToken == ','){
        getNextToken();
        Step = ParseExpression();
        if(!Step)
            return nullptr;
    }

    if(CurToken != token_in)
        return LogError("Expected 'in' after for");
    getNextToken();

    auto Body = ParseExpression();
    if(!Body)
        return nullptr;

    return std::make_unique<ForExprAST>(
        IdName, std::move(Start), std::move(End), 
        std::move(Step), std::move(Body)
    );
}

//varexpr ::= 'var' id ('=' expr)? (',' id ('=' expr)?)* 'in' expr
static std::unique_ptr<ExprAST> ParseVarExpr(){
    getNextToken();
    std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;

    if(CurToken != token_identifier)
        return LogError("expected identifier after var.");
    
    while(true){
        std::string Name = IdentifierStr;
        getNextToken();

        std::unique_ptr<ExprAST> Init;
        if(CurToken == '='){
            getNextToken();
            Init = ParseExpression();
            if(!Init)
                return nullptr;
        }
        VarNames.push_back(std::make_pair(Name, std::move(Init)));

        if(CurToken != ',') break;

        getNextToken();

        if(CurToken != token_identifier)
            return LogError("expected identifier list after var.");
    }

    if(CurToken != token_in)
        return LogError("expected 'in' keword after 'var'.");

    getNextToken();

    auto Body = ParseExpression();
    if(!Body)
        return nullptr;
    
    return std::make_unique<VarExprAST>(std::move(VarNames), std::move(Body));
}


//primary ::= number||expr||id||'if'||'for'||'var'
static std::unique_ptr<ExprAST> ParsePrimary(){

    switch (CurToken)
    {
    case token_number:
        return ParseNumberExpr();
    case '(':
        return ParseParenExpr();
    case token_identifier:
        return ParseIdentifierExpr();
    case token_if:
        return ParseIfExpr();
    case token_for:
        return ParseForExpr();
    case token_var:
        return ParseVarExpr();
    default:
        return LogError("unknown token when expecting an expression");
    }
}

// unary ::= primary
//       ::= UnaryOp primary
static std::unique_ptr<ExprAST> ParseUnary(){
    if(!isascii(CurToken) || CurToken == '(' || CurToken == ',')
        return ParsePrimary();
    
    int Opc = CurToken;
    getNextToken();
    if(auto Operand = ParsePrimary())
        return std::make_unique<UnaryExprAST>(Opc, std::move(Operand));
    return nullptr;
}


//接下来进入二元表达式的解析

static std::map<char, int> BinopPrecedence;

static int GetTokenPrecedence(){
    if (!isascii(CurToken))
    return -1;

    int TokenPrec = BinopPrecedence[CurToken];
    if(TokenPrec <= 0) { return -1;}
    return TokenPrec;
}

//解析二元操作符和右值对，[BinaryOp, RHS]*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, 
                                std::unique_ptr<ExprAST> LHS){

    while(true){
        int TokenPrec = GetTokenPrecedence();
        // fprintf(stderr, "%d",TokenPrec);
        if(TokenPrec < ExprPrec)
            return LHS;
        
        int BinOp = CurToken;
        getNextToken();

        //右值不再是primary而是unary
        auto RHS = ParseUnary();
        if(!RHS)
            return nullptr;

        int NextPrec = GetTokenPrecedence();
        if(TokenPrec < NextPrec){
            RHS = ParseBinOpRHS(TokenPrec+1, std::move(RHS));
            if(!RHS) return nullptr;
        }

        LHS = std::make_unique<BinaryExprAST>(
            BinOp, std::move(LHS), std::move(RHS)
        );
    }
}


//expr ::= LHS = unary [binaryOp, RHS = unary]*
static std::unique_ptr<ExprAST> ParseExpression(){
    //左值也不再是primary
    auto LHS = ParseUnary();
    if(!LHS)
        return nullptr;

    return ParseBinOpRHS(0, std::move(LHS));
}



//以上，我们已经完成了所有表达式节点的构建，剩下的就只有原型节点和函数节点

//prototype ::= id '(' id* ')'
//          ::= binary LETTER number? (id id)
//          ::= unary LETTER (id)
static std::unique_ptr<PrototypeAST> ParsePrototype(){
    std::string FnName;

    unsigned Kind = 0;
    unsigned BinaryPrecednece = 30;

    switch (CurToken)
    {
    case token_identifier:
        FnName = IdentifierStr;
        Kind = 0;
        getNextToken();
        break;
    case token_binary:
        getNextToken();
        if(!isascii(CurToken))
            return LogErrorP("Expected binary operator.");
        FnName = "binary";
        FnName += (char)CurToken;
        Kind = 2;
        getNextToken();
        //获取合法的优先级
        if(CurToken == token_number){
            if(NumVal < 1 || NumVal > 100)
                return LogErrorP("Invalid precedence: must be 1~100");
            BinaryPrecednece = (unsigned)NumVal;
            getNextToken();
        }
        break;
    case token_unary:
        getNextToken();
        if(!isascii(CurToken))
            return LogErrorP("Expected unary operator.");
        FnName = "unary";
        FnName += (char)CurToken;
        Kind = 1;
        getNextToken();
        break;
    default:
        return LogErrorP("Expected function name in prototype");
        break;
    }

    if(CurToken != '(')
        return LogErrorP("Expected '(' in prototype.");
    
    std::vector<std::string> ArgNames;
    while(getNextToken() == token_identifier)
        ArgNames.push_back(IdentifierStr);
    if(CurToken != ')')
        LogErrorP("Expected ')' in prototype.");

    getNextToken();

    if(Kind && ArgNames.size()!=Kind)
        return LogErrorP("Invalid number of operands for operator.");

    return std::make_unique<PrototypeAST>(
        FnName, std::move(ArgNames), Kind != 0, BinaryPrecednece
    );
}

// 'def' prototype expr
static std::unique_ptr<FunctionAST> ParseDefinition(){
    getNextToken();

    auto Proto = ParsePrototype();
    if(!Proto) return nullptr;

    if(auto E = ParseExpression())
        return std::make_unique<FunctionAST>(
            std::move(Proto), std::move(E)
        );
    return nullptr;
}

//'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern(){
    getNextToken();
    return ParsePrototype();
}

//expr
static std::unique_ptr<FunctionAST> ParseTopLevelExpr(){
    if(auto E = ParseExpression()){
        // fprintf(stderr, "toplevel");
        auto Proto = std::make_unique<PrototypeAST>(
            "__anon_expr", std::vector<std::string>()
        );
        return std::make_unique<FunctionAST>(
            std::move(Proto), std::move(E)
        );
    }
    return nullptr;
}

//开始生成llvm中间代码

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, AllocaInst*> NamedValues;

static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::unique_ptr<FunctionPassManager> TheFPM;
static std::unique_ptr<LoopAnalysisManager> TheLAM;
static std::unique_ptr<FunctionAnalysisManager> TheFAM;
static std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
static std::unique_ptr<ModuleAnalysisManager> TheMAM;
static std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
static std::unique_ptr<StandardInstrumentations> TheSI;
static ExitOnError ExitOnErr;

/*定义一个全局的函数原型表，用于保存定义过的函数的原型，
以便在JIT中删除module后不会影响函数的调用*/
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;

//  %X = alloca i32           ; type of %X is i32*.
static AllocaInst *CreateEntryBlockAlloca(
    Function *TheFunction, const std::string &VarName
){
    IRBuilder<> TmpB(
        &TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin()
    );
    return TmpB.CreateAlloca(Type::getDoubleTy(*TheContext), nullptr, VarName);
}

//新的获取函数方法，从当前module和全局原型表中寻找函数信息
Function *getFunction(std::string Name){
    if(auto *F = TheModule->getFunction(Name))
        return F;
    //find方法返回的是一个迭代器而不是value
    auto FI = FunctionProtos.find(Name);
    if(FI != FunctionProtos.end())
        //->second表示迭代器第二个的指针，也就是value
        return FI->second->codegen();

    return nullptr;
}



Value *NumberExprAST::codegen(){
    return ConstantFP::get(*TheContext, APFloat(Val));
}

Value *VariableExprAST::codegen(){
    AllocaInst *A = NamedValues[Name];
    if(!A){
        LogErrorV("Unknow variable name.");
    }
    return Builder->CreateLoad(A->getAllocatedType(), A, Name.c_str());
}

Value *VarExprAST::codegen(){
    //记录原来的符号表中的变量值
    std::vector<AllocaInst *> OldBindings;

    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    for (unsigned i = 0, e = VarNames.size(); i!=e; i++){
        const std::string &VarName = VarNames[i].first;
        ExprAST *Init = VarNames[i].second.get();

        Value *InitVal;
        if(Init){
            InitVal = Init->codegen();
            if(!InitVal)
                return nullptr;
        }else{
            InitVal = ConstantFP::get(*TheContext, APFloat(0.0));
        }

        AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
        Builder->CreateStore(InitVal, Alloca);

        //将覆盖前的变量值存储到oldbinding中，
        //因为在var/in语句中定义的变量值只在body中起作用
        OldBindings.push_back(NamedValues[VarName]);
        NamedValues[VarName] = Alloca;
    }

    Value *BodyVal = Body->codegen();
    if(!BodyVal)
        return nullptr;
    //body语句执行完，将符号表恢复
    for (unsigned i = 0, e = VarNames.size(); i!=e; i++){
        NamedValues[VarNames[i].first] = OldBindings[i];
    }
    return BodyVal;
}



Value *BinaryExprAST::codegen(){
    if(Op == '='){
        VariableExprAST *LHSE = static_cast<VariableExprAST*> (LHS.get());
        if(!LHSE)
            return LogErrorV("destination of '=' must be a variable.");
        
        Value *Val = RHS->codegen();
        if(!Val)
            return nullptr;
        Value *Variable = NamedValues[LHSE->getName()];
        if(!Variable)
            return LogErrorV("Unknown variable name.");

        Builder->CreateStore(Val, Variable);
        return Val;
    }


    Value *L = LHS->codegen();
    Value *R = RHS->codegen();

    if(!L || !R)
        return nullptr;

    switch (Op)
    {
    case '+':
        return Builder->CreateFAdd(L, R, "addtmp");
    case '-':
        return Builder->CreateFSub(L, R, "subtmp");
    case '*':
        return Builder->CreateFMul(L, R, "multmp");
    case '/':
        return Builder->CreateFDiv(L, R, "divtmp");
    case '<':
        L = Builder->CreateFCmpULT(L, R, "cmptmp");
        return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext),
        "booltmp");
    default:
        break;
    }

    //查表，是否存在对应的二元操作函数
    Function *F = getFunction(std::string("binary") + Op);
    assert(F && "binary operator not found.");

    //生成函数调用代码
    Value *Ops[2] = {L, R};
    return Builder->CreateCall(F, Ops, "binop");

}

Value *UnaryExprAST::codegen(){
    Value *OperandV = Operand->codegen();
    if(!OperandV)
        return nullptr;
    
    Function *F = getFunction(std::string("unary") + Opcode);
    if(!F)
        return LogErrorV("Unknown unary operator.");

    return Builder->CreateCall(F, OperandV, "unop");
}


Value *IfExprAST::codegen(){
    Value *CondV = Cond->codegen();
    if(!CondV)
        return nullptr;

    //%ifcond = fcmp one double %x, 0.000000e+00
    CondV = Builder->CreateFCmpONE(
        CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond"
    );

    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
    BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");

    //  br i1 %ifcond, label %then, label %else
    Builder->CreateCondBr(CondV, ThenBB, ElseBB);

    //生成ThenBB的代码
    //表示进入ThenBB基本块进行代码生成
    Builder->SetInsertPoint(ThenBB);
    Value *ThenV = Then->codegen();
    if(!ThenV)
        return nullptr;

    //  br label %ifcont
    Builder->CreateBr(MergeBB);
    //获取当前插入点所在的基本块，此行代码的目的是为了在生成phi指令时记录最后到达的基本块，
    //这是因为ThenBB中的代码也可能会引起基本块的跳转。
    ThenBB = Builder->GetInsertBlock();
    
    //生成ElseBB的代码
    TheFunction->insert(TheFunction->end(), ElseBB);
    Builder->SetInsertPoint(ElseBB);
    Value *ElseV = Else->codegen();
    if(!ElseV)
        return nullptr;

    Builder->CreateBr(MergeBB);
    ElseBB = Builder->GetInsertBlock();

    //生成MergeBB的代码
    TheFunction->insert(TheFunction->end(), MergeBB);
    Builder->SetInsertPoint(MergeBB);

    //  %iftmp = phi double [ %calltmp, %then ], [ %calltmp1, %else ]
    PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");
    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);

    //  ret double %iftmp
    return PN;
}

Value *ForExprAST::codegen(){
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

    Value *StartVal = Start->codegen();
    if(!StartVal)
        return nullptr;

    Builder->CreateStore(StartVal, Alloca);
    
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

    Builder->CreateBr(LoopBB);

    Builder->SetInsertPoint(LoopBB);

    // PHINode *Variable = Builder->CreatePHI(
    //     Type::getDoubleTy(*TheContext), 2, VarName
    // );
    // Variable->addIncoming(StartVal, PreheaderBB);

    //由于for循环引入了一个新的变量，这很可能与之前定义过的变量名冲突，
    //而for循环引入的变量只在这个循环中起作用，最简单的方法就是先把冲突的变量取出来，处理完for后再放回去
    AllocaInst *OldVal = NamedValues[VarName];
    NamedValues[VarName] = Alloca;

    if(!Body->codegen())
        return nullptr;

    Value *StepVal = nullptr;

    if(Step){
        StepVal = Step->codegen();
        if(!StepVal)
            return nullptr;    
    }else{
        StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
    }

    // Value *NextVar = Builder->CreateFAdd(Variable, StepVal, "nextvar");

    Value *EndVal = End->codegen();
    if(!EndVal)
        return nullptr;

    Value *CurVar = Builder->CreateLoad(Alloca->getAllocatedType(), Alloca, VarName.c_str());
    Value *NextVar = Builder->CreateFAdd(CurVar, StepVal, "nextvar");
    Builder->CreateStore(NextVar, Alloca);

    EndVal = Builder->CreateFCmpONE(
        EndVal, ConstantFP::get(*TheContext, APFloat(0.0)), "loopcond"
    );

    // BasicBlock *LoopEndBB = Builder->GetInsertBlock();
    BasicBlock *AfterBB = BasicBlock::Create(
        *TheContext, "afterloop", TheFunction
    );

    Builder->CreateCondBr(EndVal, LoopBB, AfterBB);

    Builder->SetInsertPoint(AfterBB);

    // Variable->addIncoming(NextVar, LoopEndBB);

    if(OldVal)
        NamedValues[VarName] = OldVal;
    else
        NamedValues.erase(VarName);
    // fprintf(stderr, "ret");
    return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}



Value *CallExprAST::codegen(){
    Function *CalleeF = getFunction(Callee);
    if(!CalleeF)
        LogErrorV("Unknown function name.");
    
    if(CalleeF->arg_size()!=Args.size())
        return LogErrorV("Incorrect argument passed.");
    std::vector<Value*> ArgsV;

    for(unsigned i = 0, e = Args.size(); i!=e; i++){
        ArgsV.push_back(Args[i]->codegen());
        if(!ArgsV.back())
            return nullptr;
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *PrototypeAST::codegen(){
    std::vector<Type*> Doubles(
        Args.size(), Type::getDoubleTy(*TheContext));
    //获取函数类型，后面用于创建函数，
    //参数依次表示，返回类型，参数，是否为可变参数
    FunctionType *FT = FunctionType::get(
        Type::getDoubleTy(*TheContext), Doubles, false
    );

    Function *F = Function::Create(
        FT, Function::ExternalLinkage, Name, TheModule.get()
    );

    unsigned Idx = 0;
    for (auto &Arg : F->args())
        Arg.setName(Args[Idx++]);

    return F;
}

Function *FunctionAST::codegen(){

    //定义新的函数时需要将新的函数原型加入到全局的函数表中，
    //函数的名字是可以重复的，但只会记录最新的原型到表中
    auto &P = *Proto;
    FunctionProtos[Proto->getName()] = std::move(Proto);
    Function *TheFunction = getFunction(P.getName());
    
    if (!TheFunction)
        return nullptr;

    if(P.isBinaryOp())
        BinopPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();

    BasicBlock *BB = BasicBlock::Create(
        *TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);

    NamedValues.clear();
    for(auto &Arg : TheFunction->args()){
        AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, std::string(Arg.getName()));

        Builder->CreateStore(&Arg, Alloca);

        NamedValues[std::string(Arg.getName())] = Alloca;
    }

    if(Value *RetVal = Body->codegen()){
        Builder -> CreateRet(RetVal);

        verifyFunction(*TheFunction);

        TheFPM->run(*TheFunction, *TheFAM);

        return TheFunction;
    }

    TheFunction->eraseFromParent();
    return nullptr;
}


static void InitializeModuleAndManager(){
    // fprintf(stderr, "initializing");
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("KaleidoscopeJIT", *TheContext);
    TheModule->setDataLayout(TheJIT->getDataLayout());

    //用于生成并在合适地方插入llvm的中间代码
    Builder = std::make_unique<IRBuilder<>>(*TheContext);

    //管理和维持我们需要运行的优化
    TheFPM = std::make_unique<FunctionPassManager>();

    //以下四个分析管理器，粒度从小到大管理llvm的不同分析pass
    TheLAM = std::make_unique<LoopAnalysisManager>();
    TheFAM = std::make_unique<FunctionAnalysisManager>();
    TheCGAM = std::make_unique<CGSCCAnalysisManager>();
    TheMAM = std::make_unique<ModuleAnalysisManager>();

    ThePIC = std::make_unique<PassInstrumentationCallbacks>();
    TheSI = std::make_unique<StandardInstrumentations>(
        *TheContext, true
    );
    TheSI->registerCallbacks(*ThePIC, TheMAM.get());

    //以下四个transform pass都比较通用
    TheFPM->addPass(InstCombinePass());
    TheFPM->addPass(ReassociatePass());
    TheFPM->addPass(GVNPass());
    TheFPM->addPass(SimplifyCFGPass());

    PassBuilder PB;
    PB.registerModuleAnalyses(*TheMAM);
    PB.registerFunctionAnalyses(*TheFAM);
    PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}



static void HandleDefinition(){
    if(auto FnAST = ParseDefinition()){
        if(auto *FnIR = FnAST->codegen()){
            fprintf(stderr, "Read function definition:");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            ExitOnErr(TheJIT->addModule(
                ThreadSafeModule(std::move(TheModule), std::move(TheContext))
            ));
            InitializeModuleAndManager();
        }
    }else{
        getNextToken();
    }
}

static void HandleExtern(){
    if(auto ProtoAST = ParseExtern()){
        if(auto *FnIR = ProtoAST->codegen()){
            fprintf(stderr, "Read extern:");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
        }
    }else{
        getNextToken();
    }
}


static void HandleTopLevelExpression(){
    if(auto FnAST = ParseTopLevelExpr()){
        if(FnAST->codegen()){
            //创建一个资源跟踪器，以便我们执行这个匿名表达式后可以释放分配给它的空间
            auto RT = TheJIT->getMainJITDylib().createResourceTracker();


            auto TSM = ThreadSafeModule(
                std::move(TheModule), std::move(TheContext)
            );
            ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
            InitializeModuleAndManager();
            //查找“__anon_expr”这个符号
            auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));

            double (*FP)() = ExprSymbol.getAddress().toPtr<double(*)()>();
            fprintf(stderr, "Evaluated to %f\n", FP());

            //删除这个匿名表达式模块
            ExitOnErr(RT->remove());
        }
    }else{
        getNextToken();
    }
}

// top ::= definition || external || expression || ';'
static void MainLoop(){
    // fprintf(stderr, "mainloop");
    while(true){
        fprintf(stderr, "ready> ");
        switch (CurToken)
        {
        case token_eof:
            return;
        case ';':
            // fprintf(stderr, ";");
            getNextToken();
            break;
        case token_def:
            // fprintf(stderr, "def");
            HandleDefinition();
            break;
        case token_extern:
            HandleExtern();
            break;
        default:
            // fprintf(stderr, "%d",CurToken);
            HandleTopLevelExpression();
            break;
        }
    }
}

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" DLLEXPORT double putchard(double X) {
  fputc((char)X, stderr);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

int main(){

    //因为圆括号括起来的表达式已经被ParseParenExpr()方法解析了，
    //因此会被看作是一个整体，因此无需定义括号表达式的优先级
    BinopPrecedence['='] = 2;
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40;

    fprintf(stderr, "ready> ");
    getNextToken();

    // fprintf(stderr, "%d", CurToken);
    /*这三行代码是为了在LLVM中配置对本地平台的支持，
    确保LLVM可以生成本地的可执行代码、输出汇编代码并且解析汇编代码。 */
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    TheJIT = ExitOnErr(KaleidoscopeJIT::Create());

    InitializeModuleAndManager();

    MainLoop();


    auto TargetTriple = sys::getDefaultTargetTriple();
    TheModule->setTargetTriple(TargetTriple);
    std::string Error;
    auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

    if(!Target){
        errs()<<Error;
        return 1;
    }

    auto CPU = "apple-m3";
    auto Features = "";

    TargetOptions opt;
    auto TheTargetMachine = Target->createTargetMachine(
        TargetTriple, CPU, Features, opt, Reloc::PIC_
    );

    TheModule->setDataLayout(TheTargetMachine->createDataLayout());
    auto Filename = "output.o";
    std::error_code EC;
    raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

    if(EC){
        errs()<< "Could not open file: "<<EC.message();
        return 1;
    }

    legacy::PassManager pass;
    auto FileType = CodeGenFileType::ObjectFile;

    if(TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)){
        errs()<<"TheTargetMachine can't emit a file of this type.";
        return 1;
    }

    pass.run(*TheModule);
    dest.flush();

    outs() << "Wrote " << Filename << "\n";

    return 0;
}