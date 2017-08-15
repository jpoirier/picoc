/* picoc expression evaluator - a stack-based expression evaluation system
 * which handles operator precedence */
#include "interpreter.h"


/* whether evaluation is left to right for a given precedence level */
#define IS_LEFT_TO_RIGHT(p) ((p) != 2 && (p) != 14)
#define BRACKET_PRECEDENCE (20)

/* If the destination is not float, we can't assign a floating value to it,
    we need to convert it to integer instead */
#define ASSIGN_FP_OR_INT(value) \
        if (IS_FP(BottomValue)) { \
            ResultFP = ExpressionAssignFP(Parser, BottomValue, value); \
        } else { \
            ResultInt = ExpressionAssignInt(Parser, BottomValue,  (long)(value), false); \
            ResultIsInt = true; }

#define DEEP_PRECEDENCE (BRACKET_PRECEDENCE*1000)


/* local prototypes */
enum OperatorOrder {
    OrderNone,
    OrderPrefix,
    OrderInfix,
    OrderPostfix
};

/* a stack of expressions we use in evaluation */
struct ExpressionStack {
    struct ExpressionStack *Next;  /* the next lower item on the stack */
    struct Value *Val;  /* the value for this stack node */
    enum LexToken Op;  /* the operator */
    unsigned short Precedence;  /* the operator precedence of this node */
    unsigned char Order;  /* the evaluation order of this operator */
};

/* operator precedence definitions */
struct OpPrecedence {
    unsigned int PrefixPrecedence:4;
    unsigned int PostfixPrecedence:4;
    unsigned int InfixPrecedence:4;
    char *Name;
};

/* NOTE: the order of this array must correspond exactly to the order of
    these tokens in enum LexToken */
static struct OpPrecedence OperatorPrecedence[] = {
    /* TokenNone, */ {0, 0, 0, "none"},
    /* TokenComma, */ {0, 0, 0, ","},
    /* TokenAssign, */ {0, 0, 2, "="},
    /* TokenAddAssign, */ {0, 0, 2, "+="},
    /* TokenSubtractAssign, */ {0, 0, 2, "-="},
    /* TokenMultiplyAssign, */ {0, 0, 2, "*="},
    /* TokenDivideAssign, */ { 0, 0, 2, "/=" },
    /* TokenModulusAssign, */ { 0, 0, 2, "%=" },
    /* TokenShiftLeftAssign, */ {0, 0, 2, "<<="},
    /* TokenShiftRightAssign, */ { 0, 0, 2, ">>=" },
    /* TokenArithmeticAndAssign, */ { 0, 0, 2, "&=" },
    /* TokenArithmeticOrAssign, */ {0, 0, 2, "|="},
    /* TokenArithmeticExorAssign, */ { 0, 0, 2, "^=" },
    /* TokenQuestionMark, */ {0, 0, 3, "?"},
    /* TokenColon, */ {0, 0, 3, ":" },
    /* TokenLogicalOr, */ {0, 0, 4, "||"},
    /* TokenLogicalAnd, */ {0, 0, 5, "&&"},
    /* TokenArithmeticOr, */ {0, 0, 6, "|"},
    /* TokenArithmeticExor, */ {0, 0, 7, "^"},
    /* TokenAmpersand, */ {14, 0, 8, "&"},
    /* TokenEqual, */  {0, 0, 9, "=="},
    /* TokenNotEqual, */ {0, 0, 9, "!="},
    /* TokenLessThan, */ {0, 0, 10, "<"},
    /* TokenGreaterThan, */ {0, 0, 10, ">"},
    /* TokenLessEqual, */ {0, 0, 10, "<="},
    /* TokenGreaterEqual, */ {0, 0, 10, ">="},
    /* TokenShiftLeft, */ {0, 0, 11, "<<"},
    /* TokenShiftRight, */ {0, 0, 11, ">>"},
    /* TokenPlus, */ {14, 0, 12, "+"},
    /* TokenMinus, */ {14, 0, 12, "-"},
    /* TokenAsterisk, */ {14, 0, 13, "*"},
    /* TokenSlash, */ {0, 0, 13, "/"},
    /* TokenModulus, */ {0, 0, 13, "%"},
    /* TokenIncrement, */ {14, 15, 0, "++"},
    /* TokenDecrement, */ {14, 15, 0, "--"},
    /* TokenUnaryNot, */ {14, 0, 0, "!"},
    /* TokenUnaryExor, */ {14, 0, 0, "~"},
    /* TokenSizeof, */ {14, 0, 0, "sizeof"},
    /* TokenCast, */ {14, 0, 0, "cast"},
    /* TokenLeftSquareBracket, */ {0, 0, 15, "["},
    /* TokenRightSquareBracket, */ {0, 15, 0, "]"},
    /* TokenDot, */ {0, 0, 15, "."},
    /* TokenArrow, */ {0, 0, 15, "->"},
    /* TokenOpenBracket, */ {15, 0, 0, "("},
    /* TokenCloseBracket, */ {0, 15, 0, ")"}
};


#ifdef DEBUG_EXPRESSIONS
static void ExpressionStackShow(Picoc *pc, struct ExpressionStack *StackTop);
#endif
static int IsTypeToken(struct ParseState * Parser, enum LexToken t, struct Value * LexValue);
static long ExpressionAssignInt(struct ParseState *Parser, struct Value *DestValue, long FromInt, int After);
static double ExpressionAssignFP(struct ParseState *Parser, struct Value *DestValue, double FromFP);
static void ExpressionStackPushValueNode(struct ParseState *Parser, struct ExpressionStack **StackTop, struct Value *ValueLoc);
static struct Value *ExpressionStackPushValueByType(struct ParseState *Parser, struct ExpressionStack **StackTop, struct ValueType *PushType);
static void ExpressionStackPushValue(struct ParseState *Parser, struct ExpressionStack **StackTop, struct Value *PushValue);
static void ExpressionStackPushLValue(struct ParseState *Parser, struct ExpressionStack **StackTop, struct Value *PushValue, int Offset);
static void ExpressionStackPushDereference(struct ParseState *Parser, struct ExpressionStack **StackTop, struct Value *DereferenceValue);
static void ExpressionPushInt(struct ParseState *Parser, struct ExpressionStack **StackTop, long IntValue);
static void ExpressionPushFP(struct ParseState *Parser, struct ExpressionStack **StackTop, double FPValue);
static void ExpressionAssignToPointer(struct ParseState *Parser, struct Value *ToValue, struct Value *FromValue, const char *FuncName, int ParamNo, int AllowPointerCoercion);
static void ExpressionQuestionMarkOperator(struct ParseState *Parser, struct ExpressionStack **StackTop, struct Value *BottomValue, struct Value *TopValue);
static void ExpressionColonOperator(struct ParseState *Parser, struct ExpressionStack **StackTop, struct Value *BottomValue, struct Value *TopValue);
static void ExpressionPrefixOperator(struct ParseState *Parser, struct ExpressionStack **StackTop, enum LexToken Op, struct Value *TopValue);
static void ExpressionPostfixOperator(struct ParseState *Parser, struct ExpressionStack **StackTop, enum LexToken Op, struct Value *TopValue);
static void ExpressionInfixOperator(struct ParseState *Parser, struct ExpressionStack **StackTop, enum LexToken Op, struct Value *BottomValue, struct Value *TopValue);
static void ExpressionStackCollapse(struct ParseState *Parser, struct ExpressionStack **StackTop, int Precedence, int *IgnorePrecedence);
static void ExpressionStackPushOperator(struct ParseState *Parser, struct ExpressionStack **StackTop, enum OperatorOrder Order, enum LexToken Token, int Precedence);
static void ExpressionParseMacroCall(struct ParseState *Parser, struct ExpressionStack **StackTop, const char *MacroName, struct MacroDef *MDef);
static void ExpressionParseFunctionCall(struct ParseState *Parser, struct ExpressionStack **StackTop, const char *FuncName, int RunIt);


#ifdef DEBUG_EXPRESSIONS
/* show the contents of the expression stack */
void ExpressionStackShow(Picoc *pc, struct ExpressionStack *StackTop)
{
    printf("Expression stack [0x%lx,0x%lx]: ", (long)pc->HeapStackTop, (long)StackTop);

    while (StackTop != NULL) {
        if (StackTop->Order == OrderNone) {
            /* it's a value */
            if (StackTop->Val->IsLValue)
                printf("lvalue=");
            else
                printf("value=");

            switch (StackTop->Val->Typ->Base) {
            case TypeVoid:
                printf("void");
                break;
            case TypeInt:
                printf("%d:int", StackTop->Val->Val->Integer);
                break;
            case TypeShort:
                printf("%d:short", StackTop->Val->Val->ShortInteger);
                break;
            case TypeChar:
                printf("%d:char", StackTop->Val->Val->Character);
                break;
            case TypeLong:
                printf("%ld:long", StackTop->Val->Val->LongInteger);
                break;
            case TypeUnsignedShort:
                printf("%d:unsigned short", StackTop->Val->Val->UnsignedShortInteger);
                break;
            case TypeUnsignedInt:
                printf("%d:unsigned int", StackTop->Val->Val->UnsignedInteger);
                break;
            case TypeUnsignedLong:
                printf("%ld:unsigned long", StackTop->Val->Val->UnsignedLongInteger);
                break;
            case TypeFP:
                printf("%f:fp", StackTop->Val->Val->FP); 
                break;
            case TypeFunction:  
                printf("%s:function", StackTop->Val->Val->Identifier);
                break;
            case TypeMacro:
                printf("%s:macro", StackTop->Val->Val->Identifier);
                break;
            case TypePointer:
                if (StackTop->Val->Val->Pointer == NULL)
                    printf("ptr(NULL)");
                else if (StackTop->Val->Typ->FromType->Base == TypeChar)
                    printf("\"%s\":string", (char *)StackTop->Val->Val->Pointer);
                else
                    printf("ptr(0x%lx)", (long)StackTop->Val->Val->Pointer);
                break;
            case TypeArray:
                printf("array");
                break;
            case TypeStruct:
                printf("%s:struct", StackTop->Val->Val->Identifier);
                break;
            case TypeUnion:
                printf("%s:union", StackTop->Val->Val->Identifier);
                break;
            case TypeEnum:
                printf("%s:enum", StackTop->Val->Val->Identifier);
                break;
            case Type_Type:
                PrintType(StackTop->Val->Val->Typ, pc->CStdOut);
                printf(":type");
                break;
            default:
                printf("unknown");
                break;
            }
            printf("[0x%lx,0x%lx]", (long)StackTop, (long)StackTop->Val);
        } else {
            /* it's an operator */
            printf("op='%s' %s %d", OperatorPrecedence[(int)StackTop->Op].Name,
                (StackTop->Order == OrderPrefix) ?
                    "prefix" : ((StackTop->Order == OrderPostfix) ? "postfix" : "infix"),
                StackTop->Precedence);
            printf("[0x%lx]", (long)StackTop);
        }

        StackTop = StackTop->Next;
        if (StackTop != NULL)
            printf(", ");
    }

    printf("\n");
}
#endif

int IsTypeToken(struct ParseState *Parser, enum LexToken t,
    struct Value * LexValue)
{
    if (t >= TokenIntType && t <= TokenUnsignedType)
        return 1; /* base type */

    /* typedef'ed type? */
    if (t == TokenIdentifier) {
        /* see TypeParseFront, case TokenIdentifier and ParseTypedef */
        struct Value * VarValue;
        if (VariableDefined(Parser->pc, LexValue->Val->Pointer)) {
            VariableGet(Parser->pc, Parser, LexValue->Val->Pointer, &VarValue);
            if (VarValue->Typ == &Parser->pc->TypeType)
                return 1;
        }
    }

    return 0;
}

long ExpressionCoerceInteger(struct Value *Val)
{
    switch (Val->Typ->Base) {
    case TypeInt:
        return (long)Val->Val->Integer;
    case TypeChar:
        return (long)Val->Val->Character;
    case TypeShort:
        return (long)Val->Val->ShortInteger;
    case TypeLong:
        return (int64_t)Val->Val->LongInteger;
    case TypeUnsignedInt:
        return (unsigned long)Val->Val->UnsignedInteger;
    case TypeUnsignedShort:
        return (unsigned long)Val->Val->UnsignedShortInteger;
    case TypeUnsignedLong:
        return (uint64_t)Val->Val->UnsignedLongInteger;
    case TypeUnsignedChar:
        return (unsigned long)Val->Val->UnsignedCharacter;
    case TypePointer:
        return (uintptr_t)Val->Val->Pointer;
    case TypeFP:
        return (long)Val->Val->FP;
    default:
        return 0;
    }
}

unsigned long ExpressionCoerceUnsignedInteger(struct Value *Val)
{
    switch (Val->Typ->Base) {
    case TypeInt:
        return (unsigned long)Val->Val->Integer;
    case TypeChar:
        return (unsigned long)Val->Val->Character;
    case TypeShort:
        return (unsigned long)Val->Val->ShortInteger;
    case TypeLong:
        return (uint64_t)Val->Val->LongInteger;
    case TypeUnsignedInt:
        return (unsigned long)Val->Val->UnsignedInteger;
    case TypeUnsignedShort:
        return (unsigned long)Val->Val->UnsignedShortInteger;
    case TypeUnsignedLong:
        return (uint64_t)Val->Val->UnsignedLongInteger;
    case TypeUnsignedChar:
        return (unsigned long)Val->Val->UnsignedCharacter;
    case TypePointer:
        return (uintptr_t)Val->Val->Pointer;
    case TypeFP:
        return (unsigned long)Val->Val->FP;
    default:
        return 0;
    }
}

double ExpressionCoerceFP(struct Value *Val)
{
    switch (Val->Typ->Base) {
    case TypeInt:
        return (double)Val->Val->Integer;
    case TypeChar:
        return (double)Val->Val->Character;
    case TypeShort:
        return (double)Val->Val->ShortInteger;
    case TypeLong:
        return (double)Val->Val->LongInteger;
    case TypeUnsignedInt:
        return (double)Val->Val->UnsignedInteger;
    case TypeUnsignedShort:
        return (double)Val->Val->UnsignedShortInteger;
    case TypeUnsignedLong:
        return (double)Val->Val->UnsignedLongInteger;
    case TypeUnsignedChar:
        return (double)Val->Val->UnsignedCharacter;
    case TypeFP:
        return Val->Val->FP;
    default:
        return 0.0;
    }
}

/* assign an integer value */
long ExpressionAssignInt(struct ParseState *Parser, struct Value *DestValue,
    long FromInt, int After)
{
    long Result;

    if (!DestValue->IsLValue)
        ProgramFail(Parser, "can't assign to this");

    if (After)
        Result = ExpressionCoerceInteger(DestValue);
    else
        Result = FromInt;

    switch (DestValue->Typ->Base) {
    case TypeInt:
        DestValue->Val->Integer = (int)FromInt;
        break;
    case TypeShort:
        DestValue->Val->ShortInteger = (short)FromInt;
        break;
    case TypeChar:
        DestValue->Val->Character = (char)FromInt;
        break;
    case TypeLong:
        DestValue->Val->LongInteger = (int64_t)FromInt;
        break;
    case TypeUnsignedInt:
        DestValue->Val->UnsignedInteger = (unsigned int)FromInt;
        break;
    case TypeUnsignedShort:
        DestValue->Val->UnsignedShortInteger = (unsigned short)FromInt;
        break;
    case TypeUnsignedLong:
        DestValue->Val->UnsignedLongInteger = (int64_t)FromInt;
        break;
    case TypeUnsignedChar:
        DestValue->Val->UnsignedCharacter = (unsigned char)FromInt;
        break;
    default:
        break;
    }
    return Result;
}

/* assign a floating point value */
double ExpressionAssignFP(struct ParseState *Parser, struct Value *DestValue,
    double FromFP)
{
    if (!DestValue->IsLValue)
        ProgramFail(Parser, "can't assign to this");

    DestValue->Val->FP = FromFP;
    return FromFP;
}

/* push a node on to the expression stack */
void ExpressionStackPushValueNode(struct ParseState *Parser,
    struct ExpressionStack **StackTop, struct Value *ValueLoc)
{
    struct ExpressionStack *StackNode = VariableAlloc(Parser->pc, Parser,
                                        sizeof(*StackNode), false);
    StackNode->Next = *StackTop;
    StackNode->Val = ValueLoc;
    *StackTop = StackNode;
#ifdef FANCY_ERROR_MESSAGES
    StackNode->Line = Parser->Line;
    StackNode->CharacterPos = Parser->CharacterPos;
#endif
#ifdef DEBUG_EXPRESSIONS
    ExpressionStackShow(Parser->pc, *StackTop);
#endif
}

/* push a blank value on to the expression stack by type */
struct Value *ExpressionStackPushValueByType(struct ParseState *Parser,
    struct ExpressionStack **StackTop, struct ValueType *PushType)
{
    struct Value *ValueLoc = VariableAllocValueFromType(Parser->pc, Parser,
            PushType, false, NULL, false);
    ExpressionStackPushValueNode(Parser, StackTop, ValueLoc);

    return ValueLoc;
}

/* push a value on to the expression stack */
void ExpressionStackPushValue(struct ParseState *Parser,
    struct ExpressionStack **StackTop, struct Value *PushValue)
{
    struct Value *ValueLoc = VariableAllocValueAndCopy(Parser->pc, Parser,
        PushValue, false);
    ExpressionStackPushValueNode(Parser, StackTop, ValueLoc);
}

void ExpressionStackPushLValue(struct ParseState *Parser,
    struct ExpressionStack **StackTop, struct Value *PushValue, int Offset)
{
    struct Value *ValueLoc = VariableAllocValueShared(Parser, PushValue);
    ValueLoc->Val = (void *)((char *)ValueLoc->Val + Offset);
    ExpressionStackPushValueNode(Parser, StackTop, ValueLoc);
}

void ExpressionStackPushDereference(struct ParseState *Parser,
    struct ExpressionStack **StackTop, struct Value *DereferenceValue)
{
    int Offset;
    int DerefIsLValue;
    struct Value *DerefVal;
    struct Value *ValueLoc;
    struct ValueType *DerefType;
    void *DerefDataLoc = VariableDereferencePointer(DereferenceValue, &DerefVal,
        &Offset, &DerefType, &DerefIsLValue);
    if (DerefDataLoc == NULL)
        ProgramFail(Parser, "NULL pointer dereference");

    ValueLoc = VariableAllocValueFromExistingData(Parser, DerefType,
                    (union AnyValue*)DerefDataLoc, DerefIsLValue, DerefVal);
    ExpressionStackPushValueNode(Parser, StackTop, ValueLoc);
}

void ExpressionPushInt(struct ParseState *Parser,
            struct ExpressionStack **StackTop, long IntValue)
{
    struct Value *ValueLoc = VariableAllocValueFromType(Parser->pc, Parser,
                            &Parser->pc->IntType, false, NULL, false);
    ValueLoc->Val->Integer = IntValue;
    ExpressionStackPushValueNode(Parser, StackTop, ValueLoc);
}

void ExpressionPushFP(struct ParseState *Parser,
    struct ExpressionStack **StackTop, double FPValue)
{
    struct Value *ValueLoc = VariableAllocValueFromType(Parser->pc, Parser,
                                 &Parser->pc->FPType, false, NULL, false);
    ValueLoc->Val->FP = FPValue;
    ExpressionStackPushValueNode(Parser, StackTop, ValueLoc);
}

/* assign to a pointer */
void ExpressionAssignToPointer(struct ParseState *Parser, struct Value *ToValue,
    struct Value *FromValue, const char *FuncName, int ParamNo,
    int AllowPointerCoercion)
{
    struct ValueType *PointedToType = ToValue->Typ->FromType;

    if (FromValue->Typ == ToValue->Typ ||
            FromValue->Typ == Parser->pc->VoidPtrType ||
            (ToValue->Typ == Parser->pc->VoidPtrType &&
            FromValue->Typ->Base == TypePointer))
        ToValue->Val->Pointer = FromValue->Val->Pointer; /* plain old pointer assignment */
    else if (FromValue->Typ->Base == TypeArray &&
            (PointedToType == FromValue->Typ->FromType ||
            ToValue->Typ == Parser->pc->VoidPtrType)) {
        /* the form is: blah *x = array of blah */
        ToValue->Val->Pointer = (void *)&FromValue->Val->ArrayMem[0];
    } else if (FromValue->Typ->Base == TypePointer &&
                FromValue->Typ->FromType->Base == TypeArray &&
               (PointedToType == FromValue->Typ->FromType->FromType ||
                ToValue->Typ == Parser->pc->VoidPtrType) ) {
        /* the form is: blah *x = pointer to array of blah */
        ToValue->Val->Pointer = VariableDereferencePointer(FromValue, NULL,
            NULL, NULL, NULL);
    } else if (IS_NUMERIC_COERCIBLE(FromValue) &&
            ExpressionCoerceInteger(FromValue) == 0) {
        /* null pointer assignment */
        ToValue->Val->Pointer = NULL;
    } else if (AllowPointerCoercion && IS_NUMERIC_COERCIBLE(FromValue)) {
        /* assign integer to native pointer */
        ToValue->Val->Pointer =
            (void*)(unsigned long)ExpressionCoerceUnsignedInteger(FromValue);
    } else if (AllowPointerCoercion && FromValue->Typ->Base == TypePointer) {
        /* assign a pointer to a pointer to a different type */
        ToValue->Val->Pointer = FromValue->Val->Pointer;
    } else
        AssignFail(Parser, "%t from %t", ToValue->Typ, FromValue->Typ, 0, 0,
            FuncName, ParamNo);
}

/* assign any kind of value */
void ExpressionAssign(struct ParseState *Parser, struct Value *DestValue,
    struct Value *SourceValue, int Force, const char *FuncName, int ParamNo,
    int AllowPointerCoercion)
{
    if (!DestValue->IsLValue && !Force)
        AssignFail(Parser, "not an lvalue", NULL, NULL, 0, 0, FuncName, ParamNo);

    if (IS_NUMERIC_COERCIBLE(DestValue) &&
            !IS_NUMERIC_COERCIBLE_PLUS_POINTERS(SourceValue, AllowPointerCoercion))
        AssignFail(Parser, "%t from %t", DestValue->Typ, SourceValue->Typ, 0, 0,
            FuncName, ParamNo);

    switch (DestValue->Typ->Base) {
    case TypeInt:
        DestValue->Val->Integer = (int)ExpressionCoerceInteger(SourceValue);
        break;
    case TypeShort:
        DestValue->Val->ShortInteger = (short)ExpressionCoerceInteger(SourceValue);
        break;
    case TypeChar:
        DestValue->Val->Character = (char)ExpressionCoerceInteger(SourceValue);
        break;
    case TypeLong:
        DestValue->Val->LongInteger = (long)ExpressionCoerceInteger(SourceValue);
        break;
    case TypeUnsignedInt:
        DestValue->Val->UnsignedInteger =
            (unsigned int)ExpressionCoerceUnsignedInteger(SourceValue);
        break;
    case TypeUnsignedShort:
        DestValue->Val->UnsignedShortInteger =
            (unsigned short)ExpressionCoerceUnsignedInteger(SourceValue);
        break;
    case TypeUnsignedLong:
        DestValue->Val->UnsignedLongInteger =
            (unsigned long)ExpressionCoerceUnsignedInteger(SourceValue);
        break;
    case TypeUnsignedChar:
        DestValue->Val->UnsignedCharacter =
            (unsigned char)ExpressionCoerceUnsignedInteger(SourceValue);
        break;
    case TypeFP:
        if (!IS_NUMERIC_COERCIBLE_PLUS_POINTERS(SourceValue, AllowPointerCoercion))
            AssignFail(Parser, "%t from %t", DestValue->Typ, SourceValue->Typ,
                0, 0, FuncName, ParamNo);
        DestValue->Val->FP = (double)ExpressionCoerceFP(SourceValue);
        break;
    case TypePointer:
        ExpressionAssignToPointer(Parser, DestValue, SourceValue, FuncName,
            ParamNo, AllowPointerCoercion);
        break;
    case TypeArray:
        if (SourceValue->Typ->Base == TypeArray && DestValue->Typ->ArraySize == 0) {
            /* destination array is unsized - need to resize the destination
                array to the same size as the source array */
            DestValue->Typ = SourceValue->Typ;
            VariableRealloc(Parser, DestValue, TypeSizeValue(DestValue, false));

            if (DestValue->LValueFrom != NULL) {
                /* copy the resized value back to the LValue */
                DestValue->LValueFrom->Val = DestValue->Val;
                DestValue->LValueFrom->AnyValOnHeap = DestValue->AnyValOnHeap;
            }
        }
        /* char array = "abcd" */
        if (DestValue->Typ->FromType->Base == TypeChar &&
                SourceValue->Typ->Base == TypePointer &&
                SourceValue->Typ->FromType->Base == TypeChar) {
            if (DestValue->Typ->ArraySize == 0) { /* char x[] = "abcd", x is unsized */
                int Size = strlen(SourceValue->Val->Pointer) + 1;
#ifdef DEBUG_ARRAY_INITIALIZER
                PRINT_SOURCE_POS();
                fprintf(stderr, "str size: %d\n", Size);
#endif
                DestValue->Typ = TypeGetMatching(Parser->pc, Parser,
                            DestValue->Typ->FromType, DestValue->Typ->Base,
                            Size, DestValue->Typ->Identifier, true);
                VariableRealloc(Parser, DestValue, TypeSizeValue(DestValue,
                    false));
            }
            /* else, it's char x[10] = "abcd" */

#ifdef DEBUG_ARRAY_INITIALIZER
            PRINT_SOURCE_POS();
            fprintf(stderr, "char[%d] from char* (len=%d)\n",
                    DestValue->Typ->ArraySize,
                    strlen(SourceValue->Val->Pointer));
#endif
            memcpy((void*)DestValue->Val, SourceValue->Val->Pointer,
                TypeSizeValue(DestValue, false));
            break;
        }

        if (DestValue->Typ != SourceValue->Typ)
            AssignFail(Parser, "%t from %t", DestValue->Typ, SourceValue->Typ,
                0, 0, FuncName, ParamNo);

        if (DestValue->Typ->ArraySize != SourceValue->Typ->ArraySize)
            AssignFail(Parser, "from an array of size %d to one of size %d",
                NULL, NULL, DestValue->Typ->ArraySize,
                SourceValue->Typ->ArraySize, FuncName, ParamNo);

        memcpy((void*)DestValue->Val, (void*)SourceValue->Val,
                TypeSizeValue(DestValue, false));
        break;
    case TypeStruct:
    case TypeUnion:
        if (DestValue->Typ != SourceValue->Typ)
            AssignFail(Parser, "%t from %t", DestValue->Typ, SourceValue->Typ,
                        0, 0, FuncName, ParamNo);
        memcpy((void*)DestValue->Val, (void*)SourceValue->Val,
                TypeSizeValue(SourceValue, false));
        break;
    default:
        AssignFail(Parser, "%t", DestValue->Typ, NULL, 0, 0, FuncName, ParamNo);
        break;
    }
}

/* evaluate the first half of a ternary operator x ? y : z */
void ExpressionQuestionMarkOperator(struct ParseState *Parser,
    struct ExpressionStack **StackTop, struct Value *BottomValue,
    struct Value *TopValue)
{
    if (!IS_NUMERIC_COERCIBLE(TopValue))
        ProgramFail(Parser, "first argument to '?' should be a number");

    if (ExpressionCoerceInteger(TopValue)) {
        /* the condition's true, return the BottomValue */
        ExpressionStackPushValue(Parser, StackTop, BottomValue);
    } else {
        /* the condition's false, return void */
        ExpressionStackPushValueByType(Parser, StackTop, &Parser->pc->VoidType);
    }
}

/* evaluate the second half of a ternary operator x ? y : z */
void ExpressionColonOperator(struct ParseState *Parser,
    struct ExpressionStack **StackTop, struct Value *BottomValue,
    struct Value *TopValue)
{
    if (TopValue->Typ->Base == TypeVoid) {
        /* invoke the "else" part - return the BottomValue */
        ExpressionStackPushValue(Parser, StackTop, BottomValue);
    } else {
        /* it was a "then" - return the TopValue */
        ExpressionStackPushValue(Parser, StackTop, TopValue);
    }
}

/* evaluate a prefix operator */
void ExpressionPrefixOperator(struct ParseState *Parser,
    struct ExpressionStack **StackTop, enum LexToken Op, struct Value *TopValue)
{
    struct Value *Result;
    union AnyValue *ValPtr;
    struct ValueType *Typ;

#ifdef DEBUG_EXPRESSIONS
    printf("ExpressionPrefixOperator()\n");
#endif

    switch (Op) {
    case TokenAmpersand:
        if (!TopValue->IsLValue)
            ProgramFail(Parser, "can't get the address of this");

        ValPtr = TopValue->Val;
        Result = VariableAllocValueFromType(Parser->pc, Parser,
                    TypeGetMatching(Parser->pc, Parser, TopValue->Typ,
                        TypePointer, 0, Parser->pc->StrEmpty, true),
                    false, NULL, false);
        Result->Val->Pointer = (void*)ValPtr;
        ExpressionStackPushValueNode(Parser, StackTop, Result);
        break;
    case TokenAsterisk:
        if(StackTop != NULL && (*StackTop) != NULL && (*StackTop)->Op == TokenSizeof)
            /* ignored */
            ExpressionStackPushValueByType(Parser, StackTop, TopValue->Typ);
        else
            ExpressionStackPushDereference(Parser, StackTop, TopValue);
        break;
    case TokenSizeof:
        /* return the size of the argument */
        if (TopValue->Typ == &Parser->pc->TypeType)
            Typ = TopValue->Val->Typ;
        else
            Typ = TopValue->Typ;
        if (Typ->FromType != NULL && Typ->FromType->Base == TypeStruct)
            Typ = Typ->FromType;
        ExpressionPushInt(Parser, StackTop, TypeSize(Typ, Typ->ArraySize, true));
        break;
    default:
        /* an arithmetic operator */
        if (TopValue->Typ == &Parser->pc->FPType) {
            /* floating point prefix arithmetic */
            double ResultFP = 0.0;
            switch (Op) {
            case TokenPlus:
                ResultFP = TopValue->Val->FP;
                break;
            case TokenMinus:
                ResultFP = -TopValue->Val->FP;
                break;
            case TokenIncrement:
                ResultFP = ExpressionAssignFP(Parser, TopValue,
                    TopValue->Val->FP+1);
                break;
            case TokenDecrement:
                ResultFP = ExpressionAssignFP(Parser, TopValue,
                    TopValue->Val->FP-1);
                break;
            case TokenUnaryNot:
                ResultFP = !TopValue->Val->FP;
                break;
            default:
                ProgramFail(Parser, "invalid operation");
                break;
            }
            ExpressionPushFP(Parser, StackTop, ResultFP);
        } else if (IS_NUMERIC_COERCIBLE(TopValue)) {
            /* integer prefix arithmetic */
            int64_t ResultInt = 0;
            int64_t TopInt = ExpressionCoerceInteger(TopValue);
            switch (Op) {
            case TokenPlus:
                ResultInt = TopInt;
                break;
            case TokenMinus:
                ResultInt = -TopInt;
                break;
            case TokenIncrement:
                ResultInt = ExpressionAssignInt(Parser, TopValue,
                    TopInt+1, false);
                break;
            case TokenDecrement:
                ResultInt = ExpressionAssignInt(Parser, TopValue,
                    TopInt-1, false);
                break;
            case TokenUnaryNot:
                ResultInt = !TopInt;
                break;
            case TokenUnaryExor:
                ResultInt = ~TopInt;
                break;
            default:
                ProgramFail(Parser, "invalid operation");
                break;
            }
            ExpressionPushInt(Parser, StackTop, ResultInt);
        } else if (TopValue->Typ->Base == TypePointer) {
            /* pointer prefix arithmetic */
            int Size = TypeSize(TopValue->Typ->FromType, 0, true);
            struct Value *StackValue;
            void *ResultPtr = 0;
            if (Op != TokenUnaryNot && TopValue->Val->Pointer == NULL)
                ProgramFail(Parser, "a. invalid use of a NULL pointer");
            if (!TopValue->IsLValue)
                ProgramFail(Parser, "can't assign to this");
            switch (Op) {
            case TokenIncrement:
                ResultPtr = TopValue->Val->Pointer =
                    (void*)((char*)TopValue->Val->Pointer+Size);
                break;
            case TokenDecrement:
                ResultPtr = TopValue->Val->Pointer =
                    (void*)((char*)TopValue->Val->Pointer-Size);
                break;
            case TokenUnaryNot:
                /* conditionally checking a pointer's value, we only want
                    to change the stack value (ResultPtr) and not the pointer's
                    actual value  */
                TopValue->Val->Pointer =
                    (void*)((char*)TopValue->Val->Pointer);
                    ResultPtr = TopValue->Val->Pointer ? NULL : (void*)0x01;
                break;
            default:
                ProgramFail(Parser, "invalid operation");
                break;
            }
            StackValue = ExpressionStackPushValueByType(Parser, StackTop,
                TopValue->Typ);
            StackValue->Val->Pointer = ResultPtr;
        } else {
            ProgramFail(Parser, "invalid operation");
        }
        break;
    }
}

/* evaluate a postfix operator */
void ExpressionPostfixOperator(struct ParseState *Parser,
    struct ExpressionStack **StackTop, enum LexToken Op, struct Value *TopValue)
{
#ifdef DEBUG_EXPRESSIONS
    printf("ExpressionPostfixOperator()\n");
#endif

    if (TopValue->Typ == &Parser->pc->FPType) {
        /* floating point prefix arithmetic */
        double ResultFP = 0.0;

        switch (Op) {
        case TokenIncrement:
            ResultFP = ExpressionAssignFP(Parser, TopValue, TopValue->Val->FP+1);
            break;
        case TokenDecrement:
            ResultFP = ExpressionAssignFP(Parser, TopValue, TopValue->Val->FP-1);
            break;
        default:
            ProgramFail(Parser, "invalid operation");
            break;
        }
        ExpressionPushFP(Parser, StackTop, ResultFP);
    } else if (IS_NUMERIC_COERCIBLE(TopValue)) {
        int64_t ResultInt = 0;
        int64_t TopInt = ExpressionCoerceInteger(TopValue);
        switch (Op) {
        case TokenIncrement:
            ResultInt = ExpressionAssignInt(Parser, TopValue, TopInt+1, true);
            break;
        case TokenDecrement:
            ResultInt = ExpressionAssignInt(Parser, TopValue, TopInt-1, true);
            break;
        case TokenRightSquareBracket:
            ProgramFail(Parser, "not supported");
            break;  /* XXX */
        case TokenCloseBracket:
            ProgramFail(Parser, "not supported");
            break;  /* XXX */
        default:
            ProgramFail(Parser, "invalid operation");
            break;
        }
        ExpressionPushInt(Parser, StackTop, ResultInt);
    } else if (TopValue->Typ->Base == TypePointer) {
        /* pointer postfix arithmetic */
        int Size = TypeSize(TopValue->Typ->FromType, 0, true);
        struct Value *StackValue;
        void *OrigPointer = TopValue->Val->Pointer;

        if (TopValue->Val->Pointer == NULL)
            ProgramFail(Parser, "b. invalid use of a NULL pointer");

        if (!TopValue->IsLValue)
            ProgramFail(Parser, "can't assign to this");

        switch (Op) {
        case TokenIncrement:
            TopValue->Val->Pointer = (void*)((char*)TopValue->Val->Pointer+Size);
            break;
        case TokenDecrement:
            TopValue->Val->Pointer = (void*)((char*)TopValue->Val->Pointer-Size);
            break;
        default:
            ProgramFail(Parser, "invalid operation");
            break;
        }
        StackValue = ExpressionStackPushValueByType(Parser, StackTop,
            TopValue->Typ);
        StackValue->Val->Pointer = OrigPointer;
    } else
        ProgramFail(Parser, "invalid operation");
}

/* evaluate an infix operator */
void ExpressionInfixOperator(struct ParseState *Parser,
    struct ExpressionStack **StackTop, enum LexToken Op,
    struct Value *BottomValue, struct Value *TopValue)
{
    long ResultInt = 0;
    struct Value *StackValue;
    void *Pointer;

#ifdef DEBUG_EXPRESSIONS
    printf("ExpressionInfixOperator()\n");
#endif

    if (BottomValue == NULL || TopValue == NULL)
        ProgramFail(Parser, "invalid expression");

    if (Op == TokenLeftSquareBracket) {
        /* array index */
        int ArrayIndex;
        struct Value *Result = NULL;

        if (!IS_NUMERIC_COERCIBLE(TopValue))
            ProgramFail(Parser, "array index must be an integer");

        ArrayIndex = ExpressionCoerceInteger(TopValue);

        /* make the array element result */
        switch (BottomValue->Typ->Base) {
        case TypeArray:
            Result = VariableAllocValueFromExistingData(Parser,
            BottomValue->Typ->FromType,
            (union AnyValue*)(&BottomValue->Val->ArrayMem[0] +
                TypeSize(BottomValue->Typ,
            ArrayIndex, true)),
            BottomValue->IsLValue, BottomValue->LValueFrom);
            break;
        case TypePointer: Result = VariableAllocValueFromExistingData(Parser,
            BottomValue->Typ->FromType,
            (union AnyValue*)((char*)BottomValue->Val->Pointer +
                TypeSize(BottomValue->Typ->FromType,
            0, true) * ArrayIndex),
            BottomValue->IsLValue, BottomValue->LValueFrom);
            break;
        default:
            ProgramFail(Parser, "this %t is not an array", BottomValue->Typ);
            break;
        }

        ExpressionStackPushValueNode(Parser, StackTop, Result);
    } else if (Op == TokenQuestionMark)
        ExpressionQuestionMarkOperator(Parser, StackTop, TopValue, BottomValue);
    else if (Op == TokenColon)
        ExpressionColonOperator(Parser, StackTop, TopValue, BottomValue);
    else if ((TopValue->Typ == &Parser->pc->FPType &&
                 BottomValue->Typ == &Parser->pc->FPType) ||
              (TopValue->Typ == &Parser->pc->FPType
                    && IS_NUMERIC_COERCIBLE(BottomValue)) ||
              (IS_NUMERIC_COERCIBLE(TopValue)
                && BottomValue->Typ == &Parser->pc->FPType) ) {
        /* floating point infix arithmetic */
        int ResultIsInt = false;
        double ResultFP = 0.0;
        double TopFP = (TopValue->Typ == &Parser->pc->FPType) ?
            TopValue->Val->FP : (double)ExpressionCoerceInteger(TopValue);
        double BottomFP = (BottomValue->Typ == &Parser->pc->FPType) ?
            BottomValue->Val->FP : (double)ExpressionCoerceInteger(BottomValue);

        switch (Op) {
        case TokenAssign:
            ASSIGN_FP_OR_INT(TopFP);
            break;
        case TokenAddAssign:
            ASSIGN_FP_OR_INT(BottomFP + TopFP);
            break;
        case TokenSubtractAssign:
            ASSIGN_FP_OR_INT(BottomFP - TopFP);
            break;
        case TokenMultiplyAssign:
            ASSIGN_FP_OR_INT(BottomFP * TopFP);
            break;
        case TokenDivideAssign:
            ASSIGN_FP_OR_INT(BottomFP / TopFP);
            break;
        case TokenEqual:
            ResultInt = BottomFP == TopFP;
            ResultIsInt = true;
            break;
        case TokenNotEqual:
            ResultInt = BottomFP != TopFP;
            ResultIsInt = true;
            break;
        case TokenLessThan:
            ResultInt = BottomFP < TopFP;
            ResultIsInt = true;
            break;
        case TokenGreaterThan:
            ResultInt = BottomFP > TopFP;
            ResultIsInt = true;
            break;
        case TokenLessEqual:
            ResultInt = BottomFP <= TopFP;
            ResultIsInt = true;
            break;
        case TokenGreaterEqual:
            ResultInt = BottomFP >= TopFP;
            ResultIsInt = true;
            break;
        case TokenPlus:
            ResultFP = BottomFP + TopFP;
            break;
        case TokenMinus:
            ResultFP = BottomFP - TopFP;
            break;
        case TokenAsterisk:
            ResultFP = BottomFP * TopFP;
            break;
        case TokenSlash:
            ResultFP = BottomFP / TopFP;
            break;
        default:
            ProgramFail(Parser, "invalid operation");
            break;
        }

        if (ResultIsInt)
            ExpressionPushInt(Parser, StackTop, ResultInt);
        else
            ExpressionPushFP(Parser, StackTop, ResultFP);
    } else if (IS_NUMERIC_COERCIBLE(TopValue) && IS_NUMERIC_COERCIBLE(BottomValue)) {
        /* integer operation */
        long TopInt = ExpressionCoerceInteger(TopValue);
        long BottomInt = ExpressionCoerceInteger(BottomValue);
        switch (Op) {
        case TokenAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue, TopInt, false);
            break;
        case TokenAddAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt + TopInt, false);
            break;
        case TokenSubtractAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt-TopInt, false);
            break;
        case TokenMultiplyAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt*TopInt, false);
            break;
        case TokenDivideAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt/TopInt, false);
            break;
        case TokenModulusAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt%TopInt, false);
            break;
        case TokenShiftLeftAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt<<TopInt, false);
            break;
        case TokenShiftRightAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt>>TopInt, false);
            break;
        case TokenArithmeticAndAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt&TopInt, false);
            break;
        case TokenArithmeticOrAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt|TopInt, false);
            break;
        case TokenArithmeticExorAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt^TopInt, false);
            break;
        case TokenLogicalOr:
            ResultInt = BottomInt || TopInt;
            break;
        case TokenLogicalAnd:
            ResultInt = BottomInt && TopInt;
            break;
        case TokenArithmeticOr:
            ResultInt = BottomInt | TopInt;
            break;
        case TokenArithmeticExor:
            ResultInt = BottomInt ^ TopInt;
            break;
        case TokenAmpersand:
            ResultInt = BottomInt & TopInt;
            break;
        case TokenEqual:
            ResultInt = BottomInt == TopInt;
            break;
        case TokenNotEqual:
            ResultInt = BottomInt != TopInt;
            break;
        case TokenLessThan:
            ResultInt = BottomInt < TopInt;
            break;
        case TokenGreaterThan:
            ResultInt = BottomInt > TopInt;
            break;
        case TokenLessEqual:
            ResultInt = BottomInt <= TopInt;
            break;
        case TokenGreaterEqual:
            ResultInt = BottomInt >= TopInt;
            break;
        case TokenShiftLeft:
            ResultInt = BottomInt << TopInt;
            break;
        case TokenShiftRight:
            ResultInt = BottomInt >> TopInt;
            break;
        case TokenPlus:
            ResultInt = BottomInt + TopInt;
            break;
        case TokenMinus:
            ResultInt = BottomInt - TopInt;
            break;
        case TokenAsterisk:
            ResultInt = BottomInt * TopInt;
            break;
        case TokenSlash:
            ResultInt = BottomInt / TopInt;
            break;
        case TokenModulus:
            ResultInt = BottomInt % TopInt;
            break;
        default:
            ProgramFail(Parser, "invalid operation");
            break;
        }
        ExpressionPushInt(Parser, StackTop, ResultInt);
    } else if (BottomValue->Typ->Base == TypePointer &&
            IS_NUMERIC_COERCIBLE(TopValue)) {
        /* pointer/integer infix arithmetic */
        long TopInt = ExpressionCoerceInteger(TopValue);

        if (Op == TokenEqual || Op == TokenNotEqual) {
            /* comparison to a NULL pointer */
            if (TopInt != 0)
                ProgramFail(Parser, "invalid operation");

            if (Op == TokenEqual)
                ExpressionPushInt(Parser, StackTop,
                    BottomValue->Val->Pointer == NULL);
            else
                ExpressionPushInt(Parser, StackTop,
                    BottomValue->Val->Pointer != NULL);
        } else if (Op == TokenPlus || Op == TokenMinus) {
            /* pointer arithmetic */
            int Size = TypeSize(BottomValue->Typ->FromType, 0, true);

            Pointer = BottomValue->Val->Pointer;
            if (Pointer == NULL)
                ProgramFail(Parser, "c. invalid use of a NULL pointer");

            if (Op == TokenPlus)
                Pointer = (void*)((char*)Pointer + TopInt * Size);
            else
                Pointer = (void*)((char*)Pointer - TopInt * Size);

            StackValue = ExpressionStackPushValueByType(Parser, StackTop,
                BottomValue->Typ);
            StackValue->Val->Pointer = Pointer;
        } else if (Op == TokenAssign && TopInt == 0) {
            /* assign a NULL pointer */
            HeapUnpopStack(Parser->pc, sizeof(struct Value));
            ExpressionAssign(Parser, BottomValue, TopValue, false, NULL, 0, false);
            ExpressionStackPushValueNode(Parser, StackTop, BottomValue);
        } else if (Op == TokenAddAssign || Op == TokenSubtractAssign) {
            /* pointer arithmetic */
            int Size = TypeSize(BottomValue->Typ->FromType, 0, true);

            Pointer = BottomValue->Val->Pointer;
            if (Pointer == NULL)
                ProgramFail(Parser, "d. invalid use of a NULL pointer");

            if (Op == TokenAddAssign)
                Pointer = (void*)((char*)Pointer + TopInt * Size);
            else
                Pointer = (void*)((char*)Pointer - TopInt * Size);

            HeapUnpopStack(Parser->pc, sizeof(struct Value));
            BottomValue->Val->Pointer = Pointer;
            ExpressionStackPushValueNode(Parser, StackTop, BottomValue);
        } else
            ProgramFail(Parser, "invalid operation");
    } else if (BottomValue->Typ->Base == TypePointer &&
            TopValue->Typ->Base == TypePointer && Op != TokenAssign) {
        /* pointer/pointer operations */
        char *TopLoc = (char*)TopValue->Val->Pointer;
        char *BottomLoc = (char*)BottomValue->Val->Pointer;

        switch (Op) {
        case TokenEqual:
            ExpressionPushInt(Parser, StackTop, BottomLoc == TopLoc);
            break;
        case TokenNotEqual:
            ExpressionPushInt(Parser, StackTop, BottomLoc != TopLoc);
            break;
        case TokenMinus:
            ExpressionPushInt(Parser, StackTop, BottomLoc - TopLoc);
            break;
        default:
            ProgramFail(Parser, "invalid operation");
            break;
        }
    } else if (Op == TokenAssign) {
        /* assign a non-numeric type */
        HeapUnpopStack(Parser->pc, sizeof(struct Value));
        /* XXX - possible bug if lvalue is a temp value and takes more
            than sizeof(struct Value) */
        ExpressionAssign(Parser, BottomValue, TopValue, false, NULL, 0, false);
        ExpressionStackPushValueNode(Parser, StackTop, BottomValue);
    } else if (Op == TokenCast) {
        /* cast a value to a different type */
        /* XXX - possible bug if the destination type takes more than s
            izeof(struct Value) + sizeof(struct ValueType *) */
        struct Value *ValueLoc = ExpressionStackPushValueByType(Parser, StackTop,
            BottomValue->Val->Typ);
        ExpressionAssign(Parser, ValueLoc, TopValue, true, NULL, 0, true);
    } else
        ProgramFail(Parser, "invalid operation");
}

/* take the contents of the expression stack and compute the top until
    there's nothing greater than the given precedence */
void ExpressionStackCollapse(struct ParseState *Parser,
    struct ExpressionStack **StackTop, int Precedence, int *IgnorePrecedence)
{
    int FoundPrecedence = Precedence;
    struct Value *TopValue;
    struct Value *BottomValue;
    struct ExpressionStack *TopStackNode = *StackTop;
    struct ExpressionStack *TopOperatorNode;

#ifdef DEBUG_EXPRESSIONS
    printf("ExpressionStackCollapse(%d):\n", Precedence);
    ExpressionStackShow(Parser->pc, *StackTop);
#endif
    while (TopStackNode != NULL && TopStackNode->Next != NULL &&
            FoundPrecedence >= Precedence) {
        /* find the top operator on the stack */
        if (TopStackNode->Order == OrderNone)
            TopOperatorNode = TopStackNode->Next;
        else
            TopOperatorNode = TopStackNode;

        FoundPrecedence = TopOperatorNode->Precedence;

        /* does it have a high enough precedence? */
        if (FoundPrecedence >= Precedence && TopOperatorNode != NULL) {
            /* execute this operator */
            switch (TopOperatorNode->Order) {
            case OrderPrefix:
                /* prefix evaluation */
#ifdef DEBUG_EXPRESSIONS
                printf("prefix evaluation\n");
#endif
                TopValue = TopStackNode->Val;

                /* pop the value and then the prefix operator - assume
                    they'll still be there until we're done */
                HeapPopStack(Parser->pc, NULL,
                    sizeof(struct ExpressionStack) +
                    sizeof(struct Value) +
                    TypeStackSizeValue(TopValue));
                HeapPopStack(Parser->pc, TopOperatorNode,
                    sizeof(struct ExpressionStack));
                *StackTop = TopOperatorNode->Next;

                /* do the prefix operation */
                if (Parser->Mode == RunModeRun /* && FoundPrecedence < *IgnorePrecedence */) {
                    /* run the operator */
                    ExpressionPrefixOperator(Parser, StackTop,
                        TopOperatorNode->Op, TopValue);
                } else {
                    /* we're not running it so just return 0 */
                    ExpressionPushInt(Parser, StackTop, 0);
                }
                break;
            case OrderPostfix:
                /* postfix evaluation */
#ifdef DEBUG_EXPRESSIONS
                printf("postfix evaluation\n");
#endif
                TopValue = TopStackNode->Next->Val;

                /* pop the postfix operator and then the value - assume
                    they'll still be there until we're done */
                HeapPopStack(Parser->pc, NULL, sizeof(struct ExpressionStack));
                HeapPopStack(Parser->pc, TopValue,
                    sizeof(struct ExpressionStack) +
                    sizeof(struct Value) +
                    TypeStackSizeValue(TopValue));
                *StackTop = TopStackNode->Next->Next;

                /* do the postfix operation */
                if (Parser->Mode == RunModeRun /* && FoundPrecedence < *IgnorePrecedence */) {
                    /* run the operator */
                    ExpressionPostfixOperator(Parser, StackTop,
                        TopOperatorNode->Op, TopValue);
                } else {
                    /* we're not running it so just return 0 */
                    ExpressionPushInt(Parser, StackTop, 0);
                }
                break;
            case OrderInfix:
                /* infix evaluation */
#ifdef DEBUG_EXPRESSIONS
                printf("infix evaluation\n");
#endif
                TopValue = TopStackNode->Val;
                if (TopValue != NULL) {
                    BottomValue = TopOperatorNode->Next->Val;

                    /* pop a value, the operator and another value - assume
                        they'll still be there until we're done */
                    HeapPopStack(Parser->pc, NULL,
                        sizeof(struct ExpressionStack) +
                        sizeof(struct Value) +
                        TypeStackSizeValue(TopValue));
                    HeapPopStack(Parser->pc, NULL,
                        sizeof(struct ExpressionStack));
                    HeapPopStack(Parser->pc, BottomValue,
                        sizeof(struct ExpressionStack) +
                        sizeof(struct Value) +
                        TypeStackSizeValue(BottomValue));
                    *StackTop = TopOperatorNode->Next->Next;

                    /* do the infix operation */
                    if (Parser->Mode == RunModeRun /* && FoundPrecedence <= *IgnorePrecedence */) {
                        /* run the operator */
                        ExpressionInfixOperator(Parser, StackTop,
                            TopOperatorNode->Op, BottomValue, TopValue);
                    } else {
                        /* we're not running it so just return 0 */
                        ExpressionPushInt(Parser, StackTop, 0);
                    }
                } else
                    FoundPrecedence = -1;
                break;
            case OrderNone:
            default:
                /* this should never happen */
                assert(TopOperatorNode->Order != OrderNone);
                break;
            }

            /* if we've returned above the ignored precedence level
                turn ignoring off */
            if (FoundPrecedence <= *IgnorePrecedence)
                *IgnorePrecedence = DEEP_PRECEDENCE;
        }
#ifdef DEBUG_EXPRESSIONS
        ExpressionStackShow(Parser->pc, *StackTop);
#endif
        TopStackNode = *StackTop;
    }
#ifdef DEBUG_EXPRESSIONS
    printf("ExpressionStackCollapse() finished\n");
    ExpressionStackShow(Parser->pc, *StackTop);
#endif
}

/* push an operator on to the expression stack */
void ExpressionStackPushOperator(struct ParseState *Parser,
    struct ExpressionStack **StackTop, enum OperatorOrder Order,
    enum LexToken Token, int Precedence)
{
    struct ExpressionStack *StackNode = VariableAlloc(Parser->pc, Parser,
        sizeof(*StackNode), false);
    StackNode->Next = *StackTop;
    StackNode->Order = Order;
    StackNode->Op = Token;
    StackNode->Precedence = Precedence;
    *StackTop = StackNode;
#ifdef DEBUG_EXPRESSIONS
    printf("ExpressionStackPushOperator()\n");
#endif
#ifdef FANCY_ERROR_MESSAGES
    StackNode->Line = Parser->Line;
    StackNode->CharacterPos = Parser->CharacterPos;
#endif
#ifdef DEBUG_EXPRESSIONS
    ExpressionStackShow(Parser->pc, *StackTop);
#endif
}

/* do the '.' and '->' operators */
void ExpressionGetStructElement(struct ParseState *Parser,
    struct ExpressionStack **StackTop, enum LexToken Token)
{
    struct Value *Ident;

    /* get the identifier following the '.' or '->' */
    if (LexGetToken(Parser, &Ident, true) != TokenIdentifier)
        ProgramFail(Parser, "need an structure or union member after '%s'",
            (Token == TokenDot) ? "." : "->");

    if (Parser->Mode == RunModeRun) {
        /* look up the struct element */
        struct Value *ParamVal = (*StackTop)->Val;
        struct Value *StructVal = ParamVal;
        struct ValueType *StructType = ParamVal->Typ;
        char *DerefDataLoc = (char *)ParamVal->Val;
        struct Value *MemberValue = NULL;
        struct Value *Result;

        /* if we're doing '->' dereference the struct pointer first */
        if (Token == TokenArrow)
            DerefDataLoc = VariableDereferencePointer(ParamVal, &StructVal,
                NULL, &StructType, NULL);

        if (StructType->Base != TypeStruct && StructType->Base != TypeUnion)
            ProgramFail(Parser,
                "can't use '%s' on something that's not a struct or union %s : it's a %t",
                (Token == TokenDot) ? "." : "->",
                (Token == TokenArrow) ? "pointer" : "", ParamVal->Typ);

        if (!TableGet(StructType->Members, Ident->Val->Identifier,
                &MemberValue, NULL, NULL, NULL))
            ProgramFail(Parser, "doesn't have a member called '%s'",
                Ident->Val->Identifier);

        /* pop the value - assume it'll still be there until we're done */
        HeapPopStack(Parser->pc, ParamVal,
            sizeof(struct ExpressionStack) +
            sizeof(struct Value) +
            TypeStackSizeValue(StructVal));
        *StackTop = (*StackTop)->Next;

        /* make the result value for this member only */
        Result = VariableAllocValueFromExistingData(Parser, MemberValue->Typ,
            (void*)(DerefDataLoc + MemberValue->Val->Integer), true,
            (StructVal != NULL) ? StructVal->LValueFrom : NULL);
        ExpressionStackPushValueNode(Parser, StackTop, Result);
    }
}

/* parse an expression with operator precedence */
int ExpressionParse(struct ParseState *Parser, struct Value **Result)
{
    int PrefixState = true;
    int Done = false;
    int BracketPrecedence = 0;
    int LocalPrecedence;
    int Precedence = 0;
    int IgnorePrecedence = DEEP_PRECEDENCE;
    int TernaryDepth = 0;
    struct Value *LexValue;
    struct ExpressionStack *StackTop = NULL;

#ifdef DEBUG_EXPRESSIONS
    printf("ExpressionParse():\n");
#endif

    do {
        struct ParseState PreState;
        enum LexToken Token;

        ParserCopy(&PreState, Parser);
        Token = LexGetToken(Parser, &LexValue, true);
        if ((((int)Token > TokenComma && (int)Token <= (int)TokenOpenBracket) ||
               (Token == TokenCloseBracket && BracketPrecedence != 0)) &&
               (Token != TokenColon || TernaryDepth > 0)) {
            /* it's an operator with precedence */
            if (PrefixState) {
                /* expect a prefix operator */
                if (OperatorPrecedence[(int)Token].PrefixPrecedence == 0)
                    ProgramFail(Parser, "operator not expected here");

                LocalPrecedence = OperatorPrecedence[(int)Token].PrefixPrecedence;
                Precedence = BracketPrecedence + LocalPrecedence;

                if (Token == TokenOpenBracket) {
                    /* it's either a new bracket level or a cast */
                    enum LexToken BracketToken = LexGetToken(Parser, &LexValue, false);
                    if (IsTypeToken(Parser, BracketToken, LexValue) &&
                            (StackTop == NULL || StackTop->Op != TokenSizeof)) {
                        /* it's a cast - get the new type */
                        struct ValueType *CastType;
                        char *CastIdentifier;
                        struct Value *CastTypeValue;

                        TypeParse(Parser, &CastType, &CastIdentifier, NULL);
                        if (LexGetToken(Parser, &LexValue, true) != TokenCloseBracket)
                            ProgramFail(Parser, "brackets not closed");

                        /* scan and collapse the stack to the precedence of
                            this infix cast operator, then push */
                        Precedence = BracketPrecedence +
                            OperatorPrecedence[(int)TokenCast].PrefixPrecedence;

                        ExpressionStackCollapse(Parser, &StackTop, Precedence+1,
                            &IgnorePrecedence);
                        CastTypeValue = VariableAllocValueFromType(Parser->pc,
                            Parser, &Parser->pc->TypeType, false, NULL, false);
                        CastTypeValue->Val->Typ = CastType;
                        ExpressionStackPushValueNode(Parser, &StackTop, CastTypeValue);
                        ExpressionStackPushOperator(Parser, &StackTop, OrderInfix,
                            TokenCast, Precedence);
                    } else {
                        /* boost the bracket operator precedence */
                        BracketPrecedence += BRACKET_PRECEDENCE;
                    }
                } else {
                    /* scan and collapse the stack to the precedence of
                        this operator, then push */

                    /* take some extra care for double prefix operators,
                        e.g. x = - -5, or x = **y */
                    int NextToken = LexGetToken(Parser, NULL, false);
                    int TempPrecedenceBoost = 0;
                    if (NextToken > TokenComma && NextToken < TokenOpenBracket) {
                        int NextPrecedence =
                            OperatorPrecedence[(int)NextToken].PrefixPrecedence;

                        /* two prefix operators with equal precedence? make
                            sure the innermost one runs first */
                        /* XXX - probably not correct, but can't find a
                            test that fails at this */
                        if (LocalPrecedence == NextPrecedence)
                            TempPrecedenceBoost = -1;
                    }

                    ExpressionStackCollapse(Parser, &StackTop, Precedence,
                        &IgnorePrecedence);
                    ExpressionStackPushOperator(Parser, &StackTop, OrderPrefix,
                        Token, Precedence + TempPrecedenceBoost);
                }
            } else {
                /* expect an infix or postfix operator */
                if (OperatorPrecedence[(int)Token].PostfixPrecedence != 0) {
                    switch (Token) {
                    case TokenCloseBracket:
                    case TokenRightSquareBracket:
                        if (BracketPrecedence == 0) {
                            /* assume this bracket is after the end of the
                                expression */
                            ParserCopy(Parser, &PreState);
                            Done = true;
                        } else {
                            /* collapse to the bracket precedence */
                            ExpressionStackCollapse(Parser, &StackTop,
                                BracketPrecedence, &IgnorePrecedence);
                            BracketPrecedence -= BRACKET_PRECEDENCE;
                        }
                        break;
                    default:
                        /* scan and collapse the stack to the precedence of
                            this operator, then push */
                        Precedence = BracketPrecedence +
                            OperatorPrecedence[(int)Token].PostfixPrecedence;
                        ExpressionStackCollapse(Parser, &StackTop, Precedence,
                            &IgnorePrecedence);
                        ExpressionStackPushOperator(Parser, &StackTop,
                            OrderPostfix, Token, Precedence);
                        break;
                    }
                } else if (OperatorPrecedence[(int)Token].InfixPrecedence != 0) {
                    /* scan and collapse the stack, then push */
                    Precedence = BracketPrecedence +
                        OperatorPrecedence[(int)Token].InfixPrecedence;

                    /* for right to left order, only go down to the next
                        higher precedence so we evaluate it in reverse order */
                    /* for left to right order, collapse down to this precedence
                        so we evaluate it in forward order */
                    if (IS_LEFT_TO_RIGHT(OperatorPrecedence[(int)Token].InfixPrecedence))
                        ExpressionStackCollapse(Parser, &StackTop, Precedence,
                            &IgnorePrecedence);
                    else
                        ExpressionStackCollapse(Parser, &StackTop, Precedence+1,
                            &IgnorePrecedence);

                    if (Token == TokenDot || Token == TokenArrow) {
                        /* this operator is followed by a struct element so
                            handle it as a special case */
                        ExpressionGetStructElement(Parser, &StackTop, Token);
                    } else {
                        /* if it's a && or || operator we may not need to
                            evaluate the right hand side of the expression */
                        if ((Token == TokenLogicalOr || Token == TokenLogicalAnd) &&
                                IS_NUMERIC_COERCIBLE(StackTop->Val)) {
                            long LHSInt = ExpressionCoerceInteger(StackTop->Val);
                            if (((Token == TokenLogicalOr && LHSInt) ||
                                    (Token == TokenLogicalAnd && !LHSInt)) &&
                                 (IgnorePrecedence > Precedence) )
                                IgnorePrecedence = Precedence;
                        }

                        /* push the operator on the stack */
                        ExpressionStackPushOperator(Parser, &StackTop,
                            OrderInfix, Token, Precedence);
                        PrefixState = true;

                        switch (Token) {
                        case TokenQuestionMark:
                            TernaryDepth++;
                            break;
                        case TokenColon:
                            TernaryDepth--;
                            break;
                        default:
                            break;
                        }
                    }

                    /* treat an open square bracket as an infix array index
                        operator followed by an open bracket */
                    if (Token == TokenLeftSquareBracket) {
                        /* boost the bracket operator precedence, then push */
                        BracketPrecedence += BRACKET_PRECEDENCE;
                    }
                } else
                    ProgramFail(Parser, "operator not expected here");
            }
        } else if (Token == TokenIdentifier) {
            /* it's a variable, function or a macro */
            if (!PrefixState)
                ProgramFail(Parser, "identifier not expected here");

            if (LexGetToken(Parser, NULL, false) == TokenOpenBracket) {
                ExpressionParseFunctionCall(Parser, &StackTop,
                    LexValue->Val->Identifier,
                    Parser->Mode == RunModeRun && Precedence < IgnorePrecedence);
            } else {
                if (Parser->Mode == RunModeRun /* && Precedence < IgnorePrecedence */) {
                    struct Value *VariableValue = NULL;

                    VariableGet(Parser->pc, Parser, LexValue->Val->Identifier,
                        &VariableValue);
                    if (VariableValue->Typ->Base == TypeMacro) {
                        /* evaluate a macro as a kind of simple subroutine */
                        struct ParseState MacroParser;
                        struct Value *MacroResult;

                        ParserCopy(&MacroParser, &VariableValue->Val->MacroDef.Body);
                        MacroParser.Mode = Parser->Mode;
                        if (VariableValue->Val->MacroDef.NumParams != 0)
                            ProgramFail(&MacroParser, "macro arguments missing");

                        if (!ExpressionParse(&MacroParser, &MacroResult) ||
                                LexGetToken(&MacroParser, NULL, false) !=
                                    TokenEndOfFunction)
                            ProgramFail(&MacroParser, "expression expected");

                        ExpressionStackPushValueNode(Parser, &StackTop, MacroResult);
                    } else if (VariableValue->Typ == &Parser->pc->VoidType)
                        ProgramFail(Parser, "a void value isn't much use here");
                    else
                        ExpressionStackPushLValue(Parser, &StackTop,
                        VariableValue, 0); /* it's a value variable */
                } else /* push a dummy value */
                    ExpressionPushInt(Parser, &StackTop, 0);

            }

             /* if we've successfully ignored the RHS turn ignoring off */
            if (Precedence <= IgnorePrecedence)
                IgnorePrecedence = DEEP_PRECEDENCE;

            PrefixState = false;
        } else if ((int)Token > TokenCloseBracket &&
                                        (int)Token <= TokenCharacterConstant) {
            /* it's a value of some sort, push it */
            if (!PrefixState)
                ProgramFail(Parser, "value not expected here");

            PrefixState = false;
            ExpressionStackPushValue(Parser, &StackTop, LexValue);
        } else if (IsTypeToken(Parser, Token, LexValue)) {
            /* it's a type. push it on the stack like a value.
                this is used in sizeof() */
            struct ValueType *Typ;
            char *Identifier;
            struct Value *TypeValue;

            if (!PrefixState)
                ProgramFail(Parser, "type not expected here");

            PrefixState = false;
            ParserCopy(Parser, &PreState);
            TypeParse(Parser, &Typ, &Identifier, NULL);
            TypeValue = VariableAllocValueFromType(Parser->pc, Parser,
                &Parser->pc->TypeType, false, NULL, false);
            TypeValue->Val->Typ = Typ;
            ExpressionStackPushValueNode(Parser, &StackTop, TypeValue);
        } else {
            /* it isn't a token from an expression */
            ParserCopy(Parser, &PreState);
            Done = true;
        }
    } while (!Done);

    /* check that brackets have been closed */
    if (BracketPrecedence > 0)
        ProgramFail(Parser, "brackets not closed");

    /* scan and collapse the stack to precedence 0 */
    ExpressionStackCollapse(Parser, &StackTop, 0, &IgnorePrecedence);

    /* fix up the stack and return the result if we're in run mode */
    if (StackTop != NULL) {
        /* all that should be left is a single value on the stack */
        if (Parser->Mode == RunModeRun) {
            if (StackTop->Order != OrderNone || StackTop->Next != NULL)
                ProgramFail(Parser, "invalid expression");

            *Result = StackTop->Val;
            HeapPopStack(Parser->pc, StackTop, sizeof(struct ExpressionStack));
        } else
            HeapPopStack(Parser->pc, StackTop->Val,
                sizeof(struct ExpressionStack) +
                sizeof(struct Value) +
                TypeStackSizeValue(StackTop->Val));
    }

#ifdef DEBUG_EXPRESSIONS
    printf("ExpressionParse() done\n\n");
    ExpressionStackShow(Parser->pc, StackTop);
#endif
    return StackTop != NULL;
}

/* do a parameterized macro call */
void ExpressionParseMacroCall(struct ParseState *Parser,
    struct ExpressionStack **StackTop, const char *MacroName,
    struct MacroDef *MDef)
{
    int ArgCount;
    enum LexToken Token;
    struct Value *ReturnValue = NULL;
    struct Value *Param;
    struct Value **ParamArray = NULL;

    if (Parser->Mode == RunModeRun) {
        /* create a stack frame for this macro */
        /* largest return type there is */
        ExpressionStackPushValueByType(Parser, StackTop, &Parser->pc->FPType);
        ReturnValue = (*StackTop)->Val;
        HeapPushStackFrame(Parser->pc);
        ParamArray = HeapAllocStack(Parser->pc,
            sizeof(struct Value*)*MDef->NumParams);
        if (ParamArray == NULL)
            ProgramFail(Parser, "(ExpressionParseMacroCall) out of memory");
    } else
        ExpressionPushInt(Parser, StackTop, 0);

    /* parse arguments */
    ArgCount = 0;
    do {
        if (ExpressionParse(Parser, &Param)) {
            if (Parser->Mode == RunModeRun) {
                if (ArgCount < MDef->NumParams)
                    ParamArray[ArgCount] = Param;
                else
                    ProgramFail(Parser, "too many arguments to %s()", MacroName);
            }

            ArgCount++;
            Token = LexGetToken(Parser, NULL, true);
            if (Token != TokenComma && Token != TokenCloseBracket)
                ProgramFail(Parser, "comma expected");
        } else {
            /* end of argument list? */
            Token = LexGetToken(Parser, NULL, true);
            if (Token != TokenCloseBracket)
                ProgramFail(Parser, "bad argument");
        }

    } while (Token != TokenCloseBracket);

    if (Parser->Mode == RunModeRun) {
        /* evaluate the macro */
        struct ParseState MacroParser;
        int Count;
        struct Value *EvalValue;

        if (ArgCount < MDef->NumParams)
            ProgramFail(Parser, "not enough arguments to '%s'", MacroName);

        if (MDef->Body.Pos == NULL)
            ProgramFail(Parser,
                "ExpressionParseMacroCall MacroName: '%s' is undefined",
                MacroName);

        ParserCopy(&MacroParser, &MDef->Body);
        MacroParser.Mode = Parser->Mode;
        VariableStackFrameAdd(Parser, MacroName, 0);
        Parser->pc->TopStackFrame->NumParams = ArgCount;
        Parser->pc->TopStackFrame->ReturnValue = ReturnValue;
        for (Count = 0; Count < MDef->NumParams; Count++)
            VariableDefine(Parser->pc, Parser, MDef->ParamName[Count],
                ParamArray[Count], NULL, true);

        ExpressionParse(&MacroParser, &EvalValue);
        ExpressionAssign(Parser, ReturnValue, EvalValue, true, MacroName, 0, false);
        VariableStackFramePop(Parser);
        HeapPopStackFrame(Parser->pc);
    }
}

/* do a function call */
void ExpressionParseFunctionCall(struct ParseState *Parser,
    struct ExpressionStack **StackTop, const char *FuncName, int RunIt)
{
    int ArgCount;
    enum LexToken Token = LexGetToken(Parser, NULL, true);    /* open bracket */
    enum RunMode OldMode = Parser->Mode;
    struct Value *ReturnValue = NULL;
    struct Value *FuncValue = NULL;
    struct Value *Param;
    struct Value **ParamArray = NULL;

    if (RunIt) {
        /* get the function definition */
        VariableGet(Parser->pc, Parser, FuncName, &FuncValue);

        if (FuncValue->Typ->Base == TypeMacro) {
            /* this is actually a macro, not a function */
            ExpressionParseMacroCall(Parser, StackTop, FuncName,
                &FuncValue->Val->MacroDef);
            return;
        }

        if (FuncValue->Typ->Base != TypeFunction)
            ProgramFail(Parser, "%t is not a function - can't call",
                FuncValue->Typ);

        ExpressionStackPushValueByType(Parser, StackTop,
            FuncValue->Val->FuncDef.ReturnType);
        ReturnValue = (*StackTop)->Val;
        HeapPushStackFrame(Parser->pc);
        ParamArray = HeapAllocStack(Parser->pc,
            sizeof(struct Value*)*FuncValue->Val->FuncDef.NumParams);
        if (ParamArray == NULL)
            ProgramFail(Parser, "(ExpressionParseFunctionCall) out of memory");
    } else {
        ExpressionPushInt(Parser, StackTop, 0);
        Parser->Mode = RunModeSkip;
    }

    /* parse arguments */
    ArgCount = 0;
    do {
        if (RunIt && ArgCount < FuncValue->Val->FuncDef.NumParams)
            ParamArray[ArgCount] = VariableAllocValueFromType(Parser->pc, Parser,
                FuncValue->Val->FuncDef.ParamType[ArgCount], false, NULL, false);

        if (ExpressionParse(Parser, &Param)) {
            if (RunIt) {
                if (ArgCount < FuncValue->Val->FuncDef.NumParams) {
                    ExpressionAssign(Parser, ParamArray[ArgCount], Param, true,
                        FuncName, ArgCount+1, false);
                    VariableStackPop(Parser, Param);
                } else {
                    if (!FuncValue->Val->FuncDef.VarArgs)
                        ProgramFail(Parser, "too many arguments to %s()", FuncName);
                }
            }

            ArgCount++;
            Token = LexGetToken(Parser, NULL, true);
            if (Token != TokenComma && Token != TokenCloseBracket)
                ProgramFail(Parser, "comma expected");
        } else {
            /* end of argument list? */
            Token = LexGetToken(Parser, NULL, true);
            if (Token != TokenCloseBracket)
                ProgramFail(Parser, "bad argument");
        }

    } while (Token != TokenCloseBracket);

    if (RunIt) {
        /* run the function */
        if (ArgCount < FuncValue->Val->FuncDef.NumParams)
            ProgramFail(Parser, "not enough arguments to '%s'", FuncName);

        if (FuncValue->Val->FuncDef.Intrinsic == NULL) {
            /* run a user-defined function */
            int Count;
            int OldScopeID = Parser->ScopeID;
            struct ParseState FuncParser;

            if (FuncValue->Val->FuncDef.Body.Pos == NULL)
                ProgramFail(Parser,
                    "ExpressionParseFunctionCall FuncName: '%s' is undefined",
                    FuncName);

            ParserCopy(&FuncParser, &FuncValue->Val->FuncDef.Body);
            VariableStackFrameAdd(Parser, FuncName,
                FuncValue->Val->FuncDef.Intrinsic ? FuncValue->Val->FuncDef.NumParams : 0);
            Parser->pc->TopStackFrame->NumParams = ArgCount;
            Parser->pc->TopStackFrame->ReturnValue = ReturnValue;

            /* Function parameters should not go out of scope */
            Parser->ScopeID = -1;

            for (Count = 0; Count < FuncValue->Val->FuncDef.NumParams; Count++)
                VariableDefine(Parser->pc, Parser,
                    FuncValue->Val->FuncDef.ParamName[Count], ParamArray[Count],
                    NULL, true);

            Parser->ScopeID = OldScopeID;

            if (ParseStatement(&FuncParser, true) != ParseResultOk)
                ProgramFail(&FuncParser, "function body expected");

            if (RunIt) {
                if (FuncParser.Mode == RunModeRun &&
                        FuncValue->Val->FuncDef.ReturnType != &Parser->pc->VoidType)
                    ProgramFail(&FuncParser,
                        "no value returned from a function returning %t",
                        FuncValue->Val->FuncDef.ReturnType);

                else if (FuncParser.Mode == RunModeGoto)
                    ProgramFail(&FuncParser, "couldn't find goto label '%s'",
                        FuncParser.SearchGotoLabel);
            }

            VariableStackFramePop(Parser);
        } else {
            // FIXME: too many parameters?
            FuncValue->Val->FuncDef.Intrinsic(Parser, ReturnValue, ParamArray,
                                              ArgCount);
        }

        HeapPopStackFrame(Parser->pc);
    }

    Parser->Mode = OldMode;
}

/* parse an expression */
long ExpressionParseInt(struct ParseState *Parser)
{
    long Result = 0;
    struct Value *Val;

    if (!ExpressionParse(Parser, &Val))
        ProgramFail(Parser, "expression expected");

    if (Parser->Mode == RunModeRun) {
        if (!IS_NUMERIC_COERCIBLE_PLUS_POINTERS(Val, true))
            ProgramFail(Parser, "integer value expected instead of %t", Val->Typ);

        Result = ExpressionCoerceInteger(Val);
        VariableStackPop(Parser, Val);
    }

    return Result;
}

