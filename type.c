/* picoc data type module. This manages a tree of data types and has facilities
 * for parsing data types. */

#include "interpreter.h"


static struct ValueType *TypeAdd(Picoc *pc, struct ParseState *Parser,
    struct ValueType *ParentType, enum BaseType Base, int ArraySize,
    const char *Identifier, int Sizeof, int AlignBytes);
static void TypeAddBaseType(Picoc *pc, struct ValueType *TypeNode,
    enum BaseType Base, int Sizeof, int AlignBytes);
static void TypeCleanupNode(Picoc *pc, struct ValueType *Typ);
static void TypeParseStruct(struct ParseState *Parser, struct ValueType **Typ,
    int IsStruct);
static void TypeParseEnum(struct ParseState *Parser, struct ValueType **Typ);
static struct ValueType *TypeParseBack(struct ParseState *Parser,
    struct ValueType *FromType);



/* some basic types */
static int PointerAlignBytes;
static int IntAlignBytes;


/* add a new type to the set of types we know about */
struct ValueType *TypeAdd(Picoc *pc, struct ParseState *Parser,
    struct ValueType *ParentType, enum BaseType Base, int ArraySize,
    const char *Identifier, int Sizeof, int AlignBytes)
{
    struct ValueType *NewType = VariableAlloc(pc, Parser,
        sizeof(struct ValueType), true);
    NewType->Base = Base;
    NewType->ArraySize = ArraySize;
    NewType->Sizeof = Sizeof;
    NewType->AlignBytes = AlignBytes;
    NewType->Identifier = Identifier;
    NewType->Members = NULL;
    NewType->FromType = ParentType;
    NewType->DerivedTypeList = NULL;
    NewType->OnHeap = true;
    NewType->Next = ParentType->DerivedTypeList;
    ParentType->DerivedTypeList = NewType;

    return NewType;
}

/* given a parent type, get a matching derived type and make one if necessary.
 * Identifier should be registered with the shared string table. */
struct ValueType *TypeGetMatching(Picoc *pc, struct ParseState *Parser,
    struct ValueType *ParentType, enum BaseType Base, int ArraySize,
    const char *Identifier, int AllowDuplicates)
{
    int Sizeof;
    int AlignBytes;
    struct ValueType *ThisType = ParentType->DerivedTypeList;
    while (ThisType != NULL && (ThisType->Base != Base ||
            ThisType->ArraySize != ArraySize || ThisType->Identifier != Identifier))
        ThisType = ThisType->Next;

    if (ThisType != NULL) {
        if (AllowDuplicates)
            return ThisType;
        else
            ProgramFail(Parser, "data type '%s' is already defined", Identifier);
    }

    switch (Base) {
    case TypePointer:
        Sizeof = sizeof(void*);
        AlignBytes = PointerAlignBytes;
        break;
    case TypeArray:
        Sizeof = ArraySize * ParentType->Sizeof;
        AlignBytes = ParentType->AlignBytes;
        break;
    case TypeEnum:
        Sizeof = sizeof(int);
        AlignBytes = IntAlignBytes;
        break;
    default:
        Sizeof = 0; AlignBytes = 0;
        break;  /* structs and unions will get bigger
                    when we add members to them */
    }

    return TypeAdd(pc, Parser, ParentType, Base, ArraySize, Identifier, Sizeof,
        AlignBytes);
}

/* stack space used by a value */
int TypeStackSizeValue(struct Value *Val)
{
    if (Val != NULL && Val->ValOnStack)
        return TypeSizeValue(Val, false);
    else
        return 0;
}

/* memory used by a value */
int TypeSizeValue(struct Value *Val, int Compact)
{
    if (IS_INTEGER_NUMERIC(Val) && !Compact)
        return sizeof(ALIGN_TYPE);  /* allow some extra room for type extension */
    else if (Val->Typ->Base != TypeArray)
        return Val->Typ->Sizeof;
    else
        return Val->Typ->FromType->Sizeof * Val->Typ->ArraySize;
}

/* memory used by a variable given its type and array size */
int TypeSize(struct ValueType *Typ, int ArraySize, int Compact)
{
    if (IS_INTEGER_NUMERIC_TYPE(Typ) && !Compact)
        return sizeof(ALIGN_TYPE);  /* allow some extra room for type extension */
    else if (Typ->Base != TypeArray)
        return Typ->Sizeof;
    else
        return Typ->FromType->Sizeof * ArraySize;
}

/* add a base type */
void TypeAddBaseType(Picoc *pc, struct ValueType *TypeNode, enum BaseType Base,
            int Sizeof, int AlignBytes)
{
    TypeNode->Base = Base;
    TypeNode->ArraySize = 0;
    TypeNode->Sizeof = Sizeof;
    TypeNode->AlignBytes = AlignBytes;
    TypeNode->Identifier = pc->StrEmpty;
    TypeNode->Members = NULL;
    TypeNode->FromType = NULL;
    TypeNode->DerivedTypeList = NULL;
    TypeNode->OnHeap = false;
    TypeNode->Next = pc->UberType.DerivedTypeList;
    pc->UberType.DerivedTypeList = TypeNode;
}

/* initialize the type system */
void TypeInit(Picoc *pc)
{
    struct IntAlign {char x; int y;} ia;
    struct ShortAlign {char x; short y;} sa;
    struct CharAlign {char x; char y;} ca;
    struct LongAlign {char x; long y;} la;
    struct DoubleAlign {char x; double y;} da;
    struct PointerAlign {char x; void *y;} pa;

    IntAlignBytes = (char*)&ia.y - &ia.x;
    PointerAlignBytes = (char*)&pa.y - &pa.x;

    pc->UberType.DerivedTypeList = NULL;
    TypeAddBaseType(pc, &pc->IntType, TypeInt, sizeof(int), IntAlignBytes);
    TypeAddBaseType(pc, &pc->ShortType, TypeShort, sizeof(short),
        (char*)&sa.y - &sa.x);
    TypeAddBaseType(pc, &pc->CharType, TypeChar, sizeof(char),
        (char*)&ca.y - &ca.x);
    TypeAddBaseType(pc, &pc->LongType, TypeLong, sizeof(long),
        (char*)&la.y - &la.x);
    TypeAddBaseType(pc, &pc->UnsignedIntType, TypeUnsignedInt,
        sizeof(unsigned int), IntAlignBytes);
    TypeAddBaseType(pc, &pc->UnsignedShortType, TypeUnsignedShort,
        sizeof(unsigned short), (char*)&sa.y - &sa.x);
    TypeAddBaseType(pc, &pc->UnsignedLongType, TypeUnsignedLong,
        sizeof(unsigned long), (char*)&la.y - &la.x);
    TypeAddBaseType(pc, &pc->UnsignedCharType, TypeUnsignedChar,
        sizeof(unsigned char), (char*)&ca.y - &ca.x);
    TypeAddBaseType(pc, &pc->VoidType, TypeVoid, 0, 1);
    TypeAddBaseType(pc, &pc->FunctionType, TypeFunction, sizeof(int),
        IntAlignBytes);
    TypeAddBaseType(pc, &pc->MacroType, TypeMacro, sizeof(int), IntAlignBytes);
    TypeAddBaseType(pc, &pc->GotoLabelType, TypeGotoLabel, 0, 1);
    TypeAddBaseType(pc, &pc->FPType, TypeFP, sizeof(double),
        (char*)&da.y - &da.x);
    TypeAddBaseType(pc, &pc->TypeType, Type_Type, sizeof(double),
    (char*)&da.y - &da.x);  /* must be large enough to cast to a double */
    pc->CharArrayType = TypeAdd(pc, NULL, &pc->CharType, TypeArray, 0,
        pc->StrEmpty, sizeof(char), (char*)&ca.y - &ca.x);
    pc->CharPtrType = TypeAdd(pc, NULL, &pc->CharType, TypePointer, 0,
        pc->StrEmpty, sizeof(void*), PointerAlignBytes);
    pc->CharPtrPtrType = TypeAdd(pc, NULL, pc->CharPtrType, TypePointer, 0,
        pc->StrEmpty, sizeof(void*), PointerAlignBytes);
    pc->VoidPtrType = TypeAdd(pc, NULL, &pc->VoidType, TypePointer, 0,
        pc->StrEmpty, sizeof(void*), PointerAlignBytes);
}

/* deallocate heap-allocated types */
void TypeCleanupNode(Picoc *pc, struct ValueType *Typ)
{
    struct ValueType *SubType;
    struct ValueType *NextSubType;

    /* clean up and free all the sub-nodes */
    for (SubType = Typ->DerivedTypeList; SubType != NULL;
            SubType = NextSubType) {
        NextSubType = SubType->Next;
        TypeCleanupNode(pc, SubType);
        if (SubType->OnHeap) {
            /* if it's a struct or union deallocate all the member values */
            if (SubType->Members != NULL) {
                VariableTableCleanup(pc, SubType->Members);
                HeapFreeMem(pc, SubType->Members);
            }

            /* free this node */
            HeapFreeMem(pc, SubType);
        }
    }
}

void TypeCleanup(Picoc *pc)
{
    TypeCleanupNode(pc, &pc->UberType);
}

/* parse a struct or union declaration */
void TypeParseStruct(struct ParseState *Parser, struct ValueType **Typ,
    int IsStruct)
{
    char *MemberIdentifier;
    char *StructIdentifier;
    enum LexToken Token;
    int AlignBoundary;
    struct Value *MemberValue;
    Picoc *pc = Parser->pc;
    struct Value *LexValue;
    struct ValueType *MemberType;

    Token = LexGetToken(Parser, &LexValue, false);
    if (Token == TokenIdentifier) {
        LexGetToken(Parser, &LexValue, true);
        StructIdentifier = LexValue->Val->Identifier;
        Token = LexGetToken(Parser, NULL, false);
    } else {
        static char TempNameBuf[7] = "^s0000";
        StructIdentifier = PlatformMakeTempName(pc, TempNameBuf);
    }

    *Typ = TypeGetMatching(pc, Parser, &Parser->pc->UberType,
        IsStruct ? TypeStruct : TypeUnion, 0, StructIdentifier, true);
    if (Token == TokenLeftBrace && (*Typ)->Members != NULL)
        ProgramFail(Parser, "data type '%t' is already defined", *Typ);

    Token = LexGetToken(Parser, NULL, false);
    if (Token != TokenLeftBrace) {
        /* use the already defined structure */
#if 0
        if ((*Typ)->Members == NULL)
            ProgramFail(Parser, "structure '%s' isn't defined",
                LexValue->Val->Identifier);
#endif
        return;
    }

    if (pc->TopStackFrame != NULL)
        ProgramFail(Parser, "struct/union definitions can only be globals");

    LexGetToken(Parser, NULL, true);
    (*Typ)->Members = VariableAlloc(pc, Parser,
        sizeof(struct Table)+STRUCT_TABLE_SIZE*sizeof(struct TableEntry), true);
    (*Typ)->Members->HashTable =
        (struct TableEntry**)((char*)(*Typ)->Members + sizeof(struct Table));
    TableInitTable((*Typ)->Members,
        (struct TableEntry**)((char*)(*Typ)->Members + sizeof(struct Table)),
        STRUCT_TABLE_SIZE, true);

    do {
        TypeParse(Parser, &MemberType, &MemberIdentifier, NULL);
        if (MemberType == NULL || MemberIdentifier == NULL)
            ProgramFail(Parser, "invalid type in struct");

        MemberValue = VariableAllocValueAndData(pc, Parser, sizeof(int), false,
            NULL, true);
        MemberValue->Typ = MemberType;
        if (IsStruct) {
            /* allocate this member's location in the struct */
            AlignBoundary = MemberValue->Typ->AlignBytes;
            if (((*Typ)->Sizeof & (AlignBoundary-1)) != 0)
                (*Typ)->Sizeof +=
                    AlignBoundary - ((*Typ)->Sizeof & (AlignBoundary-1));

            MemberValue->Val->Integer = (*Typ)->Sizeof;
            (*Typ)->Sizeof += TypeSizeValue(MemberValue, true);
        } else {
            /* union members always start at 0, make sure it's big enough
                to hold the largest member */
            MemberValue->Val->Integer = 0;
            if (MemberValue->Typ->Sizeof > (*Typ)->Sizeof)
                (*Typ)->Sizeof = TypeSizeValue(MemberValue, true);
        }

        /* make sure to align to the size of the largest member's alignment */
        if ((*Typ)->AlignBytes < MemberValue->Typ->AlignBytes)
            (*Typ)->AlignBytes = MemberValue->Typ->AlignBytes;

        /* define it */
        if (!TableSet(pc, (*Typ)->Members, MemberIdentifier, MemberValue,
                Parser->FileName, Parser->Line, Parser->CharacterPos))
            ProgramFail(Parser, "member '%s' already defined", &MemberIdentifier);

        if (LexGetToken(Parser, NULL, true) != TokenSemicolon)
            ProgramFail(Parser, "semicolon expected");

    } while (LexGetToken(Parser, NULL, false) != TokenRightBrace);

    /* now align the structure to the size of its largest member's alignment */
    AlignBoundary = (*Typ)->AlignBytes;
    if (((*Typ)->Sizeof & (AlignBoundary-1)) != 0)
        (*Typ)->Sizeof += AlignBoundary - ((*Typ)->Sizeof & (AlignBoundary-1));

    LexGetToken(Parser, NULL, true);
}

/* create a system struct which has no user-visible members */
struct ValueType *TypeCreateOpaqueStruct(Picoc *pc, struct ParseState *Parser,
    const char *StructName, int Size)
{
    struct ValueType *Typ = TypeGetMatching(pc, Parser, &pc->UberType,
        TypeStruct, 0, StructName, false);

    /* create the (empty) table */
    Typ->Members = VariableAlloc(pc,
        Parser,
        sizeof(struct Table)+STRUCT_TABLE_SIZE*sizeof(struct TableEntry), true);
    Typ->Members->HashTable = (struct TableEntry**)((char*)Typ->Members +
        sizeof(struct Table));
    TableInitTable(Typ->Members,
        (struct TableEntry**)((char*)Typ->Members+sizeof(struct Table)),
        STRUCT_TABLE_SIZE, true);
    Typ->Sizeof = Size;

    return Typ;
}

/* parse an enum declaration */
void TypeParseEnum(struct ParseState *Parser, struct ValueType **Typ)
{
    int EnumValue = 0;
    char *EnumIdentifier;
    enum LexToken Token;
    struct Value *LexValue;
    struct Value InitValue;
    Picoc *pc = Parser->pc;

    Token = LexGetToken(Parser, &LexValue, false);
    if (Token == TokenIdentifier) {
        LexGetToken(Parser, &LexValue, true);
        EnumIdentifier = LexValue->Val->Identifier;
        Token = LexGetToken(Parser, NULL, false);
    } else {
        static char TempNameBuf[7] = "^e0000";
        EnumIdentifier = PlatformMakeTempName(pc, TempNameBuf);
    }

    TypeGetMatching(pc, Parser, &pc->UberType, TypeEnum, 0, EnumIdentifier,
        Token != TokenLeftBrace);
    *Typ = &pc->IntType;
    if (Token != TokenLeftBrace) {
        /* use the already defined enum */
        if ((*Typ)->Members == NULL)
            ProgramFail(Parser, "enum '%s' isn't defined", EnumIdentifier);

        return;
    }

    if (pc->TopStackFrame != NULL)
        ProgramFail(Parser, "enum definitions can only be globals");

    LexGetToken(Parser, NULL, true);
    (*Typ)->Members = &pc->GlobalTable;
    memset((void*)&InitValue, '\0', sizeof(struct Value));
    InitValue.Typ = &pc->IntType;
    InitValue.Val = (union AnyValue*)&EnumValue;
    do {
        if (LexGetToken(Parser, &LexValue, true) != TokenIdentifier)
            ProgramFail(Parser, "identifier expected");

        EnumIdentifier = LexValue->Val->Identifier;
        if (LexGetToken(Parser, NULL, false) == TokenAssign) {
            LexGetToken(Parser, NULL, true);
            EnumValue = ExpressionParseInt(Parser);
        }

        VariableDefine(pc, Parser, EnumIdentifier, &InitValue, NULL, false);

        Token = LexGetToken(Parser, NULL, true);
        if (Token != TokenComma && Token != TokenRightBrace)
            ProgramFail(Parser, "comma expected");

        EnumValue++;
    } while (Token == TokenComma);
}

/* parse a type - just the basic type */
int TypeParseFront(struct ParseState *Parser, struct ValueType **Typ,
    int *IsStatic)
{
    int Unsigned = false;
    int StaticQualifier = false;
    enum LexToken Token;
    struct ParseState Before;
    struct Value *LexerValue;
    struct Value *VarValue;
    Picoc *pc = Parser->pc;
    *Typ = NULL;

    /* ignore leading type qualifiers */
    ParserCopy(&Before, Parser);
    Token = LexGetToken(Parser, &LexerValue, true);
    while (Token == TokenStaticType || Token == TokenAutoType ||
            Token == TokenRegisterType || Token == TokenExternType) {
        if (Token == TokenStaticType)
            StaticQualifier = true;

        Token = LexGetToken(Parser, &LexerValue, true);
    }

    if (IsStatic != NULL)
        *IsStatic = StaticQualifier;

    /* handle signed/unsigned with no trailing type */
    if (Token == TokenSignedType || Token == TokenUnsignedType) {
        enum LexToken FollowToken = LexGetToken(Parser, &LexerValue, false);
        Unsigned = (Token == TokenUnsignedType);

        if (FollowToken != TokenIntType && FollowToken != TokenLongType &&
                FollowToken != TokenShortType && FollowToken != TokenCharType) {
            if (Token == TokenUnsignedType)
                *Typ = &pc->UnsignedIntType;
            else
                *Typ = &pc->IntType;

            return true;
        }

        Token = LexGetToken(Parser, &LexerValue, true);
    }

    switch (Token) {
    case TokenIntType:
        *Typ = Unsigned ? &pc->UnsignedIntType : &pc->IntType;
        break;
    case TokenShortType:
        *Typ = Unsigned ? &pc->UnsignedShortType : &pc->ShortType;
        break;
    case TokenCharType:
        *Typ = Unsigned ? &pc->UnsignedCharType : &pc->CharType;
        break;
    case TokenLongType:
        *Typ = Unsigned ? &pc->UnsignedLongType : &pc->LongType;
        break;
    case TokenFloatType:
    case TokenDoubleType:
        *Typ = &pc->FPType;
        break;
    case TokenVoidType:
        *Typ = &pc->VoidType;
        break;
    case TokenStructType: case TokenUnionType:
        if (*Typ != NULL)
            ProgramFail(Parser, "bad type declaration");
        TypeParseStruct(Parser, Typ, Token == TokenStructType);
        break;
    case TokenEnumType:
        if (*Typ != NULL)
            ProgramFail(Parser, "bad type declaration");

        TypeParseEnum(Parser, Typ);
        break;
    case TokenIdentifier:
        /* we already know it's a typedef-defined type because we got here */
        VariableGet(pc, Parser, LexerValue->Val->Identifier, &VarValue);
        *Typ = VarValue->Val->Typ;
        break;

    default:
        ParserCopy(Parser, &Before);
        return false;
    }

    return true;
}

/* parse a type - the part at the end after the identifier. eg.
    array specifications etc. */
struct ValueType *TypeParseBack(struct ParseState *Parser,
    struct ValueType *FromType)
{
    enum LexToken Token;
    struct ParseState Before;

    ParserCopy(&Before, Parser);
    Token = LexGetToken(Parser, NULL, true);
    if (Token == TokenLeftSquareBracket) {
        /* add another array bound */
        if (LexGetToken(Parser, NULL, false) == TokenRightSquareBracket) {
            /* an unsized array */
            LexGetToken(Parser, NULL, true);
            return TypeGetMatching(Parser->pc, Parser,
                TypeParseBack(Parser, FromType), TypeArray, 0,
                    Parser->pc->StrEmpty, true);
        } else {
            /* get a numeric array size */
            enum RunMode OldMode = Parser->Mode;
            int ArraySize;
            Parser->Mode = RunModeRun;
            ArraySize = ExpressionParseInt(Parser);
            Parser->Mode = OldMode;

            if (LexGetToken(Parser, NULL, true) != TokenRightSquareBracket)
                ProgramFail(Parser, "']' expected");

            return TypeGetMatching(Parser->pc, Parser,
                TypeParseBack(Parser, FromType), TypeArray, ArraySize,
                    Parser->pc->StrEmpty, true);
        }
    } else {
        /* the type specification has finished */
        ParserCopy(Parser, &Before);
        return FromType;
    }
}

/* parse a type - the part which is repeated with each
    identifier in a declaration list */
void TypeParseIdentPart(struct ParseState *Parser, struct ValueType *BasicTyp,
    struct ValueType **Typ, char **Identifier)
{
    int Done = false;
    enum LexToken Token;
    struct Value *LexValue;
    struct ParseState Before;
    *Typ = BasicTyp;
    *Identifier = Parser->pc->StrEmpty;

    while (!Done) {
        ParserCopy(&Before, Parser);
        Token = LexGetToken(Parser, &LexValue, true);
        switch (Token) {
        case TokenOpenBracket:
            if (*Typ != NULL)
                ProgramFail(Parser, "bad type declaration");

            TypeParse(Parser, Typ, Identifier, NULL);
            if (LexGetToken(Parser, NULL, true) != TokenCloseBracket)
                ProgramFail(Parser, "')' expected");
            break;

        case TokenAsterisk:
            if (*Typ == NULL)
                ProgramFail(Parser, "bad type declaration");

            *Typ = TypeGetMatching(Parser->pc, Parser, *Typ, TypePointer, 0,
                Parser->pc->StrEmpty, true);
            break;

        case TokenIdentifier:
            if (*Typ == NULL || *Identifier != Parser->pc->StrEmpty)
                ProgramFail(Parser, "bad type declaration");

            *Identifier = LexValue->Val->Identifier;
            Done = true;
            break;

        default: ParserCopy(Parser, &Before); Done = true; break;
        }
    }

    if (*Typ == NULL)
        ProgramFail(Parser, "bad type declaration");

    if (*Identifier != Parser->pc->StrEmpty) {
        /* parse stuff after the identifier */
        *Typ = TypeParseBack(Parser, *Typ);
    }
}

/* parse a type - a complete declaration including identifier */
void TypeParse(struct ParseState *Parser, struct ValueType **Typ,
    char **Identifier, int *IsStatic)
{
    struct ValueType *BasicType;

    TypeParseFront(Parser, &BasicType, IsStatic);
    TypeParseIdentPart(Parser, BasicType, Typ, Identifier);
}

/* check if a type has been fully defined - otherwise it's
    just a forward declaration */
int TypeIsForwardDeclared(struct ParseState *Parser, struct ValueType *Typ)
{
    if (Typ->Base == TypeArray)
        return TypeIsForwardDeclared(Parser, Typ->FromType);

    if ((Typ->Base == TypeStruct || Typ->Base == TypeUnion) &&
            Typ->Members == NULL)
        return true;

    return false;
}
