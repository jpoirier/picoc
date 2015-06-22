/* picoc hash table module. This hash table code is used for both symbol tables
 * and the shared string table. */

#include "interpreter.h"


static unsigned int TableHash(const char *Key, int Len);
static struct TableEntry *TableSearch(struct Table *Tbl, const char *Key,
    int *AddAt);
static struct TableEntry *TableSearchIdentifier(struct Table *Tbl,
    const char *Key, int Len, int *AddAt);

/* initialize the shared string system */
void TableInit(Picoc *pc)
{
    TableInitTable(&pc->StringTable, &pc->StringHashTable[0],
            STRING_TABLE_SIZE, true);
    pc->StrEmpty = TableStrRegister(pc, "");
}

/* hash function for strings */
unsigned int TableHash(const char *Key, int Len)
{
    unsigned int Hash = Len;
    int Offset;
    int Count;

    for (Count = 0, Offset = 8; Count < Len; Count++, Offset+=7) {
        if (Offset > sizeof(unsigned int) * 8 - 7)
            Offset -= sizeof(unsigned int) * 8 - 6;

        Hash ^= *Key++ << Offset;
    }

    return Hash;
}

/* initialize a table */
void TableInitTable(struct Table *Tbl, struct TableEntry **HashTable, int Size,
    int OnHeap)
{
    Tbl->Size = Size;
    Tbl->OnHeap = OnHeap;
    Tbl->HashTable = HashTable;
    memset((void*)HashTable, '\0', sizeof(struct TableEntry*) * Size);
}

/* check a hash table entry for a key */
struct TableEntry *TableSearch(struct Table *Tbl, const char *Key,
    int *AddAt)
{
    /* shared strings have unique addresses so we don't need to hash them */
    int HashValue = ((unsigned long)Key) % Tbl->Size;
    struct TableEntry *Entry;

    for (Entry = Tbl->HashTable[HashValue]; Entry != NULL; Entry = Entry->Next) {
        if (Entry->p.v.Key == Key)
            return Entry;   /* found */
    }

    *AddAt = HashValue;    /* didn't find it in the chain */
    return NULL;
}

/* set an identifier to a value. returns FALSE if it already exists.
 * Key must be a shared string from TableStrRegister() */
int TableSet(Picoc *pc, struct Table *Tbl, char *Key, struct Value *Val,
    const char *DeclFileName, int DeclLine, int DeclColumn)
{
    int AddAt;
    struct TableEntry *FoundEntry = TableSearch(Tbl, Key, &AddAt);

    if (FoundEntry == NULL) {   /* add it to the table */
        struct TableEntry *NewEntry = VariableAlloc(pc, NULL,
            sizeof(struct TableEntry), Tbl->OnHeap);
        NewEntry->DeclFileName = DeclFileName;
        NewEntry->DeclLine = DeclLine;
        NewEntry->DeclColumn = DeclColumn;
        NewEntry->p.v.Key = Key;
        NewEntry->p.v.Val = Val;
        NewEntry->Next = Tbl->HashTable[AddAt];
        Tbl->HashTable[AddAt] = NewEntry;
        return true;
    }

    return false;
}

/* find a value in a table. returns FALSE if not found.
 * Key must be a shared string from TableStrRegister() */
int TableGet(struct Table *Tbl, const char *Key, struct Value **Val,
    const char **DeclFileName, int *DeclLine, int *DeclColumn)
{
    int AddAt;
    struct TableEntry *FoundEntry = TableSearch(Tbl, Key, &AddAt);
    if (FoundEntry == NULL)
        return false;

    *Val = FoundEntry->p.v.Val;

    if (DeclFileName != NULL) {
        *DeclFileName = FoundEntry->DeclFileName;
        *DeclLine = FoundEntry->DeclLine;
        *DeclColumn = FoundEntry->DeclColumn;
    }

    return true;
}

/* remove an entry from the table */
struct Value *TableDelete(Picoc *pc, struct Table *Tbl, const char *Key)
{
    /* shared strings have unique addresses so we don't need to hash them */
    int HashValue = ((unsigned long)Key) % Tbl->Size;
    struct TableEntry **EntryPtr;

    for (EntryPtr = &Tbl->HashTable[HashValue];
            *EntryPtr != NULL; EntryPtr = &(*EntryPtr)->Next) {
        if ((*EntryPtr)->p.v.Key == Key) {
            struct TableEntry *DeleteEntry = *EntryPtr;
            struct Value *Val = DeleteEntry->p.v.Val;
            *EntryPtr = DeleteEntry->Next;
            HeapFreeMem(pc, DeleteEntry);

            return Val;
        }
    }

    return NULL;
}

/* check a hash table entry for an identifier */
struct TableEntry *TableSearchIdentifier(struct Table *Tbl,
    const char *Key, int Len, int *AddAt)
{
    int HashValue = TableHash(Key, Len) % Tbl->Size;
    struct TableEntry *Entry;

    for (Entry = Tbl->HashTable[HashValue]; Entry != NULL; Entry = Entry->Next) {
        if (strncmp(&Entry->p.Key[0], (char*)Key, Len) == 0 &&
                Entry->p.Key[Len] == '\0')
            return Entry;   /* found */
    }

    *AddAt = HashValue;    /* didn't find it in the chain */
    return NULL;
}

/* set an identifier and return the identifier. share if possible */
char *TableSetIdentifier(Picoc *pc, struct Table *Tbl, const char *Ident,
    int IdentLen)
{
    int AddAt;
    struct TableEntry *FoundEntry = TableSearchIdentifier(Tbl, Ident, IdentLen,
        &AddAt);

    if (FoundEntry != NULL)
        return &FoundEntry->p.Key[0];
    else {
        /* add it to the table - we economise by not allocating
            the whole structure here */
        struct TableEntry *NewEntry = HeapAllocMem(pc,
            sizeof(struct TableEntry) -
            sizeof(union TableEntryPayload) + IdentLen + 1);
        if (NewEntry == NULL)
            ProgramFailNoParser(pc, "(TableSetIdentifier) out of memory");

        strncpy((char *)&NewEntry->p.Key[0], (char *)Ident, IdentLen);
        NewEntry->p.Key[IdentLen] = '\0';
        NewEntry->Next = Tbl->HashTable[AddAt];
        Tbl->HashTable[AddAt] = NewEntry;
        return &NewEntry->p.Key[0];
    }
}

/* register a string in the shared string store */
char *TableStrRegister2(Picoc *pc, const char *Str, int Len)
{
    return TableSetIdentifier(pc, &pc->StringTable, Str, Len);
}

char *TableStrRegister(Picoc *pc, const char *Str)
{
    return TableStrRegister2(pc, Str, strlen((char *)Str));
}

/* free all the strings */
void TableStrFree(Picoc *pc)
{
    int Count;
    struct TableEntry *Entry;
    struct TableEntry *NextEntry;

    for (Count = 0; Count < pc->StringTable.Size; Count++) {
        for (Entry = pc->StringTable.HashTable[Count];
                Entry != NULL; Entry = NextEntry) {
            NextEntry = Entry->Next;
            HeapFreeMem(pc, Entry);
        }
    }
}
