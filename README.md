Originally forked from https://github.com/zsaleeba/picoc

# Description

PicoC is a very small C interpreter for scripting. It was originally written
as a script language for a UAV on-board flight system. It's also very
suitable for other robotic, embedded and non-embedded applications.

The core C source code is around 3500 lines of code. It's not intended to be
a complete implementation of ISO C but it has all the essentials. When
compiled it only takes a few k of code space and is also very sparing of
data space. This means it can work well in small embedded devices. It's also
a fun example of how to create a very small language implementation while
still keeping the code readable.

It's been tested on x86-32, x86-64, powerpc, arm, ultrasparc, HP-PA and blackfin
processors and is easy to port to new targets.


# Running files from the command line

You can run standard C programs straight from the command line:

```C
$ picoc file.c
```

If your program is split into multiple files you can list them all on the
command line.

```C
$ picoc file1.c file2.c file3.c
```

If your program takes arguments you add them after a '-' character.

```C
$ picoc file.c - arg1 arg2
```


# Running script files

Scripts are slightly simpler than standard C programs because, A) all the system
headers are included automatically for you so you don't need to include them
in your file/s and B) scripts don't require a main() function; they have
statements that are run directly from the top of a file to the bottom.

```C
$ picoc -s file.c
```

Here's an example script:

```C
printf("Starting my script\n");

int i;
int total = 0;
for (i = 0; i < 10; i++) {
    printf("i = %d\n", i);
    total += i;
}

printf("The total is %d\n", total);
```

Here's the output from this script:

```C
$ ./picoc -s script.c
Starting my script
i = 0
i = 1
i = 2
i = 3
i = 4
i = 5
i = 6
i = 7
i = 8
i = 9
The total is 45
```


# Interactive mode

```C
> picoc -i
```

Here's an example session:

```C
$ ./picoc -i
starting picoc v2.1
picoc> char inbuf[80];
picoc> gets(inbuf);
hello!
picoc> printf("I got: %s\n", inbuf);
I got: hello!
```

Deleting variables and functions.

Sometimes in interactive mode you want to change a function or redeclare a
variable. You can do this using the "delete" statement:

```C
$ ./picoc -i
starting picoc v2.1
picoc> int fred = 1234;
picoc> printf("fred = %d\n", fred);
fred = 1234
picoc> delete fred;
picoc> char *fred = "hello";
picoc> printf("fred = '%s'\n", fred);
fred = 'hello'
```

Note, you can quit picoc's interactive mode using control-D.


# Environment variables

In some cases you may want to change the picoc stack space. The default stack
size is 512KB (see PICOC_STACK_SIZE in picoc.c) which should be large enough
for most programs.

To change the stack size you can set the STACKSIZE environment variable to a
different value. The value is in bytes.


# Compiling PicoC

picoc can be compiled for a UNIX/Linux/POSIX host by typing "make".

The test suite can be run by typing "make test".

On Windows, use the MSVC++ sln file in the msvc/picoc folder.


# Porting PicoC

platform.h is where you select your platform type and specify the includes
etc. for your platform.

platform_XXX.c contains support functions so the compiler can work on
your platform, such as how to write characters to the console etc..

platform_library.c contains your library of functions you want to make
available to user programs.

There's also a clibrary.c which contains user library functions like
printf() which are platform-independent.

Porting the system will involve setting up suitable includes and defines
in platform.h, writing some I/O routines in platform_XXX.c, putting
whatever user functions you want in platform_library.c and then changing
the main program in picoc.c to whatever you need to do to get programs
into the system.

platform.h is set to UNIX_HOST by default so tests can be easily run on
a UNIX system. You'll need to specify your own host setup dependent on
your target platform.


# Copyright

PicoC is published under the "New BSD License", see the LICENSE file.



# Adding native C functions

## Introduction
picoc allows you to define your own library functions. These functions are
written in C using your system's native C compiler. Since the native C compiler
can access the hardware this means you can add functions which give picoc control
of your hardware.

## How libraries work
Your picoc distribution contains two files which are used to define library
functions for your system. If your system is called "foobar" you'll be using:

* library_foobar.c - this is where the foobar-specific library functions go
* clibrary.c - this is where standard C library functions like printf() are defined

We'll start by defining a simple function in library_foobar.c. We need to do two things:

* add the function prototype to our list of picoc library functions
* define the native C implementation of the function

## The prototype list
Each of the library_XXX.c files defines a list of picoc prototypes for each of
the functions it defines. For example:

```C
struct LibraryFunction PlatformLibrary[] =
{
     {ShowComplex,  "void ShowComplex(struct complex *)"},
     {Cpeek,        "int peek(int, int)"},
     {Cpoke,        "void poke(int, int, int)"},
     {Crandom,      "int random(int)"},
     {NULL,         NULL}
};
```

The first column is the name of the C function. The second column is the function
prototype. The "{ NULL, NULL }" line at the end is required.

## The native C function
The native C function is called with these parameters:

```C
void MyCFunc(struct ParseState *Parser,
			 struct Value *ReturnValue,
			 struct Value **Param,
			 int NumArgs);
```

* struct ParseState *Parser - this contains internal information about the progress of parsing. It's mostly used here so error messages from your function can report the line number where an error occurred.
* struct Value *ReturnValue - this points to the place you can put your return value. This is pre-created as a value of the correct return type so all you have to do is store your result here.
* struct Value **Param - this points to an array of parameters. These are all pre-checked as being the correct type.
* int NumArgs - this is the number of parameters. Normally this will already have been checked and will be exactly what you've defined in your function prototype. It is however possible to define functions with variable numbers of arguments using a stdarg-like "..." method and this is where you find out how many parameters were passed in if you're doing that.

Here's an example function definition of "random" (as defined above):

```C
void Crandom(struct ParseState *Parser,
			 struct Value *ReturnValue,
			 struct Value **Param,
			 int NumArgs)
{
    ReturnValue->Val->Integer = random() % Param[0]->Val->Integer;
}
```

This function calls "random()" from the C standard library. It accesses an integer
parameter and returns an integer value.

## Passing parameters
We've seen how to pass integers into functions. What about passing other data types?


| Type		| Method				 | Comment
|-----------|------------------------|-------
| int		| Param[x]->Val->Integer |
| char		| Param[x]->Val->Integer | Treated as 'int' here
| double	| Param[x]->Val->FP		 | Only available on some systems
| float		| Param[x]->Val->FP		 | Same as 'double'
| enum		| Param[x]->Val->Integer | Gives integer value of enum
| pointers	| See section below		 | Slightly more complicated
| char *	| See section below		 | Slightly more complicated
| arrays	| See section below		 | Slightly more complicated
| struct	| See section below		 | Slightly more complicated
| union		| See section below		 | Slightly more complicated

## Passing pointers
Pointer parameters are slighty more complicated to access since you have to
dereference the pointer to get at the underlying data.

Here's how we dereference a pointer parameter. In this example I'll be reading
an 'int *' parameter:

```C
int IntValue = *(int*)Param[0]->Val->NativePointer;
```

## Passing strings/char*
In this example I'll be reading a 'char *' parameter. It's pretty similar to
the 'int *' example above:

```C
char *CharPtr = (char*)Param[0]->Val->NativePointer;
```

picoc strings work like C strings - they're pointers to arrays of characters,
terminated by a null character. Once you have the C char * you can use it just
like a normal C string.

Pointers to arrays of other data types work the same way.

## Passing pointers to structures and unions
If you're defining library functions which take structures as parameters you'll
have to do a little more work. You need to pre-define the structure so the
function prototype can refer to it.

In library_XXX.c you'll find a function called PlatformLibraryInit(). This is
called before the library prototypes are defined. Here's a quick way to define
a complex number structure as if it was defined in an include file:

```C
IncludeRegister("win32.h",
				&win32SetupFunc,
				&win32Functions[0],
				"struct complex {int i; int j;};");
```

Or you could just parse the structure directly:

```C
const char *definition = "struct complex {int i; int j;};";
PicocParse("my lib", definition, strlen(definition), true, false, false);
```

The same method works for defining macros too:

```C
const char *definition = "#define ABS(a) ((a) < (0) ? -(a) : (a))";
PicocParse("my lib", definition, strlen(definition), true, false, false);
```

Here's a more sophisticated method, using the internal functions of picoc directly:

```C
void PlatformLibraryInit()
{
    struct ParseState Parser;
    char *Identifier;
    struct ValueType *ParsedType;
    void *Tokens;
    char *IntrinsicName = TableStrRegister("complex library");
    const char *StructDefinition = "struct complex { int i; int j; }";

    /* define an example structure */
    Tokens = LexAnalyse(IntrinsicName, StructDefinition, strlen(StructDefinition), NULL);
    LexInitParser(&Parser, StructDefinition, Tokens, IntrinsicName, true, false);
    TypeParse(&Parser, &ParsedType, &Identifier, &IsStatic);
    HeapFree(Tokens);
}
```

This code takes the structure definition in StructDefinition and runs the lexical
analyser over it. This returns some lexical tokens. Then we initialize the parser
and have it parse the type of the structure definition from the tokens we made.
That's enough to define the structure in the system. Finally we free the tokens.

Now let's say we're going to define a function to display a complex number.
Our prototype will look like:

```C
{ShowComplex,   "void ShowComplex(struct complex *)"},
```

And finally we can define the library function:

```C
struct complex {int i; int j;};  /* make this C declaration match the picoc one */

void ShowComplex(struct ParseState *Parser,
				 struct Value *ReturnValue, struct Value **Param, int NumArgs)
{
    struct complex *ComplexVal = Param[0]->Val->NativePointer;  /* casts the pointer */

    /* print the result */
    PrintInt(ComplexVal->i, PlatformPutc);
    PlatformPutc(',');
    PrintInt(ComplexVal->j, PlatformPutc);
}
```

Unions work exactly the same way as structures. Define the prototype as "union"
rather than "struct" and you're away.

## Returning values
Returning values from library functions is very much like accessing parameters.
The type of return values is already set before your native C function is called
so all you have to do is fill in the value.

Just as with parameters, ints, chars and enums are stored in ReturnValue->Val->Integer
and floating point values are returned in ReturnValue->Val->FP.

## Returning pointers
Returning a pointer to a static string or some other allocated data is easy.
Your return code will look something like:

```C
ReturnValue->Val->NativePointer = "hello";
```

## Variable numbers of parameters
You can define your own stdarg-style library functions like printf(). Your
function prototype should use "..." in the parameter list to indicate the potential
extra parameters just like the standard stdarg system. Here's an example from clibrary.c:

```C
{LibPrintf, "void printf(char *, ...)"},
```

The NumArgs parameter to the native C function lets you know how many parameters
were passed in. You access the variable parameters just like normal parameters
using the Param[] array.

Take a look at clibrary.c for the full definition of LibPrintf() if you need a
more complete example.

## Sharing native values with PicoC
Sometimes you have native variables you'd like to share with picoc. We can
define a picoc value which shares memory with a native variable. Then we store
this variable in the picoc symbol table so your programs can find it by name.
There's an easy way to do this:

```C
int RobotIsExploding = 0;

void PlatformLibraryInit()
{
    VariableDefinePlatformVar(NULL,
    						  "RobotIsExploding",
    						  &IntType,
    						  (union AnyValue*)&RobotIsExploding,
    						  false);
}
```

The variable RobotIsExploding can be written by your native C program and read
by PicoC just like any other PicoC variable. In this case it's protected from
being written by the last parameter "IsWritable" being set to FALSE. Set it to
TRUE and PicoC will be able to write it too.


# How PicoC differs from C90

PicoC is a tiny C language, not a complete implementation of C90. It doesn't
aim to implement every single feature of C90 but it does aim to be close enough
that most programs will run without modification.

PicoC also has scripting abilities which enhance it beyond what C90 offers.

## C preprocessor
There is no true preprocessor in PicoC. The most popular preprocessor features
are implemented in a slightly limited way.

## `#define`
Macros are implemented but have some limitations. They can only be used
as part of expressions and operate a bit like functions. Since they're used in
expressions they must result in a value.

## `#if/#ifdef/#else/#endif`
The conditional compilation operators are implemented, but have some limitations.
The operator "defined()" is not implemented. These operators can only be used at
statement boundaries.

## `#include`
Includes are supported however the level of support depends on the specific port
of PicoC on your platform. Linux/UNIX and Windows support #include fully.

## Function declarations
These styles of function declarations are supported:

```C
int my_function(char param1, int param2, char *param3)
{
   ...
}

int my_function(char param1, int param2, char *param3) {
   ...
}
```

The old "K&R" form of function declaration is not supported.

## Predefined macros
A few macros are pre-defined:

* PICOC_VERSION - gives the picoc version as a string eg. "v2.1 beta r524"

## Function pointers
Pointers to functions are currently not supported.

## Storage classes
Many of the storage classes in C90 really only have meaning in a compiler so
they're not implemented in picoc. This includes: static, extern, volatile,
register and auto. They're recognised but currently ignored.

## struct and unions
Structs and unions can only be defined globally. It's not possible to define
them within the scope of a function.

Bitfields in structs are not supported.

## Linking with libraries
Because picoc is an interpreter (and not a compiler) libraries must be linked
with picoc itself. Also a glue module must be written to interface to picoc.
This is the same as other interpreters like python.

If you're looking for an example check the interface to the C standard library
time functions in cstdlib/time.c.

## goto
The goto statement is implemented but only supports forward gotos, not backward.
The rationale for this is that backward gotos are not necessary for any
"legitimate" use of goto.

Some discussion on this topic:

* http://www.cprogramming.com/tutorial/goto.html
* http://kerneltrap.org/node/553/2131
