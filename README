# Description

PicoC is a very small C interpreter for scripting. It was originally written
as a script language for a UAV's on-board flight system. It's also very
suitable for other robotic, embedded and non-embedded applications.

The core C source code is around 3500 lines of code. It's not intended to be
a complete implementation of ISO C but it has all the essentials. When
compiled it only takes a few k of code space and is also very sparing of
data space. This means it can work well in small embedded devices. It's also
a fun example of how to create a very small language implementation while
still keeping the code readable.

It's been tested on x86-32, x86-64, powerpc, arm, ultrasparc, HP-PA and blackfin
processors and is easy to port to new targets.

Originally forked from https://github.com/zsaleeba/picoc


# Running files from the command line

You can run standard C programs straight from the command line:

```
$ picoc myprogram.c
```

If your program is split into multiple files you can list them all on the command line.

```
$ picoc myprog1.c myprog2.c myprog3.c
```

If your program takes arguments you add them after a '-' character.

```
$ picoc myprogram.c - arg1 arg2
```


# Running script files

Scripts are slightly simpler than standard C programs - all the system headers
are included automatically for you so you don't need to include them yourself.
Also, scripts don't have a main() function - they just have statements which
are run directly.

```
$ picoc -s myprogram.c
```

Here's an example script:

```
printf("Starting my script\n");

int total = 0;
int i;
for (i = 0; i < 10; i++)
{
    printf("i = %d\n", i);
    total += i;
}

printf("The total is %d\n", total);
```

Here's the output from this script:

```
$ ./picoc -s myscript.c
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

```
> picoc -i
```

Here's an example session:

```
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

```
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
size is 128KB which should be large enough for most programs.

To change the stack size you can set the STACKSIZE environment variable to a
different value. The value is in bytes.


# Compiling picoc

picoc can be compiled for a UNIX/Linux/POSIX host by typing "make".

The test suite can be run by typing "make test".


# Porting picoc

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

picoc is published under the "New BSD License", see the LICENSE file.

