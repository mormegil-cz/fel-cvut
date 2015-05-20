# Karel Compiler

Overview
--------

This is a compiler of Karel, i.e. a Java application that compiles programs written
in [the Karel programming language](https://en.wikipedia.org/wiki/Karel_%28programming_language%29)
to Java class files that can be run on any computer with Java Virtual Machine.

Usage
-----

The program source is compiled with the following command: `java -jar Karel.jar sourcefile destclass`,
where `sourcefile` is a name of the file containing source text of your Karel program and `destclass` is a name you have chosen for the class which will be created from the Karel source. The file which will be created will have the name `destclass.class`.

Running Karel programs
----------------------

The resulting class file is a standard Java class file, i.e. to start the program, use `java destclass` as in case of any other Java program. You only have to enable the Java runtime environment to find `karel.common` and `karel.city` packages.
