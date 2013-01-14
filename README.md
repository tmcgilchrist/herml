Herml [![Build Status](https://travis-ci.org/tmcgilchrist/herml.png?branch=master)](https://travis-ci.org/tmcgilchrist/herml)
=======

Welcome to herml, the Haml-like templating language!


Compilation
-----------
To compile Herml, run

    make

in this directory.

Tests
-----

From a Unix shell, run:

    make clean tests

Template compilation
--------------------

Using herml:

* Start up a herml_manager process for your template directory:

    herml_manager:start_link(my_web_app,"/path/to/templates").

Note: herml_manager can cache the compiled template and use it over and over.

* Execute the template by calling the herml_manager process:

    Result = herml_manager:execute_template("file.herml", Env).

Note: Env is a proplist containing the execution environment for the
template. herml expects all variable names to be Erlang strings. For
example, here's a valid environment proplists: [{"UserName", "herml"}].

The UserName variable would be referenced from herml as @UserName.

Another note: For efficiency reasons, herml_manager:execute_template/2,3,4
returns iolists when it executes templates. If you want to view the
template output as a standard string, you can use the io module
to flatten the iolist:

    io:format("~s", [Result]).
