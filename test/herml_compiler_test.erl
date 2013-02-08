-module(herml_compiler_test).

-author("timmcgil@gmail.com").

-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    Output = herml_compiler:compile(<<"%html">>, parse_test),
    {ok, Module} = Output,
    Module:render().

parse_file_test() ->
    {ok, _Module} = herml_compiler:compile("./examples/basic_html.herml", parse_test).

basic_herml_test() ->
    % Pass in basic_html.herml
    {ok, _} = herml_compiler:compile("./example/basic_html.herml", parse_test),
    % check it compiles
    parse_test:render(),
    % inspect the module has a render/2 method
    % call render/2 and check the output is correct.
    err.

basic_module_written() ->
    % Given a simple haml file.
    % The compiler should compile successfully
    % A Module should be written out
    err.

test_file_path() ->
    "../test/examples".
