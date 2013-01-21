-module(herml_compiler_test).

-author("timmcgil@gmail.com").

-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    Output = herml_compiler:compile(<<"%html">>, parse_test),
    {ok, Module} = Output,
    Module:render().

parse_file_test() ->
    Output = herml_compiler:compile("./examples/basic_html.herml", parse_test),
    {ok, Module} = Output.
