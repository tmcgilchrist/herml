-module(herml_test).

-author("timmcgil@gmail.com").

-include_lib("eunit/include/eunit.hrl").

compile_binary_test() ->
    {ok, compile_test} = herml:compile(<<"%html">>, compile_test).

compile_file_test() ->
    {ok, compile_test} = herml:compile("./examples/basic_html.herml", compile_test).
