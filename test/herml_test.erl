-module(herml_test).

-author("timmcgil@gmail.com").

-include_lib("eunit/include/eunit.hrl").

compile_file_test() ->
    {ok, compile_test} = herml:compile(<<"%html">>, compile_test).
