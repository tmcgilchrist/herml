-module(herml_htmlizer_test).

-author("kevin@hypotheticalabs.com").

-include_lib("eunit/include/eunit.hrl").

-export([default_attr/1]).
-export([emit_single/1, emit_multi/2]).
-export([emit_single/2, emit_multi/3]).
-export([attr_key/0, attr_key/1, attr_value/0, attr_value/1]).

default_attr(Env) ->
    {[{color, "red"},
      {class, "foo"}], Env}.

emit_single(Arg) ->
    Arg.

emit_multi(Arg1, Arg2) ->
    [Arg1, Arg2].

emit_single(Arg, Env) ->
    {Arg ++ "!", Env}.

emit_multi(Arg1, Arg2, Env) ->
    {[Arg1 ++ "!", Arg2], Env}.

attr_key() -> "class".
attr_key(Key) when is_list(Key) -> "key" ++ Key.

attr_value() -> "awesome".
attr_value(Value) when is_list(Value) -> "value" ++ Value.

render_test() ->
    [
     utils:check(test_file_path() ++ "hello_world"),
     utils:check(test_file_path() ++ "message", [{"Message", "This is a test"}]),
     utils:check(test_file_path() ++ "message2", [{"Message", "This is a test"}]),
     utils:check(test_file_path() ++ "default_attr"),
     utils:check(test_file_path() ++ "call_single"),
     utils:check(test_file_path() ++ "call_single_env"),
     utils:check(test_file_path() ++ "call_multi"),
     utils:check(test_file_path() ++ "call_multi_params", [{"Foo", "This is foo"}]),
     utils:check(test_file_path() ++ "call_multi_env"),
     utils:check(test_file_path() ++ "horizontal_rule"),
     utils:check(test_file_path() ++ "close_empty"),
     utils:check(test_file_path() ++ "doctypes"),
     utils:check(test_file_path() ++ "multiple_classes"),
     utils:check(test_file_path() ++ "sort_attributes"),
     utils:check(test_file_path() ++ "style_attribute"),
     utils:check(test_file_path() ++ "dashed_attrs"),
     utils:check(test_file_path() ++ "string_and_number_attrs"),
     utils:check(test_file_path() ++ "funcall_attrs", [{"Key", "class"}, {"Value", "awesome"}]),
     utils:check(test_file_path() ++ "variable_attrs", [{"Key", "class"}, {"Value", "awesome"}]),
     utils:check(test_file_path() ++ "simple_loop", [{"Users", ["kevsmith", "seancribbs"]}]),
     utils:check(test_file_path() ++ "loop_with_ignores", [{"Users", [{1, "kevsmith"}, {2, "seancribbs"}]}]),
     utils:check(test_file_path() ++ "structured_loop", [{"Users", [{1, "kevsmith"}, {2, "seancribbs"}]}]),
     utils:check(test_file_path() ++ "tuple_access", [{"Users", [{1, "kevsmith"}, {2, "seancribbs"}]}]),
     utils:check(test_file_path() ++ "atom_value", [{"User", undefined}])
    ].

sub_template_test() ->
    {ok, _Pid} = herml_manager:start_link(foo, test_file_path()),
    %% {ok, Rendered} =  herml_manager:execute_template(foo, "main.herml"),
    %% io:format("Rendered ~s~n", [Rendered]),

    %% {ok, PreRendered} = file:read_file("main.render"),
    herml_manager:shutdown(foo).
    %% ?assertEqual(binary_to_list(PreRendered), lists:flatten(Rendered)).

iteration_match_test() ->
    [
     iteration_bad_match(test_file_path() ++ "structured_loop", [{"Users", [{1, "kevsmith"}, {2, "seancribbs", "foobar"}]}])
    ].

iteration_bad_match(File, Env) ->
    C = herml_parser:file(File ++ ".herml"),
    ?_assertThrow(bad_match, herml_htmlizer:render(C, Env, 0)).

test_file_path() ->
    "../test/examples/".
