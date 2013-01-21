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
     check("hello_world"),
     check("message", [{"Message", "This is a test"}]),
     check("message2", [{"Message", "This is a test"}]),
     check("default_attr"),
     check("call_single"),
     check("call_single_env"),
     check("call_multi"),
     check("call_multi_params", [{"Foo", "This is foo"}]),
     check("call_multi_env"),
     check("horizontal_rule"),
     check("close_empty"),
     check("doctypes"),
     check("multiple_classes"),
     check("sort_attributes"),
     check("style_attribute"),
     check("dashed_attrs"),
     check("string_and_number_attrs"),
     check("funcall_attrs", [{"Key", "class"}, {"Value", "awesome"}]),
     check("variable_attrs", [{"Key", "class"}, {"Value", "awesome"}]),
     check("simple_loop", [{"Users", ["kevsmith", "seancribbs"]}]),
     check("loop_with_ignores", [{"Users", [{1, "kevsmith"}, {2, "seancribbs"}]}]),
     check("structured_loop", [{"Users", [{1, "kevsmith"}, {2, "seancribbs"}]}]),
     check("tuple_access", [{"Users", [{1, "kevsmith"}, {2, "seancribbs"}]}]),
     check("atom_value", [{"User", undefined}])
    ].

sub_template_test() ->
    {ok, _Pid} = herml_manager:start_link(foo, test_files_path()),
    {ok, Rendered} = herml_manager:execute_template(foo, "main.herml"),
    {ok, PreRendered} = file:read_file(test_files_path() ++ "main.render"),
    herml_manager:shutdown(foo),
    ?assertEqual(binary_to_list(PreRendered), lists:flatten(Rendered)).

iteration_match_test() ->
    [
     iteration_bad_match(test_files_path() ++ "structured_loop", [{"Users", [{1, "kevsmith"}, {2, "seancribbs", "foobar"}]}])
    ].

iteration_bad_match(File, Env) ->
    C = herml_parser:file(File ++ ".herml"),
    ?_assertThrow(bad_match, herml_htmlizer:render(C, Env, 0)).

check(FileName) ->
    check(test_files_path() ++ FileName, []).

check(FileName, Env) ->
    CR = read_file(FileName),
    PR = render_file(FileName, Env),
    ?_assertEqual(CR, PR).

read_file(File) ->
    case file:read_file(File ++ ".render") of
        {ok, C} ->
            binary_to_list(C);
        {error, Why} ->
            io:format("Failed to read file ~s.render ~s", [File, Why])
    end.

render_file(File, Env) ->
    C = herml_parser:file(File ++ ".herml"),
    lists:flatten(herml_htmlizer:render(C, Env, 0)).

test_files_path() ->
    "../test/examples/".
