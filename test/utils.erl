-module(utils).
-author("timmcgil@gmail.com").

-include_lib("eunit/include/eunit.hrl").

-export([check/1, check/2]).

check(FileName) ->
    check(FileName, []).

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
    io:format("Template file: ~s~n",[File++".herml"]),
    C = herml_parser:file(File ++ ".herml"),
    lists:flatten(herml_htmlizer:render(C, Env, 0)).
