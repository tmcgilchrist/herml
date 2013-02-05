-module(herml_compiler).

%% API
-export([compile/2, compile/3]).

-record(haml_context, {
          module = [],
          compiler_options = [verbose, report_errors]}).

-record(treewalker, {
    counter = 0,
    safe = false
}).

%% TODO we want this to be the interface.
%% it compiles the HAML file into a module.
compile(File, Module) ->
    compile(File, Module, []).
compile(Binary, Module, Options) when is_binary(Binary) ->
    File = "",
    Context = init_haml_context(File, Module, Options),
    case herml_parser:string(Binary) of
        HamlParseTree ->
            case compile_to_binary(File, HamlParseTree, Context) of
                {ok, Module1, _, _} ->
                    {ok, Module1};
                Err ->
                    Err
            end
    end;

compile(File, Module, Options) ->
    % Read file
    % check whether the file has changed see herml_manager:load_and_store()
    % then do the compile and writing of the module.
    Context = init_haml_context(File, Module, Options),
    case herml_parser:file(File) of
        HamlParseTree ->
            case compile_to_binary(File, HamlParseTree, Context) of
                {ok, Module1, _, _} ->
                    {ok, Module1};
                Err ->
                    Err
            end
    end.

%%====================================================================
%% Internal functions
%%====================================================================
compile_to_binary(File, _HamlParseTree, Context) ->
    Module = Context#haml_context.module,
    ModuleAst  = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
                                     [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(0))])]),
    %% TODO debug io:format call
    Render0FunctionAst = erl_syntax:function(erl_syntax:atom(render),
                                             [erl_syntax:clause([], none, [erl_syntax:application(erl_syntax:atom(io),     % Module
                                                                                                  erl_syntax:atom(format), % Method Name
                                                                                                  [erl_syntax:atom("hello~n")]    % Argument list
                                                                                                 )
                                                                          ])
                                             ]),
    Forms = [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, Render0FunctionAst]],

    compile_forms_and_reload(File, Forms, Context#haml_context.compiler_options).

compile_forms_and_reload(File, Forms, CompilerOptions) ->
    case compile:forms(Forms, CompilerOptions) of
        {ok, Module1, Bin} ->
            load_code(Module1, Bin, []);
        {ok, Module1, Bin, Warnings} ->
            load_code(Module1, Bin, Warnings);
        error ->
            {error, lists:concat(["compilation failed: ", File])};
        {error, Errors, Warnings} ->
            io:format("Errors ~s~n Warnings: ~s~n", [Errors, Warnings]),
            {error, lists:concat(["compilation failed with errors ", File])}
    end.

load_code(Module, Bin, Warnings) ->
    code:purge(Module),
    case code:load_binary(Module, atom_to_list(Module) ++ ".erl", Bin) of
        {module, _} ->
            {ok, Module, Bin, Warnings};
        _ ->
            {error, lists:concat(["code reload failed: ", Module])}
    end.

init_haml_context(_File, Module, _Options) ->
    #haml_context{
       module = Module,
       compiler_options = []
      }.
