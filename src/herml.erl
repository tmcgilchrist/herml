-module(herml).

%% API
-export([compile/2, compile/3]).

%% @spec compile( FileOrBinary, Module::atom() ) -> {ok, Module} | {error, Reason}
compile(FileOrBinary, Module) ->
    herml_compiler:compile(FileOrBinary, Module).

%% @spec compile( FileOrBinary, Module::atom(), Options ) -> {ok, Module} | {error, Reason}
compile(FileOrBinary, Module, Options) ->
    herml_compiler:compile(FileOrBinary, Module, Options).
