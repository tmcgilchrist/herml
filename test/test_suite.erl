-module(test_suite).

-author("kevin@hypotheticalabs.com").
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    [{module, herml_scan_test},
     {module, herml_parse_test},
     {module, herml_reader_test},
     {module, herml_htmlizer_test},
     {module, herml_parser_test},
     {module, herml_manager_test},
     {module, herml_test}].
