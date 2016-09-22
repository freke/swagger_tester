-module(swagger_tester_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export(
	[
		echo_handler_tests/1
	]).

all() ->
	[
		echo_handler_tests
	].

init_per_suite(Config) ->
	application:ensure_all_started(hackney),
	Config.

end_per_suite(_Config) ->
	application:stop(hackney),
	ok.

init_per_testcase(_, Config) ->
	{ok, _} = example:start(),
	Config.

end_per_testcase(_, _Config) ->
	_ = example:stop(),
  ok = cleanup(),
	ok.

%% @private
-spec cleanup() -> ok.
cleanup() ->
  _ = application:stop(cowboy_swagger),
  _ = application:stop(trails),
  ok.

%%%===================================================================
%%% Tests functions
%%%===================================================================
echo_handler_tests(_Config) ->
	{ok, Port} = application:get_env(example, http_port),
  URL = [<<"http://localhost:">>,integer_to_binary(Port)],
	ct:comment("test GET /message should return 200 OK"),
	Swagger = {url, [URL,<<"/api-docs/swagger.json">>]},
  swagger_tester:get([URL,<<"/message">>],
		[
	    {status, 200},
	    {content_type, <<"application/json">>},
			{json_schema, swagger_tester:schema(get, <<"/message">>, 200, Swagger)},
	    {json, #{<<"echo">> => <<"">>}}
	  ]),
	ct:comment("test PUT /message/1234 should return 200 OK => {\"echo\":\"1234}\""),
	swagger_tester:put([URL,<<"/message/1234">>],
		<<"">>,
		[
	    {status, 200},
	    {content_type, <<"application/json">>},
			{json_schema, swagger_tester:schema(put, <<"/message/{echo}">>, 200, Swagger)},
	    {json, #{<<"echo">> => <<"1234">>}}
	  ]),
	ct:comment("test GET /message should return 200 OK => {\"echo\":\"1234}\""),
  swagger_tester:get([URL,<<"/message">>],
		[
	    {status, 200},
	    {content_type, <<"application/json">>},
			{json_schema, swagger_tester:schema(get, <<"/message">>, 200, Swagger)},
	    {json, #{<<"echo">> => <<"1234">>}}
	  ]),
	{comment, ""}.
