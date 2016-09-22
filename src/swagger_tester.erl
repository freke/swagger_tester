-module(swagger_tester).

%% API exports
-export([
    get/2,
    put/2,
    put/3,
    post/2,
    post/3,
    patch/3,
    delete/2,
    swagger_json/1,
    schema/4
  ]).

%%====================================================================
%% API functions
%%====================================================================
get(URL, Expectations) ->
  Method = get,
  Payload = <<>>,
  Resp = request(Method, URL, Payload),
	expectations(Expectations, Resp).

put(URL, Expectations) ->
  put(URL, <<>>, Expectations).

put(URL, Payload, Expectations) ->
  Method = put,
  Resp = request(Method, URL, Payload),
  expectations(Expectations, Resp).

post(URL, Expectations) ->
  post(URL, <<>>, Expectations).

post(URL, Payload, Expectations) ->
  Method = post,
  Resp = request(Method, URL, Payload),
  expectations(Expectations, Resp).

patch(URL, Payload, Expectations) ->
  Method = patch,
  Resp = request(Method, URL, Payload),
  expectations(Expectations, Resp).

delete(URL, Expectations) ->
  Method = delete,
  Payload = <<>>,
  Resp = request(Method, URL, Payload),
  expectations(Expectations, Resp).

schema(Method, Path, StatusCode, {url, URL}) ->
  schema(Method, Path, StatusCode, {json, swagger_tester:swagger_json(URL)});
schema(Method, Path, StatusCode, {json, Json}) ->
  Swagger = jiffy:decode(Json,[return_maps]),
  XPath =
    [
      <<"paths">>,
      Path,
      atom_to_binary(Method, utf8),
      <<"responses">>,
      list_to_binary(integer_to_list(StatusCode)),
      <<"schema">>
    ],
	Schema = lists:foldl(fun(K, A) -> maps:get(K, A) end, Swagger, XPath),
	expand_schema(Schema, Swagger).

%%====================================================================
%% Internal functions
%%====================================================================
request(Method, URL, Payload) ->
  Headers = [],
  Options = [],
  {ok, StatusCode, RespHeaders, ClientRef} = hackney:Method(URL, Headers, Payload, Options),
  {ok, Body} = hackney:body(ClientRef),
  {StatusCode, RespHeaders, Body}.

swagger_json(URL) ->
	{ok, 200, _, ClientRef} = hackney:get(URL, [], [], []),
	{ok, Body} = hackney:body(ClientRef),
	Body.

expectations([],Resp) ->
	Resp;
expectations([{status, StatusCode}|R],{StatusCode, _RespHeaders, _Body}=Resp) ->
	expectations(R,Resp);
expectations([{content_type, ContentType}|R], {_StatusCode, RespHeaders, _Body}=Resp) ->
	ContentType = proplists:get_value(<<"content-type">>, RespHeaders),
	expectations(R,Resp);
expectations([{json, ExpectedJson}|R], {_StatusCode, _RespHeaders, Body}=Resp) ->
	true = case jiffy:decode(Body,[return_maps]) of
		Json when is_map(Json) -> maps:fold(fun(K, V, A) -> A andalso maps:get(K,Json) =:= V end, true, ExpectedJson);
		[] -> [] =:= ExpectedJson
	end,
	expectations(R,Resp);
expectations([{json_schema, Schema}|R], {_StatusCode, _RespHeaders, Body}=Resp) ->
	Json = jiffy:decode(Body,[return_maps]),
	{ok, Json} = jesse:validate_with_schema(Schema, Json),
	expectations(R,Resp).

expand_schema(#{<<"$ref">> := Def}, Swagger) ->
	<<"#/definitions/",D/binary>> = Def,
	maps:get(D,maps:get(<<"definitions">>, Swagger));
expand_schema(E,Swagger) when is_map(E) ->
	maps:map(fun(_K,V) -> expand_schema(V,Swagger) end, E);
expand_schema(E,Swagger) when is_list(E) ->
	lists:map(fun(V) -> expand_schema(V,Swagger) end, E);
expand_schema(E,_Swagger) when is_binary(E)->
	E.
