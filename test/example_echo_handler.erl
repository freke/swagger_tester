-module(example_echo_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {example_default,
         [
          init/3,
          rest_init/2,
          content_types_accepted/2,
          content_types_provided/2,
          resource_exists/2
         ]}
       ]).

-export([ allowed_methods/2
        , handle_put/2
        , handle_get/2
        ]).

%trails
-behaviour(trails_handler).
-export([trails/0]).

trails() ->
  Echo =
    #{
      <<"echo">> =>
      #{
        description => <<"The echo">>,
        type => <<"string">>
      }
    },
  ok = cowboy_swagger:add_definition(<<"Echo">>, Echo),
  MetadataGet =
    #{get =>
      #{tags => ["echo"],
        description => "Gets echo var from the server",
        produces => ["application/json"],
        responses =>
          #{<<"200">> =>
            #{
              description => "echo",
              schema => cowboy_swagger:schema(<<"Echo">>)
            }
          }
      }
    },
  MetadataPut =
    #{put =>
      #{tags => ["echo"],
        description => "Sets echo var in the server",
        produces => ["application/json"],
        parameters => [
          #{name => <<"echo">>,
            description => <<"Echo message">>,
            in => <<"path">>,
            required => false,
            type => <<"string">>}
        ],
        responses =>
          #{<<"200">> =>
            #{
              description => "echo",
              schema => cowboy_swagger:schema(<<"Echo">>)
            }
          }
      }
    },
  [
    trails:trail("/message", example_echo_handler, [], MetadataGet),
    trails:trail("/message/:echo", example_echo_handler, [], MetadataPut)
  ].

%% cowboy
allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>, <<"HEAD">>], Req, State}.

%% internal
handle_get(Req, State) ->
  Echo = application:get_env(example, echo, <<"">>),
  Body = jiffy:encode(#{<<"echo">> => Echo}),
  {Body, Req, State}.

handle_put(Req, State) ->
  {Echo, Req1} = cowboy_req:binding(echo, Req, ""),
  application:set_env(example, echo, Echo),
  Body = jiffy:encode(#{<<"echo">> => Echo}),
  Req2 = cowboy_req:set_resp_body(Body, Req1),
  {true, Req2, State}.
