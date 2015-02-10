%% @doc Tests trivial things that we don't test in other tests, mostly for coverage.
-module(trivial_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

gen_servers_test_() ->
    {setup,
     fun () -> error_logger:tty(false) end,
     fun (_) -> error_logger:tty(true) end,
     fun () ->
         %% We silence the error log for these assertions. They talk a lot.
         ?assertError(badarg, queuemailerl_listener:handle_call(foo, {self(), tag}, x)),
         ?assertEqual({noreply, x}, queuemailerl_listener:handle_info(foo, x)),
         ?assertEqual({ok, x}, queuemailerl_listener:code_change(42, x, foo)),
         ?assertEqual({noreply, x},
                      queuemailerl_listener:handle_info({#'basic.deliver'{},
                                                         #amqp_msg{payload = <<"{}">>}},
                                                        x)),
         ?assertEqual({ok, x}, queuemailerl_listener:code_change(42, x, foo)),

         ?assertError(badarg, queuemailerl_smtp_worker:handle_call(foo, {self(), tag}, x)),
         ?assertError(badarg, queuemailerl_smtp_worker:handle_cast(foo, x)),
         ?assertEqual({noreply, x}, queuemailerl_smtp_worker:handle_info(foo, x)),
         ?assertEqual({ok, x}, queuemailerl_smtp_worker:code_change(42, x, foo)),

         ok
     end}.
