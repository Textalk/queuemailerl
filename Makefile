DEPS = jiffy gen_stmp rabbit amqp_client

dep_smtp = git https://github.com/Vagabond/gen_smtp.git master

include erlang.mk
