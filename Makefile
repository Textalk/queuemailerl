DEPS = jiffy gen_smtp rabbit amqp_client

dep_smtp = git https://github.com/Vagabond/gen_smtp.git master

include erlang.mk
