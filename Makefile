DEPS = jiffy gen_smtp rabbit amqp_client

dep_smtp = git https://github.com/Vagabond/gen_smtp.git master

RABBITMQ_CLIENT_PATCH = yes

include erlang.mk
