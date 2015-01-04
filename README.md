# Сборка

`rebar get-deps`

`rebar compile`

# Запуск бенчмарка

`ERL_LIBS=apps:deps erl +K true -config config/app -name bench -s enode_bench`

# Запуск сервера

`ERL_LIBS=apps:deps erl +K true -config config/app -name server -s enode server`