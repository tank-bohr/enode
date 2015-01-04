# Сборка

`rebar get-deps`

`rebar compile`

# Запуск

`ERL_LIBS=apps:deps erl -config config/app -s enode`

# Запуск сервера

`ERL_LIBS=apps:deps erl -config config/app -s enode server`