APP=logi

all: compile xref eunit dialyze

init:
	@eval "if ! [ -f 'src/${APP}.app.src' ]; then ./rebar create-app appid=${APP}; fi"
	@./rebar get-deps compile

compile:
	@./rebar compile skip_deps=true

xref:
	@./rebar xref skip_deps=true

clean:
	@./rebar clean skip_deps=true
	@rm -f .dialyze.plt

eunit:
	@./rebar eunit skip_deps=true

edoc:
	@./rebar doc skip_deps=true

start: compile
	@erl -pz ebin deps/*/ebin -eval 'erlang:display({start_app, $(APP), application:ensure_all_started($(APP))}).'

.dialyzer.plt:
	touch .dialyzer.plt
	dialyzer --build_plt --plt .dialyzer.plt --apps erts kernel stdlib

dialyze: .dialyzer.plt compile
	dialyzer --no_native --plt .dialyzer.plt -r ebin
