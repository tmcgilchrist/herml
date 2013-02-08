all: compile

test: compile
	rebar eunit

compile:
	rebar compile

clean:
	rebar clean
