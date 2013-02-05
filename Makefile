all: compile

test:
	rebar eunit

compile:
	rebar compile

clean:
	rebar clean
