all: release

compile:
	@rebar compile

release: compile
	@rebar generate
