all: release

compile:
	@rebar compile

release: compile
	@rebar generate

example: release
	@rm -rf example/rel/example
	@cd example && rebar compile && rebar generate
