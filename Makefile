all: release

deps: 
	@rebar get-deps

compile: deps
	@rebar compile

release: compile
	@rebar generate

example: release
	@rm -rf example/rel/example
	@cd example && rebar compile && rebar generate
