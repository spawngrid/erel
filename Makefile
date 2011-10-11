all: release

compile:
	@rebar compile

release: compile
	@rebar generate

example: release
	@rm -rf apps/erel/example/rel/example
	@cd apps/erel/example && rebar generate
