.PHONY: setup precommit test

setup:
	echo "make precommit" > ./.git/hooks/pre-commit && chmod +x ./.git/hooks/pre-commit

precommit:
	@mix format 
	@git update-index --again
	@mix credo --strict

test:
	@mix test --trace
