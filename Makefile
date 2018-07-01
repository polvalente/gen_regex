setup:
	echo "make precommit" > ./.git/hooks/pre-commit && chmod +x ./.git/hooks/pre-commit

precommit:
	@mix format 
	@mix credo --strict
	@git update-index --again

