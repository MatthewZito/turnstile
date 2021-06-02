HC_CMD=stack
HL_CMD=hlint

TEST=$(HC_CMD) test
LINT=$(HL_CMD)

SCRIPTS_DIR=scripts
SRC_FILES=$(wildcard src/*.hs)

.PHONY: test
test:
	$(TEST)

lint:
	$(LINT) $(SRC_FILES)
