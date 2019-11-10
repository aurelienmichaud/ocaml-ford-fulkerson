SRC_DIR=src
BUILD_DIR=./_build/
COMP=ocamlbuild
COMP_FLAGS=-I $(SRC_DIR)
BUILT_FILENAME=ftest.native

first:
	$(COMP) $(BUILT_FILENAME) $(COMP_FLAGS)

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)
	rm -f ./$(BUILT_FILENAME)

