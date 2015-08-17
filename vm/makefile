CC=clang
CFLAGS=-c -Wall
LDFLAGS=
AR=ar
ARFLAGS=-cvq
SOURCES=vm.c heap.c gc.c io.c defs.c
OBJECTS=$(SOURCES:.c=.o)

LIB_NAME=libandovm.a

TEST_SOURCES=spec/spec_main.c spec/tiny_spec/tiny_spec.c spec/vm_spec.c spec/vm_equality_spec.c
TEST_OBJECTS=$(TEST_SOURCES:.c=.o)
TEST_EXECUTABLE=vm_spec

RM=rm
RMFLAGS=-f

copy_hs: lib
	mkdir -p ../dist/build/
	cp $(LIB_NAME) ../dist/build/

run_tests: tests
	./$(TEST_EXECUTABLE)

lib: $(LIB_NAME)
	
$(LIB_NAME): $(OBJECTS)
	$(AR) $(ARFLAGS) $(LIB_NAME) $(OBJECTS)


tests: $(SOURCES) $(TEST_SOURCES) $(TEST_EXECUTABLE)

$(TEST_EXECUTABLE): $(OBJECTS) $(TEST_OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) $(TEST_OBJECTS) -o $@

.c.o:
	$(CC) $(CFLAGS) $< -o $@


.PHONY: clean

clean:
	$(RM) $(RMFLAGS) *.o
	$(RM) $(RMFLAGS) spec/*.o
	$(RM) $(RMFLAGS) spec/tiny_spec/*.o
	$(RM) $(RMFLAGS) $(TEST_EXECUTABLE)
	$(RM) $(RMFLAGS) $(LIB_NAME)


