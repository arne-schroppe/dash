CC=clang
CFLAGS=-c -Wall
LDFLAGS=
SOURCES=vm.c heap.c gc.c io.c defs.c
OBJECTS=$(SOURCES:.c=.o)

TEST_SOURCES=spec/spec_main.c spec/tiny_spec/tiny_spec.c spec/vm_spec.c spec/vm_equality_spec.c
TEST_OBJECTS=$(TEST_SOURCES:.c=.o)
TEST_EXECUTABLE=vm_spec

RM=rm
RMFLAGS=-f


run_tests: tests
	./$(TEST_EXECUTABLE)

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


