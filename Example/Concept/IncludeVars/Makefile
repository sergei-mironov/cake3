
.PHONY:all
all: output

defines.mk : input
	./foo input

output: input
	echo $(VAR2)
	echo $(VAR) > $@

.PHONY:clean
clean:
	rm defines.mk
	rm output

VAR2 = bazzz

# Note, that defines.mk defines VAR. It's definition is below first reference
# Looks like it is safe to include things after all rules
include defines.mk
