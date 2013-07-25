# NOTE the best solution is to write
# http://www.gnu.org/software/automake/manual/html_node/Multiple-Outputs.html#Multiple-Outputs
all: file-a.out file-b.out

file-a.out: input.in
	echo starting
	sleep 2
	./foo-bin input.in file-a.out file-b.out   

file-b.out: file-a.out
	@if ! test -f $@; then \
		rm -f file-a.out; \
		$(MAKE) $(AM_MAKEFLAGS) file-a.out; \
	fi

clean :
	rm *out
