#See http://stackoverflow.com/a/10609434/1133157

all: file-a.out file-b.out

.INTERMEDIATE: stamp
stamp: input.in
	echo starting
	sleep 2
	./foo-bin input.in file-a.out file-b.out   

zzz file-b.out file-a.out: stamp

.PHONY: zzz

clean :
	rm *out
	rm stamp

