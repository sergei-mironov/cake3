#See http://stackoverflow.com/a/10609434/1133157

all: q1_good file-b.out

all_bug: q1_bug file-b.out

q1_bug: file-a.out
	cat file-a.out > q1_bug

q1_good: file-a.out file-b.out
	cat file-a.out > q1_good

.PHONY: dosmth
dosmth:
	echo dosmth

.PHONY: zzz1 zzz2
zzz1 file-b.out zzz2 file-a.out: dosmth stamp

stamp: input.in
	echo starting
	sleep 2
	./foo-bin input.in file-a.out file-b.out   
	touch stamp



clean :
	rm *out
	rm stamp

