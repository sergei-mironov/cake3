Thirdcake
=========

Thirdcake is a Makefile DSL written in Haskell. It can help you to define clean
and correct Makefile for your project. Currenly, only GNU Make dialect is
supported.

Why
---

Make is a build tool created more than 20 yesrs ago, it has a number of versions
and dialects. Basic Makefiles are really easy to write and understand.
Unfortunately, it is hard to write real-world scale rules set correctly due to
tricky syntax and complex extentions. As of today, Make has automatic,
declarative and imperative variables, builtin rules, pattern-rules, double-colon
rules, C-style ifdefs (which doesn't work well with declarative variables) and
many many other strange things. Nevertheless, it is still widely used as a
de-facto standard build tool which everyone has access to.

So the goal of Thirdcake is to provide a tool which would allow the developer to

  * Write Makefiles which could be distributed to the end users as is
  * Be sure that my Makefiles do right things
  * Take a bit of Haskell practice :)

Features and limitations
------------------------

Thirdcake follows Autoconf's path in a sence that it generates Makefile which
can be used with current and modified (up to some level) environment but has to
be regenerated when environment modifications exceeds that level.

Currently, the tool doesn't support pattern rules and variable-level functions
so nearly all the computatinos should be done at Haskell level. However, as a
reward, thirdcake protect the develper from a number of common make-specific
mistakes.

Features
````````

  * Rebuild a rule when variable changes. Consider following antipattern:

        # You often write rules like this, doesn't you?
        out : in
             foo $(FLAGS) -o $@ $^

    Unfortunately, changing FLAGS doesn't lead to rebuilding of out.
    Hardly-trackable bugs may appear if a part of a project was built with one
    set of optimisation flags and another part was build with another set by
    mistake.

    Thirdcake implements the makevar checksum
    [pattern](http://stackoverflow.com/a/17830736/1133157) from StackOverflow to
    detect changes in variables and rebuild targets when nessesary.
 
  * A rule with multiple target problem.
    
    It is not that simple to explain to Make that a rule generates several
    targets. Really,
        
        out1 out2 : in1 in2
            foo in1 in2 -o1 out1 -o2 out2

    is not corret. Read this [Automake
    article](http://www.gnu.org/software/automake/manual/html_node/Multiple-Outputs.html#Multiple-Outputs)
    if you are surprised. Thirdcake implements [.INTERMEDIATE
      pattern](http://stackoverflow.com/a/10609434/1133157) in such situations

  * Makefiles hierarchy. Say, we have a project A with subproject L. L has it's
    own Makefiles and we want to re-use it in our global A/Makefile. Make
    provides only two ways of doing that. We could either include L/Makefile or
    call $(MAKE) -C L. First solution is a pain because we would probably merge
    together two files with different current directory assumptions. Second
    approach is OK unless we need to pass additional paramters or depend on a
    specific rule from L.

    Thirdcake's approach in this case is a compromise: it uses standard Haskell
    import mechanism so it is possible to import L/Cakelib.hs from
    A/Cakefile.hs and do whatever you  want to, but resulting makefiles will
    always be monolitic.

Limitations
```````````

  * Thirdcake doesn't support make-level includes. This is serious limitation,
    so I'm going to add that support ASAP.
  * No suuport for referencing a makefile-variable from within other variable.
    Again, lack of time. On the TODO list.
  * Resulting Makefile is actually a GNUMakefile. GNU extentions (shell function
    and others) are needed to make variable-guard tricks to work.
  * Coreutils package is required. Resulting Makefile uses md5sum and cut
    programs, distributed with this package.


How does it work
----------------

Thirdcake allows user to write Cakefile using a subset of Haskell (still,
Cakefiles are normal Haskell programs) to define rules, targets and other stuff
as usual. When compiled (ghc is required for that), Cakefile should be run in
order to print corresponding GNU Makefile, understandable by GNU make.

Again, refer to Example/Foo/Cakefile to see an example.


Installing
----------

  1. Install [Haskell Platform](http://www.haskell.org/platform/)

  2. Install dependencies
    
         cabal install haskell-src-meta monadloc QuasiText

  3. Build the thirdcake from Github (Thirdcake is not ready for Hackage yet)

         git clone http://github.com/grwlf/thirdcake
         cd thirdcake
         cabal configure && cabal install

Using
-----

Check Example/Foo/Cakefile



