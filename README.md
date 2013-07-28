Thirdcake
=========

Thirdcake is a Makefile DSL written in Haskell. It can help you to define clean
and correct Makefile for your project. Currenly, only GNU Make dialect is
supported.

Installing
----------

  1. Install [Haskell Platform](http://www.haskell.org/platform/)

  2. Install dependencies
    
         cabal install haskell-src-meta monadloc QuasiText

  3. Build the thirdcake from Github (Thirdcake is not ready for Hackage yet)

         git clone http://github.com/grwlf/thirdcake
         cd thirdcake
         cabal configure && cabal install

Why
---

Make is a build tool which was created more than 20 yesrs ago. It has a number
of versions and dialects. Basic Makefiles are really easy to write and
understand.  Unfortunately, it is hard to write real-world scale set of rules
correctly due to tricky syntax and lots of pitfails. As of today, Make has
automatic, declarative and imperative variables, builtin rules, pattern-rules,
double-colon rules, C-style ifdefs (which doesn't work well with declarative
variables) and lots of other strange things. Nevertheless, it is still widely
used as a de-facto standard tool which everyone has access to.

The goals of Thirdcake are to help the developer to:

  * Stop overusing Make by removing build-logic from Makefile into more sutable
    language
  * Still have a correct Makefile which could be distributed to endusers
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

### Features

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

### Limitations

  * Thirdcake doesn't support make-level includes. This is serious limitation,
    so I'm going to add that support ASAP.
  * No suuport for referencing a makefile-variable from within other variable.
    Again, lack of time. On the TODO list.
  * Resulting Makefile is actually a GNUMakefile. GNU extentions (shell function
    and others) are needed to make variable-guard tricks to work.
  * Coreutils package is required. Resulting Makefile uses md5sum and cut
    programs, distributed with this package.


How it works
------------


Thirdcake allows user to write Cakefile.hs in plain Haskell to define rules,
targets and other stuff as usual. After that, `cake3` compiles it into Makefile
(ghc is required for that). At this point, make will do the rest.

    vim Cakefile.hs >> cake3 >> repeat make

Again, in more details:

  1. User writes ./Cakefile.hs describing the rules. Rerer to
     Example/Foo/Cakefile.hs

  2. User executes `cake3` which compiles ./Cakefile.hs into `./Cakegen` and
     produces Makefile. Note that cake3 expects ./Cakegen to print content of
     the Makefile to it's standard output. Also, cake3 creates ./Cakefile_P.hs
     containing information about paths. Most important are _files_ function which
     translates relative _filename_ into _"." </> path_to_root </> filename_

  3. `make` can now be used to build the project. Note, that make knows how to
     update itself, so user doesn't have to run cake3 every time he or she
     changes ./Cakefile.hs.



