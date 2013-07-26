Thirdcake
---------

Thirdcake is a Makefile DSL written in Haskell. It can help you to define clean
and correct Makefile for your project. Currenly, only GNU Make dialect is
supported.

Why
===

Basically, everyone has make since it is a de-facto standard build tool.
Unfortunately, it is hard to write correct Makefile due to tricky syntax and
complex rules. As of today, Make has declarative and imperative variables,
builtin rules, pattern-rules, double-colon rules, C-style defines (which doesn't
work well with declarative variables) and much more.

Unfortunately, Make doesn't provide adequate solutions for a number of simple
problems:
  
  * It is not that simple to define a rule which generates several targets.
    Really,
        
        out1 out2 : in1 in2
            foo in1 in2 -o1 out1 -o2 out2

    is not corret. Read this [Automake article](http://www.gnu.org/software/automake/manual/html_node/Multiple-Outputs.html#Multiple-Outputs)
    if you are surprised.

  * Make doesn't track the usage of its variables, so the result of

        out : in
             foo $(FLAGS) -o $@ $^

    will not be rebuilt if someone change FLAGS. Hardly-trackable bugs can
    appear if a part of a big project was built with one set of optimisation
    flags and another part was mistakenly build with another set.


Installing
==========

  1. Install [Haskell Platform](http://www.haskell.org/platform/)

  2. Install dependencies
    
         cabal install haskell-src-meta monadloc QuasiText

  3. Build the thirdcake from Github (Thirdcake is not ready for Hackage yet)

         git clone http://github.com/grwlf/thirdcake
         cd thirdcake
         cabal configure && cabal install

Using
=====

Check Example/Foo/Cakefile



