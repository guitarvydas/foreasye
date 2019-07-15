(install gprolog - on mac, don't install from source, use brew or other)
make
./run.sh

to save you some time:
- look at calc_bound.pl
- (dashes not allowed, but underscore is)
- IDs that begin with a Capital letter (or _) represent logic variables
- Everything else, atoms and rules must begin with lower-case
- Arity is (sometimes) specified by "/N" after the rule name.
- And-ness is specified by a comma ","
- Or-ness is specified by rules of the same name and arity
- All rules with the same name and arity must be grouped together ally
- All rules end with a period '.'
- ":- initialization(main)" is the gprolog way to tell the compiler to start at "main"
- ":- dynamic(xxx)" is the gprolog way to tell the compiler that the rule might not be hard-coded and must be compiled as a dynamic rule
- A "factbase" is my name for a triple-store (db),
- A "fact" is a triple, in prolog "reln(subj,obj)".
- I build a factbase in a bash pipeline - I take a few simple rules and extend them by adding new facts into the factbase (thus, I need to use ":- dynamic(xxx)" for each one of those kind of facts).
- The Meat of calc_bounds.pl is "main" and createBoundingBoxes", the rest is factbase read-in and write-out.
- readFactBase reads a factbase from stdin (called user_input in gprolog) and calls "element" recursively to read in the facts (from the previous pass).
- writeFactBase writes out the factbase.  It calls "writeTerm" for every kind of fact that I'm interested in.
forall(Term,action) - does "action" for every Term that matches (kinda like maphas and friends).
