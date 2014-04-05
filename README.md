# Livescript to AST to Escodegen (and maybe sourcemaps)

This fork is a work-in-progress branch to change the Livescript
compiler's output from JS text to the Parser API AST, which can
then be further optimized/analyzed, or generated back to JS text
with escodegen. This gets us about halfway to source maps, at
the cost of further deviation from the CoffeeScript/coco codebases
while still falling short of full CoffeeScriptRedux.

I've been working on it on and off for the past months. I felt it time to at
least stick it on github publicly, so other interested parties can
potentially help/comment.

## Still TODO

- Fix label compilation
- Fix reuse of temporary variables.
- Somehow retain JS comments in AST output.

## How to develop

While you can run the unit tests with slake, it's easier at this
point to just compare the compiled output until it's identical.

There are two included tools for this purpose, `regen.ls` and `tast.ls`.
Use them with your favorite diff program (I like meld) like so (using zsh's incredibly helpful temporary file substitution syntax):

    TEST=test/label.ls
    meld =(lsc -cbp $TEST | lsc regen.ls) =(lsc -aj $TEST | lsc tast.ls) 

This takes the stable version's output, passes it through esprima/escodegen to
take out any trivial formatting differences, and compares it with the new
compilation using `ast.ls` in the `src/` directory (which takes the LiveScript
AST as input and outputs Parser API ast, then puts it through escodegen).

If the files are identical, they should compile to the same text. If not, it
should be easier to tell where the new compiler went wrong.

If the new compiler bails out completely with an error, you can inspect
the generated JS AST by passing an extra argument to `tast`:

    lsc -aj $TEST | lsc tast.ls give-me-the-ast

If there is JS ast that is mistakenly `toString`ed by the old codegen,
it should show up as a string in a weird place.

Thus, the feedback loop is:

1. Run meld on a test file
2. Fix `src/ast.ls`
3. GOTO 1 unless file is fixed
4. If every test file compiles identically, run `slake build` and
   see if the whole compiler recompiles. If it does, you're done!

## Other notes

In some ways, I feel like this is the wrong way to go, and we should instead
just scrap the ad-hoc compiler for a CSR-based design. In other ways though,
the old compiler is still pretty nice, and "going halfway" will still
kind-of-allow bug fixes to propagate from old coco and coffeescript.

However, in the process, I've unraveled some of the ultra-compact code from
coco and added comments, which might be helpful regardless.

# (Original Readme) LiveScript
is a language which compiles to JavaScript. It has a straightforward mapping to JavaScript and allows you to write expressive code devoid of repetitive boilerplate. While LiveScript adds many features to assist in functional style programming, it also has many improvements for object oriented and imperative programming.

Check out **[livescript.net](http://livescript.net)** for more information, examples, usage, and a language reference.

### Build Status
[![Build Status](https://travis-ci.org/gkz/LiveScript.png?branch=master)](https://travis-ci.org/gkz/LiveScript)

### Install
Have Node.js installed. `sudo npm install -g LiveScript`

After, run `livescript` for more information.


### Source
[git://github.com/gkz/LiveScript.git](git://github.com/gkz/LiveScript.git)
