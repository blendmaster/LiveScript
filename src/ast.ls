# Contains all of the node classes for the AST (abstract syntax tree).
# Most nodes are created as the result of actions in the [grammar](#grammar),
# but some are created by other nodes as a method of code generation.
# To convert the syntax tree into a string of JavaScript code,
# call `Block::compileRoot`.

require! esprima
$ = require \./js

### Node
# The abstract base class for all nodes in the syntax tree.
# Each subclass implements the `compileNode` method, which performs the
# code generation for that node. To compile a node to JavaScript,
# call `compile` on it, which wraps `compileNode` in some generic extra smarts.
# An options hash is passed and cloned throughout, containing information about
# the environment from higher in the tree (such as if a returned value is
# being requested by the surrounding function), information about the current
# scope, and indentation level.
(Node = -> ...):: =
  compile: (options, level) ->
    o = {} <<< options
    o.level? = level
    node = @unfoldSoak o or this
    # If a statement appears within an expression, wrap it in a closure.
    return node.compileClosure o if o.level > LEVEL_TOP and node.isStatement!
    code = node.compileNode o
    if node.temps then for tmp in that then o.scope.free tmp
    code

  compileClosure: (o) ->
    # A statement that _jumps_ out of current context (like `return`) can't be
    # an expression via closure-wrapping, as its meaning will change.
    that.carp 'inconvertible statement' if @getJump!
    fun = Fun [] Block this; call = Call!
    var hasArgs, hasThis
    @traverseChildren !->
      switch it.value
      | \this      => hasThis := true
      | \arguments => hasArgs := it.value = \args$
    if hasThis
      call.args.push Literal \this
      call.method = \.call
    if hasArgs
      call.args.push Var \arguments
      fun.params.push Var \args$
    # Flag the function as `wrapper` so that it shares a scope
    # with its parent to preserve the expected lexical scope.
    Parens(Chain fun<<<{+wrapper, @void} [call]; true)compile o

  # Compiles a child node as a block statement.
  compileBlock: (o, node) ->
    $.BlockStatement body:
      if node?
        js = node.compile o, LEVEL_TOP
        if js instanceof $.Statement
          # XXX unwrap inner block statements
          if js instanceof $.BlockStatement
            js.body
          else
            [js]
        else
          [$.ExpressionStatement expression: js]
      else
        []

  # If the code generation wishes to use the result of a complex expression
  # in multiple places, ensure that the expression is only ever evaluated once,
  # by assigning it to a temporary variable.
  cache: (o, once, level) ->
    unless @isComplex!
      return [if level? then @compile o, level else this] * 2
    sub = Assign ref = Var(o.scope.temporary!), this
    # Pass a `level` to precompile.
    if level?
      sub.=compile o, level
      o.scope.free ref.value if once
      return [sub, ref.value]
    # If flagged as `once`, the tempvar will be auto-freed.
    if once then [sub, ref <<< {+temp}] else [sub, ref, [ref.value]]

  # caches to a variable/source pair suitable for looping.
  # returns [ref, assign] if cached
  # or [this] if we don't need caching
  cacheLoopReference: (o, name, ret) ->
    if this instanceof Var
    or this instanceof Unary and @op in <[ + - ]> and -1/0 < +@it.value < 1/0
    or this instanceof Literal and not @isComplex!
      return [this] # just the expr
    else
      asn = Assign Var(tmp = o.scope.temporary name), this
      asn.void = true unless ret
      return [Var tmp; asn]

  # Passes each child to a function, returning its return value if exists.
  eachChild: (fn) ->
    for name in @children when child = @[name]
      if \length of child
        for node, i in child then return that if fn(node, name, i)
      else return that if fn(child, name   )?

  # Performs `eachChild` on every descendant.
  # Overridden by __Fun__ not to cross scope by default.
  traverseChildren: (fn, xscope) ->
    @eachChild (node, name, index) ~>
      fn(node, this, name, index) ? node.traverseChildren fn, xscope

  # Performs anaphoric conversion if a `that` is found within `@aTargets`.
  anaphorize: ->
    @children = @aTargets
    if @eachChild hasThat
      if (base = this)[name = @aSource] instanceof Existence
        base.=[name]; name = \it
      unless base[name]value is \that
        base[name] = Assign Var(\that), base[name]
    function hasThat
      it.value is \that or if it.aSource
      then hasThat that if it[that]
      else it.eachChild hasThat
    delete @children
    @[@aSource] <<< {+cond}

  # Throws a syntax error, appending `@line` number to the message.
  carp: (msg, type = SyntaxError) ->
    throw type "#msg on line #{ @line or @traverseChildren -> it.line }"

  # Defines delegators.
  delegate: !(names, fn) ->
    for let name in names
      @[name] = -> fn.call this, name, it

  # Default implementations of the common node properties and methods. Nodes
  # will override these with custom logic, if needed.
  children: []

  terminator: \;

  isComplex: YES

  isStatement  : NO
  isAssignable : NO
  isCallable   : NO
  isEmpty      : NO
  isArray      : NO
  isString     : NO
  isRegex      : NO

  isMatcher: -> @isString! or @isRegex!

  # Do I assign a certain variable?
  assigns: NO

  # Picks up name(s) from LHS.
  ripName: VOID

  unfoldSoak   : VOID
  unfoldAssign : VOID
  unparen      : THIS
  unwrap       : THIS
  maybeKey     : THIS
  expandSlice  : THIS
  varName      : String
  getAccessors : VOID
  getCall      : VOID
  getDefault   : VOID
  # Digs up a statement that jumps out of this node.
  getJump      : VOID

  invert: -> Unary \! this, true

  invertCheck: ->
    if it.inverted then @invert! else this

  addElse: (@else) -> this

  # Constructs a node that returns the current node's result.
  makeReturn: (arref) ->
    if arref
      # add value into current array return buffer
      Chain Var arref
        ..add Index Key \push
        ..add Call [this]
    else
      Return this

  makeObjReturn: (arref) ->
    if arref
      base = this.lines.0
      base.=then.lines.0 if this.lines.0 instanceof If
      items = base.items
      if not items.0? or not items.1?
        @carp 'must specify both key and value for object comprehension'
      Assign (Chain Var arref).add(Index items.0, \., true), items.1
    else Return this

  # Extra info for `toString`.
  show: String

  # String representation of the node for inspecting the parse tree.
  # This is what `livescript --ast` prints out.
  toString: (idt or '') ->
    tree  = \\n + idt + @constructor.displayName
    tree += ' ' + that if @show!
    @eachChild !-> tree += it.toString idt + TAB
    tree

  # JSON serialization
  stringify : (space) -> JSON.stringify this, null space
  toJSON    : -> {type: @constructor.displayName, ...this}

# JSON deserialization
exports.parse    = (json) -> exports.fromJSON JSON.parse json
exports.fromJSON = function
  return it unless it and typeof it is \object
  if it.type
    # XXX the Switch statement has its own `type` field which
    # clobbers the constructor type, so fix
    if it.type is \switch or it.type is \match
      node = ^^exports.Switch::
    # XXX the Slice expression sometimes has type = 'to' or 'til'
    else if it.type is \to or it.type is \til
      node = ^^exports.Slice::
    else
      try
        node = ^^exports[it.type]::
      catch
        throw new Error("can't find #{JSON.stringify it}")

    for key, val of it then node[key] = fromJSON val
    return node
  if it.length? then [fromJSON v for v in it] else it

#### Mixins

Negatable =
  show   : -> @negated and \!
  invert : -> !=@negated; this

#### Block
# A list of expressions that forms the body of an indented block of code.
class exports.Block extends Node
  (body || []) ~>
    if \length of body
      @lines = body
    else
      @lines = []
      @add body

  children: [\lines]

  toJSON: -> delete @back; super!

  add: ->
    it.=unparen!
    switch
    | @back     => that.add it
    | it.lines  => @lines.push ...that
    | otherwise =>
      @lines.push it
      @back = that if delete it.back
    this

  prepend: ->
    @lines.splice @neck!, 0, ...arguments
    this

  pipe: (target, type) ->
    args = if type is \|> then @lines.pop! else target
    args = [args] if typeof! args isnt \Array
    switch type
    | \|>  => @lines.push Call.make(target,      args, pipe: true)
    | \<|  => @lines.push Call.make(@lines.pop!, args)
    this

  unwrap: -> if @lines.length is 1 then @lines.0 else this

  # Removes trailing comment nodes.
  # TODO support comment AST nodes that aren't JS
  chomp: ->
    {lines} = this; i = lines.length
    while lines[--i] then break unless that.comment
    lines.length = i + 1
    this

  # Finds the right position for inserting variable declarations, i.e.
  # after any "use strict" or block comments
  neck: ->
    pos = 0
    for x in @lines
      break unless x.comment or x instanceof Literal
      ++pos
    pos

  isComplex: -> @lines.length > 1 or @lines.0?isComplex!

  ::delegate <[ isCallable isArray isString isRegex ]> -> @lines[*-1]?[it]!

  getJump: -> for node in @lines then return that if node.getJump it

  # **Block** does not return its entire body, rather it
  # ensures that the final line is returned.
  makeReturn: ->
    @chomp!
    if @lines[*-1]?=makeReturn it
      --@lines.length if that instanceof Return and not that.it
    this

  compile: (o, level ? o.level) ->
    if level > LEVEL_TOP
      @compileExpressions o, level
    else
      $.BlockStatement body: @compileBlockBody o, level

  compileBlockBody: (o, level ? o.level) ->
    o.block = this

    statements = []
    for node in @lines
      node = node.unfoldSoak o or node
      ast = (node <<< {+front, +void})compile o, level
      if ast instanceof $.Statement
        if ast instanceof $.BlockStatement
          # unwrap JS block statement, as it's pretty much useless
          statements.push ...ast.body
        else
          statements.push ast
      else
        statements.push $.ExpressionStatement expression: ast
    return statements

  # **Block** is the only node that can serve as the root.
  compileRoot: (options) ->
    o = {level: LEVEL_TOP, scope: @scope = Scope.root = new Scope, ...options}

    if saveTo = delete o.saveScope
       o.scope = saveTo.savedScope or= o.scope # use savedScope as your scope

    delete o.filename

    # TODO I think this is a hack to compile stuff in querystrings
    # or bookmarklets, disabling for now
    # if /^\s*(?:[/#]|javascript:)/test @lines.0?code
    #  prefix = @lines.shift!code + \\n

    if delete o.eval and @chomp!lines.length
      if o.bare then @lines.push Parens @lines.pop! else @makeReturn!

    body = @compileWithDeclarations o

    $.Program body:
      if o.bare
        body
      else
        # (function() { ... }).call(this)
        [
          $.ExpressionStatement expression: $.CallExpression do
            callee: $.MemberExpression do
              computed: false
              object: $.FunctionExpression params: [], body:
                $.BlockStatement {body}
              property: $.Identifier name: \call
            arguments: [$.ThisExpression!]
        ]

  # Compile to an array of JS Statements, including VariableDeclarations
  # and FunctionStatements for this scope
  compileWithDeclarations: (o) ->
    o.level = LEVEL_TOP
    pre = []
    if (i = @neck!) is not 0
      # temporarily remove the rest of the lines of this Block,
      # compile the initial lines, then replace our state with the rest
      rest   = @lines.splice i, 9e9
      pre    = @compileBlockBody o
      @lines = rest

    post = @compileBlockBody o

    if @scope?
      # prepend and append vars and functions respectively
      post = @scope.add-scope post, o

    return pre ++ post

  # Compile to a SequenceExpression i.e. comma-separated list of expressions.
  # Blocks with only one expression compile to the expression itself for
  # simplification.
  compileExpressions: (o, level) ->
    {lines} = @chomp!; i = -1
    # remove starting comments
    while lines[++i] then lines.splice i-- 1 if that.comment

    lines.push Literal \void unless lines.length

    lines.0 <<< {@front}; lines[*-1] <<< {@void}

    if lines.length is 1
      # level is > LEVEL_TOP, so this shouldn't result in a Statement
      return lines.0.compile o, level
    else
      expr = for node in lines
        (node <<< {+void})compile o, LEVEL_PAREN
      $.SequenceExpression expressions: expr

#### Atom
# An abstract node for simple values.
class Atom extends Node
  show      : -> @value
  isComplex : NO

#### Literal
# `this`, `debugger`, regexes and primitives.
class exports.Literal extends Atom
  (@value) ~>
    return new Super        if value is \super

  isEmpty    : -> @value in <[ void null ]>
  isCallable : -> @value in <[ this eval .. ]>
  isString   : -> 0 <= '\'"'indexOf "#{@value}"charAt!
  isRegex    : -> "#{@value}"charAt! is \/
  isComplex  : -> @isRegex! or @value is \debugger
  isWhat     : ->
    | @isEmpty!    => \empty
    | @isCallable! => \callable
    | @isString!   => \string
    | @isRegex!    => \regex
    | @isComplex!  => \complex
    | otherwise    => void

  varName: -> if /^\w+$/test @value then \$ + @value else ''

  compile: (o, level ? o.level) ->
    switch val = "#{@value}"
    | \this      =>
      if o.scope.fun?bound
        $.Identifier name: that
      else
        $.ThisExpression!
    | \arguments =>
      # XXX not actually a literal, but the lexer seems to think so
      $.Identifier name: \arguments
    | \void      =>
      if level is LEVEL_TOP
        throw new Error this
        $.EmptyStatement!
      else
        # void 8
        $.UnaryExpression operator: \void, argument: $.Literal value: 8
    | \null      =>
        $.Literal value: null
    | \on \yes   =>
        $.Literal value: true
    | \off \no   =>
        $.Literal value: false
    | \*         => @carp 'stray star'
    | \..        =>
      @carp 'stray reference' unless o.ref
      if not @cascadee then o.ref.erred = true
      $.Identifier name: o.ref
    | \debugger  =>
      if level > LEVEL_TOP
        # wrap debugger statement in IEFE
        $.FunctionExpression params: [], body:
          $.BlockStatement body: $.DebuggerStatement!
      else
        $.DebuggerStatement!
    | otherwise =>
      # XXX @let replacement for `for let` loops
      if @abused-by-let
        @let-abuse # already compiled
      else if @abused-by-star
        @value # already compiled
      else
        try
          # JS AST requires actual strings, numbers, regexes etc
          # so we must eval our string sources. We could do more
          # strict vivification, but given that we're in a JS execution
          # environment anyway, might as well defer to it for parsing.
          $.Literal value: eval val
        catch
          throw new Error "couldn't eval literal #val : #e"

#### Var
# Variables.
class exports.Var extends Atom
  (@value) ~>
    if /\s/.test @value
      throw new Error "probably not a var: #{@value}"

  ::isAssignable = ::isCallable = YES

  assigns: -> it is @value

  maybeKey: -> Key(@value) <<< {@line}

  varName: ::show

  compile: (o) ->
    if @temp
      o.scope.free @value

    $.Identifier name: @value

#### Key
# A property name in the form of `{key: _}` or `_.key`.
class exports.Key extends Node
  (name, @reserved or name.reserved) ~> @name = '' + name

  isComplex: NO

  assigns: -> it is @name

  varName: ->
    {name} = this
    if @reserved or name in <[ arguments eval ]> then "$#name" else name

  show: -> if @reserved then "'#{@name}'" else @name

  compile: ->
    if @reserved
      # need to compile as a literal string for ES3 compliance
      $.Literal value: "#{@name}"
    else
      $.Identifier {@name}

#### Index
# Dots and brackets to access an object's property.
class exports.Index extends Node
  # XXX if symbol is [] or {}, it means to vivify, otherwise it
  # MUST be \.
  (key, symbol or \., init) ~>
    if init and key instanceof Arr
      switch key.items.length
      | 1 => key = Parens k unless (k = key.items.0) instanceof Splat
    switch symbol
    | '[]' => @vivify = Arr
    | '{}' => @vivify = Obj
    | _    =>
      @assign = symbol.slice 1 if \= is symbol.slice -1
    import {key, symbol}

  children: [\key]

  show: -> [\? if @soak] + @symbol

  isComplex: -> @key.isComplex!

  varName: -> @key instanceof [Key, Literal] and @key.varName!

  compile: (o) ->
    # Chain compilation handles all of the fancy work with @vivify and @assign
    # and @soak, so if we're actually getting compiled, we are just a simple
    # MemberExpression at this point

    js-key = @key.compile o, LEVEL_PAREN

    $.MemberExpression do
      # object is set by Chain::compile
      property: js-key

      # Key AST nodes are dotted property access.
      # Key nodes which are reserved named are also [], i.e.
      # uncomputed
      computed: @key not instanceof Key or js-key instanceof $.Literal

#### Slice
# slices away at the target
class exports.Slice extends Node
  ({@type, @target, @from, @to}) ~>
    @from ?= Literal 0
    @to = Binary \+ @to, Literal \1 if @to and @type is \to

  children: [\target \from \to]

  show: -> @type

  compileNode: (o) ->
    @to = Binary \|| @to, Literal \9e9 if @to and @type is \to
    args = [@target, @from]
    args.push @to if @to
    Chain Var (util \slice) .add Index (Key \call), \. true .add Call args .compile o

#### Chain
# Acts as a container for property-access/function-call chains, by holding
# __Index__ or __Call__ instances as `@tails`.
class exports.Chain extends Node
  (head, tails) ~>
    return head if not tails and head instanceof Chain
    import {head, tails or []}

  children: <[ head tails ]>

  add: ->
    if @tails.length
      last = @tails[*-1]
      # optimize `x |> f 1, _` to `f(1, x)`
      if last instanceof Call
      and last.partialized?length is 1
      and it.args.length is 1
        index = last.partialized.0.head.value # Chain Literal i
        delete last.partialized
        last.args[index] = it.args.0 # extract the single arg from pipe call
        return this
    if @head instanceof Existence
      {@head, @tails} = Chain @head.it
      it.soak = true
    @tails.push it
    bi = if @head instanceof Parens and @head.it instanceof Binary
         and not @head.it.partial then @head.it
         else if @head instanceof Binary and not @head.partial then @head
    if @head instanceof Super
      if not @head.called and it instanceof Call and not it.method
        it.method = \.call
        it.args.unshift Literal \this
        @head.called = true
      else if not @tails.1 and it.key?name is \prototype
        @head.sproto = true
    else if delete it.vivify
      @head = Assign Chain(@head, @tails.splice 0, 9e9), that!, \= \||
    else if it instanceof Call and @tails.length is 1
    and bi and bi.op in logics = <[ && || xor ]>
      call = it
      f = (x, key) ->
        y = x[key]
        if y instanceof Binary and y.op in logics
        then f y, \first; f y, \second
        else x[key] = Chain y .auto-compare call.args
      f bi, \first
      f bi, \second
      return bi
    this

  auto-compare: (target) ->
        test = this.head
        switch
        | test instanceof Literal
          Binary \===  test, target.0
        | test instanceof Unary and test.it instanceof Literal
          Binary \===  test, target.0
        | test instanceof Arr, test instanceof Obj
          Binary \==== test, target.0
        | test instanceof Var and test.value is \_
          Literal \true
        | otherwise
          this .add Call target or []

  flipIt: -> @flip = true; this

  # __Chain__ can be unwrapped as its inner node, if there are no subnodes.
  unwrap: -> if @tails.length then this else @head

  ::delegate <[ getJump assigns isStatement isString ]>
           , (it, arg) -> not @tails.length and @head[it] arg

  isComplex  : -> @tails.length or @head.isComplex!
  isCallable : ->
    if @tails[*-1] then not that.key?items else @head.isCallable!
  isArray    : ->
    if @tails[*-1] then that.key instanceof Arr else @head.isArray!
  isRegex    : ->
    @head.value is \RegExp and not @tails.1 and @tails.0 instanceof Call

  isAssignable: ->
    return @head.isAssignable! unless tail = @tails[*-1]
    return false if tail not instanceof Index
                 or tail.key instanceof List
                 or tail.symbol is \.~
    for tail in @tails when tail.assign then return false
    true

  # `@$` `o.0`
  isSimpleAccess: ->
    @tails.length is 1 and not @head.isComplex! and not @tails.0.isComplex!

  makeReturn: -> if @tails.length then super ... else @head.makeReturn it

  getCall: -> (tail = @tails[*-1]) instanceof Call and tail

  varName: -> @tails[*-1]?varName!

  # A reference has base part (`this` value) and name part.
  # We cache them separately for compiling complex expressions, so that e.g.
  #
  #     a()[b()] ||= c
  #
  # compiles to
  #
  #     (ref$ = a())[key$ = b()] || (ref$[key$] = c);
  #
  cacheReference: (o) ->
    name = @tails[*-1]
    # `a.b()`
    return @cache o, true if name instanceof Call
    # `a` `a.b`
    if @tails.length < 2 and not @head.isComplex! and not name?isComplex!
      return [this] * 2
    base = Chain @head, @tails.slice 0 -1
    # `a().b`
    if base.isComplex!
      ref  = o.scope.temporary!
      base = Chain Assign Var(ref), base
      bref = Var(ref) <<< {+temp}
    # `a{}`
    return [base, bref] unless name
    # `a[b()]`
    if name.isComplex!
      ref  = o.scope.temporary \key
      name = Index Assign Var(ref), name.key
      nref = Index Var(ref) <<< {+temp}
    [base.add name; Chain bref || base.head, [nref or name]]

  compileNode: (o) ->
    if @flip
      util \flip
      util \curry
    {head, tails} = this; head <<< {@front, @newed}
    return head.compile o unless tails.length
    return that.compile o if @unfoldAssign o
    for t in tails when t.partialized then has-partial = true; break
    if has-partial
      util \slice
      pre  = []
      rest = []
      for t in tails
        broken = broken or t.partialized?
        if   broken
        then rest.push t
        else pre .push t
      [partial, ...post] = rest if rest?
      @tails = pre
      context = if pre.length then Chain head, pre[til -1] else Literal \this

      return (Chain (Chain Var util \partialize
        .add Index Key \apply
        .add Call [context, Arr [this; Arr partial.args; Arr partial.partialized]]), post).compile o

    @carp 'invalid callee' if tails.0 instanceof Call and not head.isCallable!

    @expandSlice o; @expandBind o; @expandSplat o; @expandStar o

    if @splatted-new-args
      # compile call to splatNew utility function which applies
      # arguments to a newly constructed `clazz`
      clazz = Chain @head, tails.slice 0 -1
      return (Chain Var util \splatNew
        .add Call [clazz, @splatted-new-args]).compile o

    # compile our flattened Chain node into nested JS AST expression nodes
    # we keep mutating the `js` variable to wrap the previous node
    # in the next Call/Member Expression,
    #
    # e.g.
    #
    # a.b.c
    #
    # compiles like:
    #
    # a
    # =>
    # {object: a, property: b}
    # =>
    # {object: {object: a, property: b}, property: c}
    js = @head.compile o

    for node in @tails
      js-node = node.compile o
      if js-node instanceof $.MemberExpression
        js-node.object = js
      else if js-node instanceof $.NewExpression
        js-node.callee = js
      else # call expression
        js-node.callee =
          # XXX compile Call.method = '.apply'/'.call' abuse as
          # separate CallExpression
          if node.method?
            $.MemberExpression do
              object: js
              property: $.Identifier name: that.substring 1
          else
            js

      js = js-node

    return js

  # Unfolds a soak into an __If__: `a?.b` => `a.b if a?`
  unfoldSoak: (o) ->
    if @head.unfoldSoak o
      that.then.tails.push ...@tails
      return that
    for node, i in @tails when delete node.soak
      bust = Chain @head, @tails.splice 0 i
      node.carp 'invalid accessign' if node.assign and not bust.isAssignable!
      test = if node instanceof Call
        [test, @head] = bust.cacheReference o
        # typeof it === 'function'
        Binary \===,
          Unary \typeof test
          Literal "'function'"
      else
        if i and node.assign
          [test, bust] = bust.cacheReference o
          @head = bust.head; @tails.unshift ...bust.tails
        else
          [test, @head] = bust.unwrap!cache o, true
        Existence test
      return If(test, this) <<< {+soak, @cond, @void}

  unfoldAssign: (o) ->
    if @head.unfoldAssign o
      that.right.tails.push ...@tails
      return that
    for index, i in @tails then if op = index.assign
      index.assign = ''
      left = Chain @head, @tails.splice 0 i .expandSlice o .unwrap!
      if left instanceof Arr
        # `[a, b].=reverse()` => `[a, b] = [a, b].reverse()`
        lefts = left.items; {items: rites} = @head = Arr!
        for node, i in lefts
          [rites[i], lefts[i]] = Chain node .cacheReference o
      else
        [left, @head] = Chain left .cacheReference o
      op = \:= if op is \=
      return Assign(left, this, op) <<< {+access}

  expandSplat: !(o) ->
    {tails} = this
    i = -1
    while call = tails[++i]
      continue unless call.args
      # i.e. call _is_ actually a Call, not and Index or something

      args = call.args.slice!

      # shift `this` context if already present, else ctx = void
      ctx = if call.method is \.call then args.shift!

      seen = false
      for node in args
        seen = true if node instanceof Splat
      # if there are no splats, don't bother doing anything
      continue unless seen

      args-expr = Splat.expandArray args, true # apply = true

      if call.new
        # we can't do the prototype-setting `new` with .apply() semantics, so
        # save the args to be compiled using a special hack in `compileNode`
        @splatted-new-args = args-expr
      else
        # if there is no 'this' context and the last part of the chain
        # was an index [] access, cache everything before this part of the
        # chain as a temporary variable, and make that the context
        #
        # e.g.
        #
        # a[b].c(...d)
        #
        # # =>
        #
        # (temp = a[b]).c.apply(temp, d)
        #
        # This ensures that a possibly complicated index expression (b) isn't
        # evaluated more than once.
        if not ctx and tails[i-1] instanceof Index
          cached-tails = tails.splice 0 i-1

          # assign the new @head of this chain and the ctx of this call
          [@head, ctx] = Chain(@head, cached-tails)cache o, true

          # reset counting to the now-mutated tails array
          i = 0

        # mutate the call to apply the Arr of the expanded splat arg
        call <<< {
          method: \.apply
          args: [ctx or Literal(\null), args-expr]
        }

  expandBind: !(o) ->
    {tails} = this; i = -1; while tails[++i]
      continue unless that.symbol is \.~
      that.symbol = ''
      obj   = Chain(@head, tails.splice 0 i)unwrap!
      {key} = tails.shift!
      call  = Call.make Util(\bind), [obj, key <<< {+reserved}]
      @head = if @newed then Parens call, true else call
      i = -1

  # change all stars in Indexes to containing length references
  expandStar: !(o) ->
    {tails} = this
    i = -1
    while tails[++i]
      continue if that.args or that.stars or that.key instanceof Key

      stars = []

      that.eachChild seek # fill stars array

      continue unless stars.length > 0

      [sub, ref, temps] = Chain(@head, tails.splice 0 i)unwrap!cache o

      # change all star literals to to "ref.length"
      # XXX this abuses the defaut compilation strategy of
      # Literal to just pass through the value by turning the
      # `value` to a "ref.length" string.
      #
      # Proper compilation would be to replace all the Literal \* nodes
      # with Chain(ref, [Index Key \length]) nodes, but there aren't
      # currently nice ast traversal facilities that allow replacement
      # of nodes, so we'll keep the abuse for now.
      value = Chain(ref, [Index Key \length])compile o
      for star in stars then star <<< {value, isAssignable: YES, +abused-by-star}

      # replace our head with the assignment and the this Index,
      # which is now at the first position due to the splice
      @head = Chain sub, [tails.shift!]

      o.scope.free temps.0 if temps

      # all the previous tails were spliced into the temp cache
      # so start back at the first tail
      i = -1

    !function seek
      if it.value is \*               then stars.push it
      else unless it instanceof Index then it.eachChild seek

  # `a[x, y] = b{z} = c` => `[a[x], a[y]] = {z: b.z} = c`
  expandSlice: (o, assign) ->
    {tails} = this; i = -1
    while tail = tails[++i] when tail.key?items
      tail.carp 'calling a slice' if tails[i+1] instanceof Call
      x = tails.splice 0 i+1
      x = x.pop!key.toSlice o, Chain(@head, x)unwrap!, tail.symbol, assign
      @head = x <<< {@front}
      i = -1
    this

#### Call
# `x(y)`
#
# Note that `new a()` is also parsed as a Chain Call with @new set to 'new',
# while `new a` is parsed as a Unary with the `new` operator.
#
# In JS, there is only NewExpression, so this node as well as the
# Unary LiveScript node compile to it.
#
class exports.Call extends Node
  (args || []) ~>
    if args.length is 1 and (splat = args.0) instanceof Splat
      if splat.filler
        @method = \.call
        args <<< [Literal \this; Splat Var \arguments]
      else if splat.it instanceof Arr
        args = splat.it.items
    else
      for a, i in args when a.value is \_
        args[i] = Chain Literal \void
        args[i].placeholder = true
        (@partialized ?= []).push Chain Literal i
    import {args}

  children: [\args]

  show: -> [@new] + [@method] + [\? if @soak]

  compile: (o) ->
    # in Chain compilation, it just so happens that NewExpression and
    # CallExpression compilation work out to the same JS AST "shape",
    # so no special logic is needed outside of the JS node type.

    # XXX the @method field is an abuse to make AST transformations easier,
    # so we leave it up to Chain::compile to handle that properly by wrapping
    # another MemberExpression in the JS AST.
    if @new
      $.NewExpression arguments: [arg.compile o, LEVEL_CALL for arg in @args]
    else
      $.CallExpression arguments: [arg.compile o, LEVEL_CALL for arg in @args]

  # helper method to make Call chain from other nodes
  @make = (callee, args, opts) ->
    call = Call args
    call <<< opts if opts
    Chain(callee)add call

  @block = (fun, args, method) ->
    Parens(Chain fun, [Call(args) <<< {method}]; true) <<< {+calling}

  @back = (params, node, bound, curried) ->
    fun = Fun params,, bound, curried
    node.=it if fun.hushed = node.op is \!
    if node instanceof Label
      fun <<< {name: node.label, +labeled}
      node.=it
    node.=it if not fun.hushed and fun.hushed = node.op is \!
    node.getCall!?partialized = null
    {args} = node.getCall! or (node = Chain node .add Call!)getCall!
    index = 0
    for a in args
      break if a.placeholder
      ++index
    node <<< back: (args[index] = fun)body

  @let = (args, body) ->
    params = for a, i in args
      if a.op is \= and not a.logic and a.right
        args[i] = that
        continue if i is 0 and gotThis = a.left.value is \this
        a.left
      else Var a.varName! || a.carp 'invalid "let" argument'
    gotThis or args.unshift Literal \this
    ret = @block Fun(params, body), args, \.call
    ret

#### List
# An abstract node for a list of comma-separated items.
class List extends Node
  children: [\items]

  show  : -> @name
  named : (@name) -> this

  isEmpty : -> not @items.length
  assigns : -> for node in @items then return true if node.assigns it

#### Obj
# `{x: y}`
class exports.Obj extends List
  (@items or []) ~>

  asObj: THIS

  # `base{x: y}` => `{x: base.y}`
  toSlice: (o, base, symbol, assign) ->
    {items} = this
    if items.length > 1 then [base, ref, temps] = base.cache o else ref = base
    for node, i in items
      continue if node.comment
      if node instanceof [Prop, Splat]
        node[name = node.children[*-1]] =
          chain = Chain base, [Index node[name]maybeKey!]
      else
        # `o{k or v}` => `{k: a.k or v}`
        node.=first if logic = node.getDefault!
        if node instanceof Parens
          # `a{(++i)}` => `{(ref$ = ++i): a[ref$]}`
          [key, node] = node.cache o, true
          # `a{(++i)} = b` => `{(ref$): a[ref$ = ++i]} = b`
          #                => `a[ref$ = ++i] = b[ref$]`
          [key, node] = [node, key] if assign
          key = Parens key
        else key = node
        val = chain = Chain base, [Index node.maybeKey!, symbol]
        val = logic <<< first: val if logic
        items[i] = Prop key, val
      base = ref
    chain or @carp 'empty slice'
    (chain.head = Var temps.0)temp = true if temps
    this

  compileNode: (o) ->
    {items} = this

    to-compile = []
    for node, i in items
      # if node.comment
      #  code += idt + node.compile o
      #  continue

      if node instanceof Splat or (node.key or node) instanceof Parens
        rest = items.slice i
        break
      else
        to-compile.push node

    if rest
      # compile import, making sure to force an import statement
      # rather than gluing both objects back together
      (Import(Obj(to-compile) <<< {@front}, Obj(rest), false, true) <<< {@void})
        .compile o
    else
      dic = {}

      # convert to js properties and check for duplicates
      properties = []
      for node in to-compile
        node.=first if logic = node.getDefault!

        if logic
          # `{@a or b}` => `{a: @a or b}`
          if node instanceof Prop
          then node.val = logic <<< first: node.val
          else node = Prop node, logic <<< first: node

        if @deepEq and node instanceof Prop
          if node.val instanceof Var and node.val.value is \_
          then node.val = Obj [Prop (Key \__placeholder__), Literal true]
          else if node.val instanceof [Obj, Arr] then node.val.deepEq = true

        if node instanceof Prop
          # copy identifier name to otherwise anonymous Function/Class values
          unless node.accessor
            node.val.ripName node.key
        else if node instanceof Key
          # it's a Key masquerading as an Identifier, need to expand
          # {a} => {a: a}
          node = Prop node, Var node.name
        else
          # it's a Literal, expand
          # {1} => {1: 1}
          node = Prop node, node

        # Canonicalize the key, e.g.: `0.0` => `0` for duplicate checking
        canon-key =
          if ID.test node.key
            do Function "return #{node.key}"
          else
            node.key

        if dic"#canon-key." .^.= 1 # register existence
          # XXX Props with getters and setters can potentially compile
          # to two Property JS AST nodes
          properties.push ...node.compileAccessors o
        else
          node.carp "duplicate property \"#key\""
      $.ObjectExpression {properties}

#### Prop
# `x: y`
# If this is a simple prop, @val is an AST node
# If this is a getter and or setter, @val is an array of Fun AST nodes.
class exports.Prop extends Node
  (@key, @val) ~>
    return Splat @val if key.value is \...
    if val.getAccessors!
      @val = that
      for fun in that
        fun.x = if fun.hushed = fun.params.length then \s else \g
      import {\accessor}

  children: <[ key val ]>

  show: -> @accessor

  assigns: -> @val.assigns? it

  # livescript Prop AST can compile to two JS Properties if
  # there is a getter and a setter, so we return an array
  compileAccessors: (o) ->
    if @accessor
      for fun in @val
        fun.accessor = true

        type: \Property
        key: @key.compile o
        value: fun.compile o, LEVEL_LIST
        kind: "#{fun.x}et" # get or set
    else
      [
        type: \Property
        key: @key.compile o
        value: @val.compile o
        kind: \init
      ]

  compileDescriptor: (o) ->
    with Obj!
      for fun in @val then ..items.push Prop Key(fun.x + \et  ), fun

      ..items.push Prop Key(\configurable), Literal true
      ..items.push Prop Key(\enumerable  ), Literal true

#### Arr
# `[x, y]`
class exports.Arr extends List
  (@items or []) ~>

  isArray: YES

  asObj: -> Obj([Prop Literal(i), item for item, i in @items])

  # `base[x, ...y]` => `[base[x], ...base[y]]`
  toSlice: (o, base, symbol) ->
    {items} = this
    if items.length > 1 then [base, ref] = base.cache o else ref = base
    for item, i in items
      item.=it if splat = item instanceof Splat
      continue if item.isEmpty!
      chain = Chain base, [Index item, symbol]
      items[i] = if splat then Splat chain else chain
      base = ref
    chain or @carp 'empty slice'
    this

  compile: (o) ->
    {items} = this
    # if there are splats to expand out
    if Splat.hasSplats items
      Splat.expandArray(items, false)compile o, LEVEL_LIST
    else
      $.ArrayExpression {elements: for item in items
        if @deepEq
          if item instanceof Var and item.value is \_
            item = Obj [Prop (Key \__placeholder__), Literal true]
          else if item instanceof [Obj, Arr]
            item.deepEq = true

        item.compile o, LEVEL_LIST
      }
  @maybe = (nodes) ->
    return nodes.0 if nodes.length is 1 and nodes.0 not instanceof Splat
    constructor nodes

  # turn input node into a splatted array, e.g. [...it], for
  # destructuring rendArr use which expect Arr nodes.
  @wrap = -> constructor [Splat it <<< isArray: YES]

#### Unary operators
class exports.Unary extends Node
  # `flag` denotes inversion or postcrement.
  (op, it, flag) ~>
    if it?
      if not flag and it.unaries
        that.push op
        return it
      switch op
      case \!
        break if flag
        return it <<< {+hushed} if it instanceof Fun and not it.hushed
        return it.invert!
      case \++ \-- then @post = true if flag
      case \new
        # `new C?` => `new C?()`
        if it instanceof Existence and not it.negated
          it = Chain(it)add Call!
        it.newed = true
        for node in it.tails or ''
          if node instanceof Call and not node.new
            node.args.shift! if node.method is \.call
            node <<< {\new, method: ''}
            return it
      case \~ then if it instanceof Fun and it.statement and not it.bound
        return it <<< bound: \this$
    import {op, it}

  children: [\it]

  show: -> [\@ if @post] + @op

  isCallable: -> @op in <[ do new delete ]> or not @it?

  isArray: -> @it instanceof Arr   and @it.items.length
           or @it instanceof Chain and @it.isArray!

  isString: -> @op in <[ typeof classof ]>

  invert: ->
    return @it if @op is \! and @it.op in <[ ! < > <= >= of instanceof ]>
    constructor \! this, true

  unfoldSoak: (o) ->
    @op in <[ ++ -- delete ]> and @it? and If.unfoldSoak o, this, \it

  getAccessors: ->
    return unless @op is \~
    return [@it] if @it instanceof Fun
    if @it instanceof Arr
      {items} = @it
      return items if not items.2
                   and items.0 instanceof Fun
                   and items.1 instanceof Fun

  function crement then {'++':\in '--':\de}[it] + \crement

  compileNode: (o) ->
    return @compileAsFunc o if not @it?
    return that if @compileSpread o
    {op, it} = this
    switch op
    case \!   then it.cond = true
    case \new
      it.carp 'invalid constructor' unless it.isCallable!
      return $.NewExpression callee: it.compile(o), arguments: []
    case \do
      # `do f?` => `f?()`
      if o.level is LEVEL_TOP and it instanceof Fun and it.is-statement!
        return "#{ it.compile o } #{ Unary \do Var it.name .compile o }"
      x = Parens if it instanceof Existence and not it.negated
                 then Chain(it)add Call!
                 else Call.make it
      return (x <<< {@front, @newed})compile o
    case \delete
      @carp 'invalid delete' if it instanceof Var or not it.isAssignable!
      return @compilePluck o if o.level and not @void
    case \++ \--
      it.isAssignable! or @carp 'invalid ' + crement op
      if it instanceof Var and o.scope.checkReadOnly it.value
        @carp "#{ crement op } of #that \"#{it.value}\"" ReferenceError
      it{front} = this if @post

      return $.UpdateExpression do
        operator: op
        argument: it.compile o, LEVEL_OP + PREC.unary
        prefix: not @post
    case \^^
      return
        Call.make Var(util(\clone)), [it]
          .compile o, LEVEL_LIST
    case \jsdelete
      return $.UnaryExpression operator: \delete, argument: it.compile o
    case \classof
      # toString.call(it).slice(8, -1)
      return Chain(Var util \toString)
        .add Index Key \call
        .add Call [it]
        .add Index Key \slice
        .add Call [Literal 8; Unary \- Literal 1]
        .compile o

    return $.UnaryExpression do
      operator: op
      argument: it.compile o, LEVEL_OP + PREC.unary
      prefix: not @post

  # `^delete o[p, ...q]` => `[^delete o[p], ...^delete o[q]]`
  compileSpread: (o) ->
    {it} = this; ops = [this]
    while it instanceof constructor, it.=it then ops.push it
    return '' unless it.=expandSlice(o)unwrap! instanceof Arr
                 and (them = it.items)length
    for node, i in them
      node.=it if sp = node instanceof Splat
      for op in ops by -1 then node = constructor op.op, node, op.post
      them[i] = if sp then lat = Splat node else node
    if not lat and (@void or not o.level)
      it = Block(them) <<< {@front, +void}
    it.compile o, LEVEL_PAREN

  # `v = delete o.k`
  compilePluck: (o) ->
    [get, del] = Chain @it .cacheReference o

    ref = o.scope.temporary!

    # (ref$ = it[a], delete it[a], $ref)
    Block [
      Assign Var(ref), get
      Unary \jsdelete del
      Var(o.scope.free ref)
    ] .compile o

  compileAsFunc: (o) ->
    if @op is \!
    then $.Identifier name: util \not
    else
      (Fun [], Block Unary @op, Chain Var \it).compile o

#### Binary operators
class exports.Binary extends Node
  (op, first, second, destructuring) ~>
    if destructuring
      logic = op.logic
      logic = destructuring if typeof! destructuring is \String
      op = | logic    => that
           | op is \= => \?
           | _        => \=
    @partial = not first? or not second?
    if not @partial
      if \= is op.charAt op.length-1 and op.charAt(op.length-2) not in <[ = < > ! ]>
        return Assign first.unwrap!, second, op
      switch op
      | \in        => return new In first, second
      | \with      => return new Import (Unary \^^ first), second, false
      | \<<< \<<<< => return Import first, second, op is \<<<<
      | \<|        => return Block first .pipe second, op
      | \|>        => return Block second .pipe first, \<|
      | \. \.~     => return Chain first .add Index second, op
    import {op, first, second}

  children: <[ first second ]>

  show: -> @op

  isCallable: ->
    @partial or @op in <[ && || ? << >> ]> and @first.isCallable! and @second.isCallable!

  isArray: -> switch @op | \* => @first .isArray!
                         | \/ => @second.isMatcher!

  isString: -> switch @op
    | \+ \* => @first.isString! or @second.isString!
    | \-    => @second.isMatcher!

  COMPARER   = /^(?:[!=]=|[<>])=?$/
  INVERSIONS = '===':'!==' '!==':'===' '==':'!=' '!=':'=='

  invert: ->
    if not COMPARER.test @second.op and INVERSIONS[@op]
      @op = that
      @was-inverted = true
      return this
    Unary \! Parens(this), true

  invertIt: -> @inverted = true; this

  getDefault: -> switch @op | \? \|| \&& => this

  xorChildren: (test) ->
    return false unless (first = test @first) xor test @second
    return if first then [@first, @second] else [@second, @first]

  compileNode: (o) ->
    return @compilePartial o if @partial
    switch @op
    case \? then return @compileExistence o
    case \*
      return @compileJoin   o if @second.isString!
      return @compileRepeat o if @first.isString! or @first.isArray!
    case \-       then return @compileRemove o if @second.isMatcher!
    case \/       then return @compileSplit  o if @second.isMatcher!
    case \** \^   then return @compilePow o
    case \<? \>?  then return @compileMinMax o
    case \<< \>>  then return @compileCompose o
    case \++ then return @compileConcat o
    case \%%      then return @compileMod o
    case \xor     then return @compileXor o
    case \&& \||
      @second.void = true if top = @void or not o.level
      if top or @cond
        @first .cond = true
        @second.cond = true
    case \instanceof
      {items}:rite = @second.expandSlice(o)unwrap!
      if rite instanceof Arr
        return @compileAnyInstanceOf o, items if items.1
        @second = items.0 or rite
      @second.isCallable! or @second.carp 'invalid instanceof operand'
    case <[ ==== !=== ]>       then @op.=slice 0 3; fallthrough
    case <[ <== >== <<= >>= ]> then return @compileDeepEq o
    default
      if COMPARER.test @op
        if @op in [\=== \!==] and @xorChildren (.isRegex!)
          return @compileRegexEquals o, that
        if @op is \=== and (@first instanceof Literal and @second instanceof Literal)
        and @first.isWhat! isnt @second.isWhat!
          console?.warn "WARNING: strict comparison of two different types will always be false: #{@first.value} == #{@second.value}"
      return @compileChain o if COMPARER.test @op and COMPARER.test @second.op

    @first <<< {@front}

    $.BinaryExpression do
      operator: @mapOp @op
      left : @first .compile o, LEVEL_OP + PREC[@op]
      right: @second.compile o, LEVEL_OP + PREC[@op]

  mapOp: (op) ->
    | op.match //\.([&\|\^] | << | >>>?)\.// => that.1
    | op is \of                              => \in
    | otherwise                              => op

  # Mimic Python/Perl6's chained comparisons
  # when multiple comparison operators are used sequentially:
  #
  #     $ livescript -pe '50 < 65 === 9r72 > 10'
  #     true
  #
  # See <http://docs.python.org/reference/expressions.html#notin>.
  compileChain: (o) ->
    [sub, @second.first] = @second.first.cache o, true

    level = LEVEL_OP + PREC[@op]

    # first `op` (ref$ = second) && ref$ `op` third
    $.BinaryExpression do
      operator: \&&
      left: $.BinaryExpression do
        operator: @op
        left: @first.compile o, level
        right: sub.compile o, level
      right:
        @second.compile o, LEVEL_OP

  compileExistence: (o) ->
    if @void or not o.level
      x = Binary \&& Existence(@first, true), @second
      return (x <<< {+void})compileNode o
    x = @first.cache o, true
    If(Existence x.0; x.1)addElse(@second)compileExpression o

  # `x instanceof [A, B]` => `x instanceof A || x instanceof B`
  compileAnyInstanceOf: (o, items) ->
    [sub, ref, @temps] = @first.cache o
    test = Binary \instanceof sub, items.shift!
    for item in items then test = Binary \|| test, Binary \instanceof ref, item
    Parens test .compile o

  compileMinMax: (o) ->
    lefts = @first .cache o, true
    rites = @second.cache o, true
    x = Binary @op.charAt!, lefts.0, rites.0
    If x, lefts.1 .addElse rites.1 .compileExpression o

  compileMethod: (o, klass, method, arg) ->
    args = [@second] ++ (arg || [])
    if @first"is#klass"!
      Chain(@first, [Index Key method; Call args])compile o
    else
      args.unshift @first

      # call utility method with this = @first
      (Chain Var util method
        ..add Index Key \call
        ..add Call args
      )compile o

  compileJoin   : -> @compileMethod it, \Array  \join
  compileRemove : -> @compileMethod it, \String \replace Literal "''"
  compileSplit  : -> @compileMethod it, \String \split

  compileRepeat: (o) ->

    {first: x, second: n} = this
    {items} = x.=expandSlice o .unwrap!

    arr = x.isArray! and \Array
    if items and Splat.hasSplats items
      x = Splat.expandArray items, false
      items = null

    if arr and not items
    or not (n instanceof Literal and n.value < 0x20)
      return Call.make Util(\repeat + (arr or \String)), [x, n] .compile o

    n = +n.value

    return x.compile o if 1 <= n < 2

    # `[x] * 2` => `[x, x]`
    if items
      if n < 1 then return Block items .add Arr([]) .compile o

      refs = []

      for item, i in items
        [items[i], refs.*] = item.cache o, 1x

      # mutate x's items array to add repetition after initial assigns
      for i til n-1
        items.push ...refs

      x.compile o

    # `'x' * 2` => `'xx'`
    else if x instanceof Literal
      $.Literal value: "#{x.compile(o)value}" * n

    # `"#{x}" * 2` => `(ref$ = "" + x) + ref$`
    else
      if n < 1
        return Block(x.it)add(Literal "''")compile o
      else
        [sub, ref] = x.cache o, true # once

        ident = ref.compile o

        js = sub.compile o
        # add ref n - 1 times by wrapping + BinaryExpressions
        for i til n-1
          js = $.BinaryExpression do
            operator: \+
            left: js
            right: ident

        return js

  compilePow: (o) ->
    # math.pow(first, second)
    (Chain Var \Math
      ..add Index Key \pow
      ..add Call [@first, @second]
    )compile o

  compileConcat: (o) ->
    f = (x) ->
      | x instanceof Binary and x.op is \++ =>
        (f x.first) ++ (f x.second)
      | otherwise                            => [x]
    Chain @first .add Index (Key \concat), \., true .add Call(f @second) .compile o

  compileCompose: (o) ->
    op = @op
    functions = [@first]
    x = @second
    while x instanceof Binary and x.op is op
      functions.push x.first
      x = x.second
    functions.push x

    functions.reverse! if op is \<<

    first = functions.shift!
    n = Chain first
      .add Index (Key \apply)
      .add Call [Literal 'this'; Var 'arguments']

    for f in functions
      n = Chain f .add Call [n]

    Fun [], Block [n] .compile o

  compileMod: (o) ->
    # (first % (ref$ = second) + ref$) % ref$
    ref = o.scope.temporary!

    ident = $.Identifier name: ref

    js = $.BinaryExpression do
      operator: \%
      left: $.BinaryExpression do
        operator: \+
        left: $.BinaryExpression do
          operator: \%
          left: @first.compile o
          right: $.AssignmentExpression do
            operator: \=
            left: $.Identifier name: ref
            right: ident
        right: ident
      right: ident

    o.scope.free ref

    js

  compilePartial: (o) ->
    vit = Var \it
    switch
    case  not @first? and not @second?
      x = Var \x$; y = Var \y$
      (Fun [x, y], Block((Binary @op, x, y).invertCheck this), false, true).compile o
    case @first?
      (Fun [vit], Block((Binary @op, @first, vit) .invertCheck this)).compile o
    default
      (Fun [vit], Block((Binary @op, vit, @second).invertCheck this)).compile o

  compileRegexEquals: (o, [regex, target]) ->
    if @op is \===
      method = if @was-inverted then \test else \exec
      Chain regex .add Index Key method .add Call [target] .compile o
    else
      Unary \! (Chain regex .add Index Key \test .add Call [target]) .compile o

  compileDeepEq: (o) ->
    if @op in <[ >== >>= ]>
      [@first, @second] = [@second, @first]
      @op = if @op is \>== then \<== else \<<=
    if @op is \!==
      @op = \===
      negate = true
    for x in [@first, @second]
      x.deepEq = true if x instanceof [Obj, Arr]
    r = Chain Var (util \deepEq) .add Call [@first, @second, Literal "'#{@op}'"]
    (if negate then Unary \! r else r).compile o

  compileXor: (o) ->
    left  = Chain @first  .cacheReference o
    right = Chain @second .cacheReference o
    Binary \&& (Binary \!== (Unary \! left.0), (Unary \! right.0))
             , (Parens Binary \|| left.1, right.1) .compile o

#### Assign
# Assignment to a variable/property.
class exports.Assign extends Node
  (@left, rite, @op or \=, @logic or @op.logic, @defParam) ~>
    @op += ''
    @[if rite instanceof Node then \right else \unaries] = rite

  children: <[ left right ]>

  show: -> [,]concat(@unaries)reverse!join(' ') + [@logic] + @op

  assigns: -> @left.assigns it

  ::delegate <[ isCallable isRegex ]> -> @op in <[ = := ]> and @right[it]!

  isArray: -> switch @op
    | \= \:= => @right.isArray!
    | \/=    => @right.isMatcher!

  isString: -> switch @op
    | \= \:= \+= \*= => @right.isString!
    | \-=            => @right.isMatcher!

  unfoldSoak: (o) ->
    if @left instanceof Existence
      # `[a, b]? = c` => `[a, b] = c if c?`
      if delete (@left.=it)name
      then rite = @right; rite = Assign @right = Var(that), rite
      else [rite, @right, temps] = @right.cache o
      return If(Existence rite; this) <<< {temps, @cond, @void}
    If.unfoldSoak o, this, \left

  unfoldAssign: -> @access and this

  compileNode: (o) ->
    return @compileSplice o if @left instanceof Slice and @op is \=
    left = @left.expandSlice(o, true)unwrap!
    unless @right
      left.isAssignable! or left.carp 'invalid unary assign'
      [left, @right] = Chain left .cacheReference o
      if @unaries?
        for op in @unaries then @right = Unary op, @right
    return (Parens(@right) <<< {@front, @newed})compile o if left.isEmpty!
    if left.getDefault!
      @right = Binary left.op, @right, left.second
      left.=first
    return @compileDestructuring o, left if left.items
    left.isAssignable! or left.carp 'invalid assign'
    return @compileConditional   o, left if @logic
    {op, right} = this
    return @compileMinMax  o, left, right if op in <[ <?= >?= ]>
    if op in <[ **= ^= %%= ++= |>= ]>
    or op is \*= and right.isString!
    or op in <[ -= /= ]> and right.isMatcher!
      [left, reft] = Chain(left)cacheReference o
      right = Binary op.slice(0 -1), reft, right
      op    = \:=
    op = (op.slice 1 -2) + \= if op in <[ .&.= .|.= .^.= .<<.= .>>.= .>>>.= ]>
    (right.=unparen!)ripName left.=unwrap!
    sign = op.replace \: ''

    if lvar = left instanceof Var
      if op is \=
        o.scope.declare left.value, left,
          (@const or not @defParam and o.const and \$ isnt left.value.slice -1)
      else if o.scope.checkReadOnly left.value
        left.carp "assignment to #that \"#{left.value}\"" ReferenceError

    # if we're assigning a function to an Index chain
    # the `super` reference to the appropriate superclass method/static method
    if left instanceof Chain and right instanceof Fun
      # find left-side of .prototype
      proto-chain = Chain left.head
      found-proto = false
      for tail in left.tails
        # all tails are Indices, not Calls
        if tail.key instanceof Key and tail.key.name is \prototype
          found-proto = true
          break
        else
          proto-chain.add tail

      if found-proto
        # thing.prototype.method = -> super
        # =>
        # thing.prototype.method = -> thing.superclass.prototype.method
        right.in-class = proto-chain
      else
        # thing.static-method = -> super
        # =>
        # thing.static-method = -> thing.superclass.static-method
        #
        # proto-chain is basically a copy of left, so just remove
        # the last index
        proto-chain.tails.pop!
        right.in-class-static = proto-chain

    if o.level is LEVEL_TOP and right instanceof While and not right.else and
      (left instanceof Var or left instanceof Chain and left.isSimpleAccess!)

      # Optimize `a = while ...` by not compiling a closure and instead
      # doing statements
      # res$ = {} or []
      # while ...
      #   res$.push ...
      # left = res$
      res = o.scope.temporary \res
      Block [
        Assign Var(res), if right.objComp then Obj! else Arr!
        right.makeReturn(res)
        Assign left, Var(o.scope.free res)
      ] .compile o
    else
      $.AssignmentExpression do
        operator: sign
        left: left.compile o
        right: right.compile o, LEVEL_LIST

  compileConditional: (o, left) ->
    if left instanceof Var and @logic in <[ ? ]> and @op is \=
      o.scope.declare left.value, left
    lefts = Chain(left)cacheReference o
    # Deal with `a && b ||= c`.
    o.level += LEVEL_OP < o.level
    morph = Binary @logic, lefts.0, @<<<{-logic, left: lefts.1}
    (morph <<< {@void})compileNode o

  compileMinMax: (o, left, right) ->
    lefts = Chain(left)cacheReference o
    rites = right.cache o, true
    test  = Binary @op.replace(\? ''), lefts.0, rites.0
    put   = Assign lefts.1, rites.1, \:=
    # `a <?= b` => `a <= b || a = b `
    return Parens(Binary \|| test, put)compile o if @void or not o.level
    # `r = a <?= b` => `r = if a <= b then a else a = b`
    [test.first, left] = test.first.cache o, true
    If test, left .addElse put .compileExpression o

  # Implementation of recursive destructuring,
  # when assigning to an array or object literal.
  # See <http://wiki.ecmascript.org/doku.php?id=harmony:destructuring>.
  compileDestructuring: (o, {{length: len}:items}:left) ->
    ret  = o.level and not @void

    rite = @right
    r-compile-level = if len is 1 then LEVEL_CALL else LEVEL_LIST

    if left.value
      # reuse the existing Var
      cache = Assign do
        Var that
        rite
      o.scope.declare left.value, left
      rite = Var left.value
    else if (ret or len > 1)
      is-literal = rite instanceof Literal
      is-var = rite instanceof Var
      assigned-by-left = is-var and left.assigns rite.value
      if not is-literal and not is-var or assigned-by-left
        # can't use right, so assign temporary to it
        rref = o.scope.temporary!
        cache = Assign do
          Var rref
          rite
        rite  = Var rref

    # rendArr or rendObj
    list = for @"rend#{ left.constructor.displayName }" o, items, rite
      ..compile o, LEVEL_LIST

    o.scope.free rref  if rref
    list.unshift cache.compile o, LEVEL_LIST if cache

    if ret or not list.length
      list.push rite.compile o, r-compile-level

    $.SequenceExpression expressions: list

  compileSplice: (o) ->
    [from-exp-node, from-exp] = Chain @left.from .cacheReference o
    [right-node, right]       = Chain @right     .cacheReference o
    to-exp = Binary \- @left.to, from-exp
    Block [Chain Var (util \splice) .add Index (Key \apply), \. true
        .add Call [@left.target, (Chain Arr [from-exp-node, to-exp]
                        .add Index (Key \concat), \. true .add Call [right-node])]; right]
      .compile o, LEVEL_LIST

  rendArr: (o, nodes, rite) ->
    for node, i in nodes
      continue if node.isEmpty!
      if node instanceof Splat
        len and node.carp 'multiple splat in an assignment'

        skip = (node.=it)isEmpty!

        len = nodes.length

        if i+1 is len
          break if skip

          args = [rite]
          args.push Literal i if i > 0

          # slice$.call(rite, i)
          slice = Chain Var util \slice
            ..add Index Key \call
            ..add Call args

          val = Arr.wrap slice
        else
          # rite.length - (len - i - 1)"
          val = ivar =
            Binary \-,
              Chain rite .add Index Key \length
              Literal "#{len - i - 1}"

          # Optimize `[..., a] = b`.
          if skip and i+2 is len
            # note that ivar and val are still set
            # so this changes behavior of the non-splat case
            continue

          start = i+1;

          ivar = Var o.scope.temporary \i

          @temps = [ivar.value]

          val = if skip
            node = ivar
            val
          else
            # splat section of array
            # i < (ivar = val)
            # ? slice$.call(rite, i, ivar)
            # : (ivar = i, [])"
            f = If Binary(\<, Literal(i), Assign(ivar, val))
            # XXX to match old compilation, force compact
            # ternary expression
            f.compact = true
            f.then =
                Chain Var util \slice
                  .add Index Key \call
                  .add Call [rite, Literal(i), ivar]
            f.else =
              Block [
                Assign ivar, Literal i
                Arr []
              ]
            Arr.wrap f
      else
        idx =
          if ivar?
            # if there was a splat that set ivar to cache
            # its location
            if start < i
              # [ivar + (i - start)]
              Binary \+,
                ivar
                Literal "#{i - start}"
            else
              ivar
          else
            Literal i

        val = Chain rite, [Index idx]

      if node instanceof Assign
        node = Binary node.op, node.left, node.right, (node.logic or true)

      this with {left: node, right: val, +void}

  rendObj: (o, nodes, rite) ->
    for node in nodes
      node.=it if splat = node instanceof Splat

      # `{a or b} = c` => `a = c.a or b`
      logic = node.getDefault!
      if logic
        node.=first

      if node instanceof Parens
        [node, key] = Chain(node.it)cacheReference o
      else if node instanceof Prop
        node = ({key} = node)val
      else
        key  = node

      node = Var node.name if node instanceof Key
      node = logic <<< first: node if logic
      # XXX rite needs parenthesis for cases that the legacy
      # mode of wrapping raw js in a 'Var' node seemed to paper over
      unless rite instanceof Var
        rite = Parens rite, true # force parens
      val  = Chain rite, [Index key.maybeKey!]
      val  = Import Obj!, val if splat
      this with {left: node, right: val, +void}

#### Import
# Copies properties from right to left.
class exports.Import extends Node
  (@left, @right, @all and \All, force-import = false) ~>
    if not all and left instanceof Obj and right.items
      unless force-import
        return Obj left.items ++ right.asObj!items

  children: <[ left right ]>

  show: -> @all

  ::delegate <[ isCallable isArray ]> -> @left[it]!

  unfoldSoak: (o) ->
    {left} = this
    if left instanceof Existence and not left.negated
      if left.=it instanceof Var
        {value} = @left = left
        unless o.scope.check value, true
          # typeof value != 'undefined' && value
          left = Binary \&&,
            Binary \!=,
              Unary \typeof left
              Literal "'undefined'"
            left
      else
        [left, @left, temps] = left.cache o
      return If(left, this) <<< {temps, +soak, @cond, @void}
    If.unfoldSoak o, this, \left
    or (@void or not o.level) and
    If.unfoldSoak o, this, \right

  compileNode: (o) ->
    {right} = this
    unless @all
      if right instanceof Chain
        right = right.unfoldSoak   o
             or right.unfoldAssign o
             or right.expandSlice  o .unwrap!
      if right instanceof List
        return @compileAssign o, right.asObj!items

    Call.make Util("import#{ @all or '' }"), [@left, right] .compileNode o

  # If the right operand of `<<<` is an object or array literal,
  # expand it to a series of assignments.
  compileAssign: (o, items) ->
    return @left.compile o unless items.length

    top = o.level is LEVEL_TOP
    if items.length < 2 and (top or @void or items.0 instanceof Splat)
      reft = @left
      reft = Parens reft if reft.isComplex!
    else
      [left, reft, @temps] = @left.cache o

    body = []

    if @temps
      body.push left

    for node, i in items
      if node.comment
        body.push node
        continue
      if node instanceof Splat
        body.push Import(reft, node.it)
        continue

      logic = node.getDefault!

      node.=first if logic

      if node instanceof Prop and node.accessor
        {key, val} = node

        if node.accessor
          key = Literal "'#{key.name}'" if key instanceof Key
          # Object.defineProperty(reft, key, descriptor)
          body.push do
            Chain Var \Object
              .add Index Key \defineProperty
              .add Call [
                reft
                key
                node.compileDescriptor o
              ]
      else
        if dyna = node instanceof Parens
          [key, val] = node.it.cache o, true
        else if node instanceof Prop
          {key, val} = node
        else
          key = val = node

        dyna  or  key.=maybeKey!
        logic and val = logic <<< first: val

        try
          body.push Assign(Chain reft, [Index key]; val)
        catch
          throw new Error "here #e"

    # if we're returning, append left reference
    if not @void and node not instanceof Splat
      body.push reft

    Block body .compile o

#### In
# Handles `in` operation that tests if the left operand is included within
# the right operand, arraywise.
class exports.In extends Node implements Negatable
  (@item, @array) ->

  children: <[ item array ]>

  compileNode: (o) ->
    {items} = array = @array.expandSlice(o)unwrap!

    # if not literal array or there's only one item
    if array not instanceof Arr or items.length < 2
      # in$(item, array)
      js = $.CallExpression do
        callee: $.Identifier name: util \in
        arguments:
          @item.compile o, LEVEL_LIST
          array.compile o, LEVEL_LIST

      if @negated # prepend '!'
        js = $.UnaryExpression operator: \!, prefix: true, argument: js

      return js
    else
      # a in [1 2 3]
      # =>
      # a === 1 || a === 2 || a === 3;
      #
      # or
      #
      # a not in [1 2 3]
      # =>
      # a !== 1 && a !== 2 && a !== 3;
      #
      # or
      #
      # a in [1, ...b]
      # =>
      # a === 1 || a in b

      [cmp, cnj] = if @negated then [' !== ' ' && '] else [' === ' ' || ']

      to-compile = items.slice!
      first-test = to-compile.shift!

      if @item.isComplex!
        [sub, ref] = @item.cache o, false, LEVEL_PAREN

        item-ref = Var ref

        # start node with assignment of ref
        # (ref$ = complex) === 1 || ref$ === 2 ...
        js =
          if first-test instanceof Splat
            $.SequenceExpression expressions:
              * sub
              * (new In(item-ref; first-test.it) <<< {@negated})compile o
          else
            $.BinaryExpression do
              operator: cmp
              left: sub
              right: first-test.compile o
      else
        # simple, use actual item
        item-ref = @item

        js =
          if first-test instanceof Splat
            # compile utility function against inside of splat
            (new In(item-ref; first-test.it) <<< {@negated})compile o
          else
            $.BinaryExpression do
              operator: cmp
              left: item-ref.compile o
              right: first-test.compile o

      # successively chain other comparisons
      while (test = to-compile.shift!)
        js = $.BinaryExpression do
          operator: cnj
          left: js # previous comparison
          right:
            if test instanceof Splat
              # compile utility function against inside of splat
              (new In(item-ref; test.it) <<< {@negated})compile o, LEVEL_TOP
            else
              $.BinaryExpression do
                operator: cmp
                left: item-ref.compile o
                right: test.compile o, LEVEL_OP

      o.scope.free ref if ref?

      return js

#### Existence
# Checks a value for existence--not `undefined` nor `null`.
class exports.Existence extends Node implements Negatable
  (@it, @negated) ~>

  children: [\it]

  compileNode: (o) ->
    node = @it.unwrap! <<< {@front}

    js-node = node.compile o, LEVEL_OP + PREC\==

    if node instanceof Var and not o.scope.check node.value, true
      [op, eq] = if @negated then <[ || = ]> else <[ && ! ]>

      # typeof it != 'undefined' && it !== null
      # or
      # typeof it == 'undefined' || it === null
      $.BinaryExpression do
        operator: op
        left: $.BinaryExpression do
          operator: eq + \=
          left: $.UnaryExpression do
            operator: \typeof
            argument: js-node
          right: $.Literal value: 'undefined'
        right: $.BinaryExpression do
          operator: eq + \==
          left: js-node
          right: $.Literal value: null
    else
      # just compile to weak-equals null
      $.BinaryExpression do
        operator: if @negated then \== else \!=
        left: js-node
        right: $.Literal value: null

#### Fun
# A function definition. This is the only node that creates a `new Scope`.
class exports.Fun extends Node
  (@params or [], @body or Block!, @bound and \this$, @curried or false, @hushed = false) ~>

  children: <[ params body ]>

  show: -> [@name] + ["~#that" if @bound]

  named: -> import {name: it, +statement}

  isCallable: YES

  isStatement: -> !!@statement

  # Short-circuit `traverseChildren` method to prevent it
  # from crossing scope boundaries by default.
  traverseChildren: (, xscope) -> super ... if xscope

  makeReturn: -> if @statement then import {+returns} else super ...

  ripName: !-> @name ||= it.varName!

  compileNode: (o) ->
    pscope = o.scope

    sscope = pscope.shared or pscope

    scope  = o.scope = @body.scope =
      new Scope (if @wrapper then pscope else sscope), @wrapper && sscope

    scope.fun = this

    if @proto
      scope.assign \prototype do
        Chain @proto .add Index Key \prototype
    if @cname
      scope.assign \constructor Var @cname

    inLoop = delete o.loop

    {body, name} = this

    # we don't know the context yet, so we can't make a
    # FunctionDeclaration or FunctionExpression. once we know, we'll
    # import this object into the actual node
    js-fn = {}

    if @bound is \this$
      if @ctor
        # var this$ = this instanceof ctor$ ? this : new ctor$
        scope.assign-js \this$, $.ConditionalExpression do
          test: $.BinaryExpression do
            operator: \instanceof
            left: $.ThisExpression!
            right: $.Identifier name: \ctor$
          consequent: $.ThisExpression!
          alternate: $.NewExpression do
            callee: $.Identifier name: \ctor$
            arguments: []

        body.lines.push Return Var \this$
      else if sscope.fun?bound
      then @bound = that
      else sscope.assign \this$ Literal \this

    if @statement
      name                    or @carp  'nameless function declaration'
      pscope is o.block.scope or @carp 'misplaced function declaration'
      @accessor              and @carp 'named accessor'
      pscope.add name, \function, this

    if @statement or name and @labeled
      scope.add name, \function, this
      js-fn.id = $.Identifier {name}

    body.makeReturn! unless @hushed or @ctor or @newed

    js-fn.params = @compileParams o, scope
    js-fn.body = $.BlockStatement body: body.compileWithDeclarations o

    if @curried and @has-splats
        @carp 'cannot curry a function with a variable number of arguments'

    # if curried, wrap function expression in call
    curry-code-check = (force-statement) ~>
      if @curried and @params.length > 1 and not @class-bound
        # wrap in curry$(function () {...}, bound)
        js = $.FunctionExpression js-fn
        $.CallExpression do
          callee: $.Identifier name: util \curry
          arguments:
            if @bound then [js, $.Literal value: true] else [js]
      else
        if force-statement or @statement
          $.FunctionDeclaration js-fn
        else
          $.FunctionExpression js-fn

    if inLoop
      # initialize function as FunctionDeclarator
      # and use the temp variable in its place
      # force statement so we can move the fn to the end of the scope
      # XXX relies on Scope#add-scope's behavior with
      # utility functions, should refactor this to make it more clear
      return $.Identifier do
        name: pscope.assign-js pscope.temporary(\fn), curry-code-check true

    if @returns
      # we're in a statement context so wrap in $.BlockStatement so
      # we can add the extra return
      $.BlockStatement body:
        * $.FunctionDeclaration js-fn
        * $.ReturnStatement argument: $.Identifier {name}
    else if @bound and @ctor
      # also in statement context, so add bound ctor definition
      #
      # function ctor$(){} ctor$.prototype = prototype;
      $.BlockStatement body:
        * $.FunctionDeclaration js-fn
        * $.FunctionDeclaration do
            id: $.Identifier name: \ctor$
            params: []
            body: $.BlockStatement body: []
        * $.ExpressionStatement expression: $.AssignmentExpression do
            operator: \=
            left: $.MemberExpression do
              computed: false
              object: $.Identifier name: \ctor$
              property: $.Identifier name: \prototype
            right:
              $.Identifier name: \prototype
    else
      curry-code-check!

  compileParams: (o, scope) ->
    {{length}:params, body} = this

    # Remove trailing placeholders.
    for p in params by -1
      break unless p.isEmpty! or p.filler
      --params.length
    for p, i in params
      if p instanceof Splat
        @has-splats = true
        splace = i
      # `(a = x) ->` => `(a ? x) ->`
      else if p.op is \=
        params[i] = Binary (p.logic or \?), p.left, p.right

    # `(a, ...b, c) ->` => `(a) -> [[] ...b, c] = @@`
    if splace?
      rest = params.splice splace, 9e9
    else if @accessor
      that.carp 'excess accessor parameter' if params.1
    else unless length or @wrapper
      params.0 = Var \it if body.traverseChildren -> it.value is \it or null
    names   = []
    assigns = []
    if params.length
      dic = {}
      for p in params
        vr = p
        vr.=first if df = vr.getDefault!
        if vr.isEmpty!
          vr = Var scope.temporary \arg
        else if vr.value is \..
          vr = Var o.ref = scope.temporary!
        else if vr not instanceof Var
          unaries = []
          while vr instanceof Unary
            has-unary = true
            unaries.push vr
            vr.=it
          v = Var delete (vr.it || vr)name || vr.varName! ||
                  scope.temporary \arg
          assigns.push Assign vr, switch
            | df        => Binary p.op, v, p.second
            | has-unary => fold ((x, y) -> y.it = x; y), v, unaries.reverse!
            | otherwise => v
          vr = v
        else if df
          assigns.push Assign vr, p.second, \=, p.op, true

        names.push scope.add vr.value, \arg, p

    if rest
      while splace-- then rest.unshift Arr!
      assigns.push Assign Arr(rest), Var \arguments

    @body.prepend ...assigns if assigns.length

    return [$.Identifier {name} for name in names]

#### Class
class exports.Class extends Node
  ({@title, @sup, @mixins, body}) -> @fun = Fun [] body

  children: <[ title sup mixins fun ]>

  isCallable: YES

  ripName: !-> @name = it.varName!

  compile: (o, level) ->
    {{{lines}:body}:fun, title} = this
    bound-funcs = []
    curried-bound-funcs = []
    decl = title?varName!
    name = decl or @name
    if ID.test name || '' then fun.cname = name else name = \constructor
    proto = Var \prototype
    const ctor-name = \constructor$$
    var ctor, ctor-place
    import-proto-obj = (node, i) ->
      j = 0
      while j < node.items.length, j++
        prop = node.items[j]
        key = prop.key
        if (key instanceof Key and key.name is ctor-name)
        or (key instanceof Literal and key.value is "'#ctor-name'")
          node.carp 'redundant constructor' if ctor
          ctor := prop.val
          node.items.splice j--, 1
          ctor-place := i
        continue unless prop.val instanceof Fun or prop.accessor
        if key.isComplex!
          key = Var o.scope.temporary \key
          prop.key = Assign key, prop.key
        if prop.val.bound
          if prop.val.curried
            curried-bound-funcs.push prop.key
          else
            bound-funcs.push prop.key
          prop.val.bound = false
          # need to know whether bound param of curry$ should be true
          prop.val.class-bound = true
        for v in [] ++ prop.val
          v.meth = key
      if node.items.length then Import proto, node else Literal 'void'

    for node, i in lines
      if node instanceof Obj
        lines[i] = import-proto-obj node, i
      else if node instanceof Fun and not node.statement
        ctor and node.carp 'redundant constructor'
        ctor = node
      else if node instanceof Assign and node.left instanceof Chain
      and node.left.head.value is \this and node.right instanceof Fun
        node.right.stat = node.left.tails.0.key
      else
        node.traverseChildren !->
          if it instanceof Block
            for child, k in it.lines when child instanceof Obj
              it.lines[k] = import-proto-obj child, i

    ctor ||= lines.* = if @sup
                    then  Fun [] Block Chain(new Super).add Call [Splat Var \arguments]
                    else Fun!
    unless ctor instanceof Fun
      lines.splice ctor-place + 1, 0, Assign (Var ctor-name), ctor
      lines.unshift ctor = Fun [] Block Return Chain(Var ctor-name).add Call [Splat \arguments true]
    ctor <<< {name, +ctor, +statement}
    for f in bound-funcs
      ctor.body.lines.unshift do
        Assign (Chain Literal \this .add Index f),
               (Chain Var (util \bind)
                 .add Call [Literal \this; Literal "'#{f.name}'"; Var \prototype])

    for f in curried-bound-funcs
      ctor.body.lines.unshift do
        Assign (Chain Literal \this .add Index Key "_#{f.name}"),
               (Chain Var (util \curry)
                 .add Call [Chain Var \prototype .add Index f; Var \true])
        Assign (Chain Literal \this .add Index f),
               (Chain Var (util \bind)
                 .add Call [Literal \this; Literal "'_#{f.name}'"])

    lines.push vname = fun.proto = Var fun.bound = name
    args = []
    if @sup
      args.push that
      imports = Chain Import (Literal \this), Var \superclass
      fun.proto = Util.Extends (if fun.cname
        then Block [Assign (imports.add Index Key \displayName), Literal "'#name'"
                   ; Var name]
        else imports)
        , fun.params.* = Var \superclass
    if @mixins
      imports = for args.* in that
        Import do
          proto
          # arguments[(args.length - 1)]
          Chain(Var(\arguments))add Index Literal (args.length - 1)
          true
      body.prepend ...imports
    if fun.cname and not @sup
      # set displayName = 'name'
      body.prepend Assign do
        Chain(Var name)add Index Key \displayName
        Literal "'#name'"
    clas = Parens Call.make(fun, args), true
    clas = Assign vname, clas if decl and title.isComplex!
    clas = Assign title, clas if title
    clas.compile o, level

#### Super
# Reference to the parent method or constructor.
class exports.Super extends Node
  ->

  isCallable: YES

  compile: ({scope}:o) ->
    if @sproto
      $.Identifier name: \superclass
    else
      while not scope.get \superclass and scope.fun, scope.=parent
        result = that

        if result.meth
          # superclass.prototype[that]
          return Chain Var \superclass
            .add Index Key \prototype
            .add Index that
            .compile o
        else if result.stat
          # superclass[that]
          return Chain Var \superclass
            .add Index that
            .compile o
        else if scope.fun.in-class
          # that.superclass.prototype.<name>
          return Chain that
            .add Index Key \superclass
            .add Index Key \prototype
            .add Index Key scope.fun.name
            .compile o
        else if scope.fun.in-class-static
          # that.superclass.<name>
          return Chain that
            .add Index Key \superclass
            .add Index Key scope.fun.name
            .compile o

      if o.scope.fun?name
        # that.superclass
        Chain Var that
          .add Index Key \superclass
          .compile o
      else
        # couldn't find anything, just emit `superclass`
        $.Identifier name: \superclass

#### Parens
# An extra set of parentheses,
# specifying evaluation order and/or forcing expression.
class exports.Parens extends Node
  (@it, @keep, @string) ~>

  children: [\it]

  show: -> @string and '""'

  ::delegate <[ isComplex isCallable isArray isRegex ]> -> @it[it]!

  isString: -> @string or @it.isString!

  unparen: -> if @keep then this else @it.unparen!

  compile: (o, level ? o.level) ->
    {it} = this
    it{cond, \void} ||= this
    it.head.hushed = true if @calling and (not level or @void)
    unless @keep or @newed or level >= LEVEL_OP + PREC[it.op]
      return (it <<< {@front})compile o, level || LEVEL_PAREN
    if it.isStatement!
      it.compileClosure o
    else
      # since we're working with JS AST nodes directly, we
      # don't need to clarify order of operations, so this is just
      # a passthrough compilation
      it.compile o, LEVEL_PAREN

#### Splat
# A splat, either as an argument to a call
# or as part of a destructuring assignment.
class exports.Splat extends Node
  (@it, @filler) ~>

  ::{children, isComplex} = Parens::

  isAssignable: YES

  assigns: -> @it.assigns it

  # if the splat hasn't already been expanded by another
  # node's compilation, than it's not in the right place
  compile: -> @carp 'invalid splat'

  @hasSplats = (list) ->
    expand list # XXX this mutates the caller's list, hope they don't mind...
    for node in list
      return true if node instanceof Splat
    return false

  # expands a list of nodes mixed with splats to an expression node.
  @expandArray = (list, apply) ->
    expand list
    index = 0
    for node in list
      break if node instanceof Splat
      ++index

    if index >= list.length
      throw new Error "compiler bug: expandArray called on list without splats!"

    unless list.length >= 2
      # the first and only element of the array is the splat
      # so simply unwrap it
      #
      # Function::apply can take array-like objects, but other
      # uses require 'slice' to turn array-likes to actual arrays
      if apply
        return list.0.it
      else # we need an array from array-likes (arguments)
        return ensureArray list.0.it

    # we have multiple items, at least one of which is a splat
    # We turn compile the array as groups of non-splats (atoms)
    # and the internal splats, then concat them with either the
    # non-splats before the first splat or the first arg (also all
    # non-splats)
    #
    # e.g.
    #
    # [1, 2, ...a, 3, 4, ...b, 5] => [1 2].concat(a.slice(), [3, 4], b.slice(), [5])
    args = []; atoms = []

    # for all nodes after the first splat
    for node in list.splice index, 9e9
      if node instanceof Splat
        args.push Arr atoms.splice 0, 9e9 if atoms.length
        args.push ensureArray node.it
      else atoms.push node

    args.push Arr atoms if atoms.length # push last batch of atoms

    head =
      if index > 0 # there are still things remaining in 'list'
        # use the mutated list variable
        # e.g. [1, ...a] = [1].concat(a)
        Arr list
      else
        # use the first item in the args
        # e.g. [...a, ...b] = a.concat(b)
        args.shift!

    Chain head
      ..add Index Key \concat
      ..add Call args

  # flattens the nodes array's splats of array literals
  # e.g. [...[1, 2], 3] -> [1, 2, 3]
  # deep flatten, so splats within splats are flattened
  function expand nodes
    index = -1
    while node = nodes[++index] then if node instanceof Splat
      {it} = node
      if it.isEmpty!
        nodes.splice index-- 1
      else if it instanceof Arr
        nodes.splice index, 1, ...expand it.items
        index += it.items.length - 1
    nodes

  function ensureArray node
    return node if node.isArray!
    Chain Var util \slice
      ..add Index Key \call
      ..add Call [node]

#### Jump
# `break` `continue`
class exports.Jump extends Node
  (@verb, @label) ~>

  show: -> (@verb or '') + if @label then ' ' + that else ''

  isStatement : YES
  makeReturn  : THIS

  getJump: (ctx or {}) ->
    return this unless ctx[@verb]
    return that not in (ctx.labels ?= []) and this if @label

  compileNode: (o) ->
    if @label and not @label in (o.labels ?= [])
      @carp "unknown label \"#that\""
    else if not o[@verb]
      @carp "stray #{@verb}"

    if @verb is \break
      $.BreakStatement {@label}
    else
      $.ContinueStatement {@label}

  @extended = !(sub) ->
    sub::children = [\it]
    @[sub.displayName.toLowerCase!] = sub

#### Throw
class exports.Throw extends Jump
  (@it) ~>

  getJump: VOID

  compileNode: (o) ->
    $.ThrowStatement argument:
      @it?compile(o, LEVEL_PAREN) || $.Literal value: null

#### YadaYadaYada
# `...` indicates unimplementedness.
class exports.YadaYadaYada extends Throw
  ~>
  compileNode: (o) ->
    # throw Error('unimplemented');
    $.ThrowStatement argument: $.CallExpression do
      callee: $.Identifier \Error
      arguments: [$.Literal value: \unimplemented]

#### Return
class exports.Return extends Jump
  ~> if it and it.value is not \void then import {it}

  getJump: THIS

  compileNode: (o) ->
    $.ReturnStatement argument: @it?compile o, LEVEL_PAREN

#### While
# The traditional `while`/`for`/`do` loop.
# Returns an array of values collected from the last expression when requested.
class exports.While extends Node
  (test, @un, mode) ->
    mode and if mode instanceof Node then @update = mode else @post = true

    # `while true` `until false` => `for (;;)`
    if @post or test.value is not ''+!un then import {test}

  children: <[ test body update else ]>

  aSource: \test, aTargets: <[ body update ]>

  show: -> [\! if @un; \do if @post] * ''

  ::isStatement = ::isArray = YES

  makeComprehension: (toAdd, loops) ->
    @is-comprehension = true
    while loops.length
      toAdd = loops.pop!addBody Block toAdd
      toAdd <<< {+in-comprehension} if not toAdd.is-comprehension
    @addBody Block toAdd

  getJump: (ctx or {}) ->
    ctx <<< {+\continue, +\break}
    for node in @body?.lines or [] then return node if node.getJump ctx

  addBody: (@body) ->
    @body = Block If @guard, @body if @guard
    [top] = @body.lines
    @body.lines.length = 0 if top?verb is \continue and not top.label
    this

  addGuard:   (@guard)          -> this
  addObjComp: (@objComp = true) -> this

  makeReturn: ->
    return this if @has-returned
    if it
      if @objComp
        @body = Block @body.makeObjReturn it
        @body = If @guard, @body if @guard
      else
        unless @body or @index
          @addBody Block Var @index = \ridx$
        last = @body.lines?[*-1]
        if (@is-comprehension or @in-comprehension) and not last?is-comprehension
          @body.makeReturn it
          @else?makeReturn it
          @has-returned = true
        else
          @res-var = it
          @else?makeReturn it
    else
      @getJump! or @returns = true
    this

  compileNode: (o) ->
    o.loop = true

    if @else
      @yet = o.scope.temporary \yet

    js =
      if @test and not (@update or @else)
        if @post
          $.DoWhileStatement test: @test.compile o, LEVEL_PAREN
        else
          $.WhileStatement   test: @test.compile o, LEVEL_PAREN
      else
        if @test
          $.ForStatement do
            # yet = true in initialization
            init: if @else
              $.AssignmentExpression do
                operator: \=
                left: $.Identifier name: @yet
                right: $.Literal value: true
            test: @test.compile o, LEVEL_PAREN
            update: if @update then that.compile o, LEVEL_PAREN
        else
          # compile to for(;;) due to logic in constructor that removes
          # @test if it's (while true) or (until false)
          $.ForStatement do
            init: if @else
              $.AssignmentExpression do
                operator: \=
                left: $.Identifier name: @yet
                right: $.Literal value: true
            test: null
            update: null

    @compileBody o, js

  # js is containing DoWhile/While/ForStatement
  # which has its `body` set and is possibly wrapped
  # in other statements to add behavior
  compileBody: (o, js) ->
    o.break = o.continue = true

    {body: {lines}, yet} = this

    mid = void
    ret = void

    empty = if @objComp then Obj! else Arr!

    last = lines?[*-1]

    unless (@is-comprehension or @in-comprehension) and not last?is-comprehension
      var has-loop
      last?traverseChildren !->
        if it instanceof Block and it.lines[*-1] instanceof While
          has-loop := true

      if @returns and not @res-var
        @res-var = res = Var o.scope.assign \results$ empty

      if @res-var and (last instanceof While or has-loop)
        temp = o.scope.temporary \lresult

        lines.unshift Assign (Var temp), Arr!, \=

        lines[*-1]?=makeReturn temp

        mid = do
          Chain Var @res-var
            .add Index (Key \push), \., true
            .add Call [Chain Var temp]
      else
        @has-returned = true
        if @res-var
          @body.makeReturn @res-var.value

    if @returns
      @body = Block @body.makeObjReturn \results$ if @objComp
      @body = If @guard, @body if @guard and @objComp
      if (not last instanceof While and not @has-returned) or
         @is-comprehension or @in-comprehension

        res = Var o.scope.assign \results$ empty
        lines[*-1]?=makeReturn res.value

      ret = $.ReturnStatement argument: (res or empty)compile o

      @else?makeReturn!

    if yet
      @body.prepend Assign Var(yet), Literal \false

    # add push to res as last statement in body if present
    @body.add mid if mid?

    js.body = @body.compile o, LEVEL_TOP

    # compile else as separate if statement
    if yet
      js = $.BlockStatement body:
        * js # while/for statement
        * $.IfStatement do
            test: $.Identifier name: yet
            consequent: @compileBlock o, Block @else
      o.scope.free yet

    # add return statement if present
    if ret
      # avoid wrapping blockstatement in another statement
      # the Block.compile strategy will take care of unwrapping
      # top-level BlockStatements, but not nested ones
      if js instanceof $.BlockStatement
        js.body.push ret
      else
        js = $.BlockStatement body:
          * js # while/for
          * ret

    return js

#### For
# LiveScript's replacements for the `for` loop are array, object or range iterators.
class exports.For extends While
  ->
    import all it
    @item = null if @item instanceof Var and not @item.value
    @kind ?= []
    for @kind => @[..] = true
    @carp '`for own` requires `of`' if @own and not @object

  children: <[ item source from to step body ]>

  aSource: null

  show: -> ((@kind || []) ++ @index)join ' '

  addBody: (body) ->
    if @let
      @item = Literal \.. if delete @ref
      body = Block Call.let do
        with []
          ..push Assign Var(that), Var \index$$ if @index
          ..push Assign that,      Var \item$$ if @item
        body

    super body

    if @guard and @let and (@index or @item)
      @body.lines[0].if.traverse-children !~>
        if it instanceof Var
          if @index and it.value is @index
            it.value = \index$$
          if @item and it.value is @item.value
            it.value = \item$$
    if @let
      delete @index
      delete @item
    this

  compileNode: (o) ->
    o.loop = true
    @temps = []

    if @else
      @yet = o.scope.temporary \yet

    js = if @object?
      @compileForOfLoop o
    else if @to?
      @compileToFromLoop o
    else
      @compileForInLoop o

  # object loop
  compileForOfLoop: (o) ->
    # for @index, @item of @source then @body

    # XXX while @index is normally not optional, the `for let`
    # compilation will make it null, so we have to use a temporary
    if @index?
      idx = @index
    else
      idx = o.scope.temporary \i

    # make sure index (field name) is in scope
    o.scope.declare idx

    # cache @source if it's complex e.g.
    #
    # for k, v in get-obj() then ...
    # =>
    # for (k in (ref = get-obj())) {
    #   v = ref[k]
    #   ...
    # }
    [source-var, src-assign] = @source.cacheLoopReference o, \ref, not @object
    if src-assign?
      @temps.push source-var.value

    # @item = source-var[idx] at beginning of body
    if @item? and @item is not ''
      @body.prepend Assign do
        @item,
        Chain source-var .add Index(Var(idx)) # source-var[idx]

    if @let
      # desugaring of `for let` is done somewhere upstream, so
      # all we have to do is replace the placeholder variables
      # with the actual temporary names we have
      # XXX this is an abuse of the Literal AST node
      @body.traverseChildren !->
        switch it.value
        | \index$$ =>
          it.abused-by-let = true
          it.let-abuse = $.Identifier name: idx
        # item$$ only exists if @item exists and thus source-var is defined
        | \item$$  =>
          it.abused-by-let = true
          # source-var[idx]
          it.let-abuse = $.MemberExpression do
            computed: true
            object: source-var.compile o
            property: $.Identifier name: idx

    right = (if src-assign? then that else @source).compile o, LEVEL_LIST
    if @yet
      # set yet$ to true in in 'initializer' e.g.
      # for (i in (yet$ = true, obj)) { ... }
      # so if there are no properties, the `else` block runs
      right = $.SequenceExpression expressions:
        * $.AssignmentExpression do
            operator: \=
            left: $.Identifier name: @yet
            right: $.Literal value: true
        * right # actual `right`

    js = $.ForInStatement do
      left: $.Identifier name: idx
      right: right

    # node that desurgaring of `let` scoping as a function
    # and `when` guards is done already, so we just have to
    # compile the body.
    @compileBody o, js

  # indicies loop
  compileToFromLoop: (o) ->
    # for @index from @from @op(til/to) @to by @step then @body

    # @from is optional but is set to (Literal 0) by the parser/lexer if
    # missing from the source, so it's always there at this point

    # @step is optional, null if unspecified

    # body is optional e.g.
    # [i til 2] => [i$ for i$ from i til 2]

    @addBody Block Var idx if not @body

    inits   = Block!
    updates = Block!

    idx = o.scope.temporary \i
    @temps.push idx
    inits.add Assign (Var idx), @from

    if @yet
      # set yet$ to true in in 'initializer'
      # so if loop body never executes, the `else` block runs
      inits.add Assign (Var @yet), Literal \true

    [to-var, to-init] = @to.cacheLoopReference o, \to
    if to-init? # it was complex
      inits.add to-init
      @temps.push to-var.value

    if @step?
      [step-var, step-init] = @step.cacheLoopReference o, \step
      if step-init? # it was complex
        inits.add step-init
        @temps.push step-var.value

    test =
      if @step?
        step-number = parse-number @step
        if not Number.isNaN step-number
          # step is a literal number, just determine sign

          # <, >, >=, <=
          op = (if step-number >= 0 then '<' else '>') \
             + (if @op is \to then \= else '')

          $.BinaryExpression do
            operator: op
            left: $.Identifier name: idx
            right: to-var.compile o
        else
          # dynamically determine sign
          #
          # for a til b by step
          #
          # =>
          #
          # for (i$ = a, $to = b, step$ = step;
          #      $step < 0 ? i$ > $to : i$ < to$
          #      ; i$ += step$) ...

          op-add = if @op is \to then \= else ''

          $.ConditionalExpression do
            test: $.BinaryExpresion do
              operator: '<'
              left: step-var.compile o
              right: $.Literal value: 0
            consequent: $.BinaryExpresion do
              operator: '>' + op-add
              left: $.Identifier name: idx
              right: to-var.compile o
            alternate: $.BinaryExpresion do
              operator: '<' + op-add,
              left: $.Identifier name: idx
              right: to-var.compile o
      else if not Number.isNaN (from-no = parse-number @from)
      and     not Number.isNaN (to-no   = parse-number @to)
        # both to and from are number literals, determine sign
        op = (if from-no < to-no then '<' else '>') \
           + (if @op is \to then \= else '')

        $.BinaryExpression do
          operator: op
          left: $.Identifier name: idx
          right: to-var.compile o
      else
        # assume @to is greater than @a
        $.BinaryExpression do
          operator: '<'
          left: $.Identifier name: idx
          right: to-var.compile o

    update =
      if @step
        $.AssignmentExpression do
          operator: \+=
          left: $.Identifier name: idx
          right: step-var.compile o
      else if not Number.isNaN (from-no = parse-number @from)
      and     not Number.isNaN (to-no   = parse-number @to)
        $.UpdateExpression do
          prefix: true
          operator: if from-no < to-no then '++' else '--'
          argument: $.Identifier name: idx
      else
        # assume from < to
        $.UpdateExpression do
          prefix: true
          operator: \++
          argument: $.Identifier name: idx

    if @let
      # desugaring of `for let` is done somewhere upstream, so
      # all we have to do is replace the placeholder variables
      # with the actual temporary names we have
      # XXX this is an abuse of the Literal AST node
      @body.traverseChildren !->
        switch it.value
        | \index$$ =>
          it.abused-by-let = true
          it.let-abuse = $.Identifier name: @index

    js = $.ForStatement do
      init: inits.compile o, LEVEL_LIST
      test: test
      update: update

    @compileBody o, js

  compileForInLoop: (o) ->
    # for @item, @index in @source by @step then @body
    # @index is optional
    #
    # @item is also optional, in which case @ref is true e.g.
    #
    #     for stuff then ..member
    #

    inits = Block!

    idx = o.scope.temporary \i
    @temps.push idx
    inits.add Assign (Var idx), Literal 0

    if @yet
      # set yet$ to true in in 'initializer'
      # so if loop body never executes, the `else` block runs
      inits.add Assign (Var @yet), Literal \true

    len = o.scope.temporary \len
    @temps.push len

    if @ref?
      @item = Var o.scope.temporary \x

    [source-var, source-init] = @source.cacheLoopReference o, \ref
    if source-init? # it was complex
      # len$ = ($ref = source).length
      inits.add Assign (Var len), Chain source-init, [Index Key \length]
      @temps.push source-var.value
    else
      # len$ = source.length
      inits.add Assign (Var len), Chain @source, [Index Key \length]

    if @step?
      [step-var, step-init] = @step.cacheLoopReference o, \step
      if step-init? # it was complex
        inits.add step-init
        @temps.push step-var.value

    test = $.BinaryExpression do
      operator: '<'
      left: $.Identifier name: idx
      right: $.Identifier name: len

    update =
      if @step?
        $.AssignmentExpression do
          operator: '+='
          left: $.Identifier name: idx
          right: step-var
      else
        $.UpdateExpression do
          prefix: true
          operator: '++'
          argument: $.Identifier name: idx

    if @item? and not @item.isEmpty!
      @body.prepend Assign @item,
        Chain source-var .add Index(Var(idx)) # source-var[idx]

    if @index? and @index is not ''
      @body.prepend Assign (Var @index), Var idx

    # set scope's cascade ref to the temp var if present
    o.ref = @item.value if @ref

    if @let
      # desugaring of `for let` is done somewhere upstream, so
      # all we have to do is replace the placeholder variables
      # with the actual temporary names we have
      # XXX this is an abuse of the Literal AST node
      @body.traverseChildren !->
        switch it.value
        | \index$$ =>
          it.abused-by-let = true
          it.let-abuse = $.Identifier name: idx
        # item$$ only exists if @item exists and thus source-var is defined
        | \item$$  =>
          it.abused-by-let = true
          # source[idx]
          it.let-abuse = $.MemberExpression do
            computed: true
            object: source-var.compile o
            property: $.Identifier name: idx

    js = $.ForStatement do
      init: inits.compile o, LEVEL_LIST
      test: test
      update: update

    @compileBody o, js

  parse-number = (node) ->
    if node instanceof Literal
      parseFloat node.value
    else if node instanceof Unary
    and node.op is \-
    and node.it instanceof Literal
      -(parseFloat node.it.value)

#### Try
# Classic `try`-`catch`-`finally` block with optional `catch`.
class exports.Try extends Node
  (@attempt, @thrown, @recovery, @ensure) ->
    @recovery?lines.unshift Assign (@thrown or Var \e), Var \e$

  children: <[ attempt recovery ensure ]>

  show: -> @thrown

  isStatement: YES

  isCallable: -> @recovery?isCallable! and @attempt.isCallable!

  getJump: -> @attempt.getJump it or @recovery?getJump it

  makeReturn: ->
    @attempt .=makeReturn it
    @recovery?=makeReturn it
    this

  compileNode: (o) ->
    # try { @attempt } catch (@thrown) { @recovery } finally { @ensure }

    # @recovery is optional, defaulting to empty block
    # @ensure is optional, just like the `finally` clause

    # we need at least either a finally or a catch block,
    # so create empty catch if neither are present
    if not @recovery? and not @ensure?
      @recovery = Block []

    $.TryStatement do
      block: @compileBlock o, @attempt

      # JS AST supports multiple handlers, so we use an array
      handlers:
        if @recovery
          [$.CatchClause do
             # if there's a specified throw param, it's assigned
             # in the @recovery body by the constructor logic
             param: $.Identifier name: \e$
             body: @compileBlock o, @recovery]
        else
          []

      finalizer: if @ensure? then @compileBlock o, @ensure

#### Switch
# Compiles to the regular js `switch`-`case`-`default`,
# but with forced `break` after each cases.
class exports.Switch extends Node
  (@type, @topic, @cases, @default) ->
    if type is \match
      @target = Arr topic if topic
      @topic = null
    else
      if topic
        throw "can't have more than one topic in switch statement" if topic.length > 1
        @topic.=0
    if @cases.length and (last = @cases[*-1]).tests.length is 1
    and last.tests.0 instanceof Var and last.tests.0.value is \_
      @cases.pop!
      @default = last.body

  children: <[ topic cases default ]>

  aSource: \topic, aTargets: [\cases]

  show: -> @type

  isStatement: YES

  isCallable: ->
    for c in @cases when not c.isCallable! then return false
    if @default then @default.isCallable! else true

  getJump: (ctx or {}) ->
    ctx.break = true
    for c in @cases then return that if c.body.getJump ctx
    @default?getJump ctx

  makeReturn: ->
    for c in @cases then c.makeReturn it
    @default?makeReturn it
    this

  compileNode: (o) ->
    [target-node, target] = Chain @target .cacheReference o if @target

    topic = if @type is \match
      t = if target then [target-node] else []
      Block (t ++ [Literal \false]) .compile o, LEVEL_PAREN
    else
      if !!@topic
        @anaphorize!compile o, LEVEL_PAREN
      else
        # topic falsy, so use Literal false to make short cases
        $.Literal value: false

    stop  = @default or @cases.length - 1

    o.break = true

    js-cases = []
    # flatten compiled cases
    for c, i in @cases
      js-cases.push ...c.compileCase do
        o, i is stop, (@type is \match or !topic), @type, target

    if @default and @default.lines.length > 0
      js-cases.push do
        # test: null means `default`, confusingly enough
        $.SwitchCase do
          test: null
          consequent: @default.compileBlockBody o, LEVEL_TOP

    $.SwitchStatement do
      discriminant: topic
      cases: js-cases

#### Case
class exports.Case extends Node
  (@tests, @body) ->

  children: <[ tests body ]>

  isCallable: -> @body.isCallable!

  makeReturn: ->
    @body.makeReturn it unless @body.lines[*-1]?value is \fallthrough
    this

  # a single LiveScript Case can compile to multple
  # JS cases, so this returns an array of $.SwitchCase
  compileCase: (o, nobr, bool, type, target) ->
    tests = []
    for test in @tests
      test.=expandSlice(o)unwrap!
      if test instanceof Arr and type isnt \match
        for t in test.items then tests.push t
      else
        tests.push test

    if tests.length is 0
      tests.push Literal \void

    if type is \match
      for test, i in tests
        tar = Chain target .add Index (Literal i), \., true
        tests[i] = Chain test .auto-compare (if target then [tar] else null)

    if bool
      binary = if type is \match then \&& else \||
      [t] = tests; i = 0; while tests[++i] then t = Binary binary, t, that
      tests = [(@<<<{t, aSource: \t, aTargets: [\body]})anaphorize!invert!]

    [..., last-case] = js-cases =
      for t in tests
        $.SwitchCase do
          test: t.compile o, LEVEL_PAREN
          consequent: []

    {lines} = @body; last = lines[*-1]

    ft = last?value is \fallthrough

    # fallthrough is represented as a Var, so remove it to avoid
    # compiling a dummy variable
    lines.pop! if ft

    # unless force no-break, or fallthrough, or an explicit jump
    unless nobr or ft or last instanceof Jump
      # add break statement
      @body.add Jump \break

    # compile body to last case
    last-case.consequent = @body.compileBlockBody o, LEVEL_TOP

    return js-cases

#### If
# The `if`/`else` structure that acts as both statement and expression.
class exports.If extends Node
  (@if, @then, @un) ~>
    # whether to compile ternary expressions in one line
    # default false, but caller-settable
    @compact = false

  children: <[ if then else ]>

  aSource: \if, aTargets: [\then]

  show: -> @un and \!

  terminator: ''

  ::delegate <[ isCallable isArray isString isRegex ]> ->
    @else?[it]! and @then[it]!

  getJump: -> @then.getJump it or @else?getJump it

  makeReturn: ->
    @then.=makeReturn it
    @else?=makeReturn it
    this

  compileNode: (o, level ? o.level) ->
    if @un then @if.=invert! else @soak or @anaphorize!

    if level is LEVEL_TOP
      @compileStatement o
    else
      @compileExpression o

  compileStatement: (o) ->
    $.IfStatement do
      test:       @if  .compile o, LEVEL_PAREN
      consequent: wrap-statement @then.compile o, LEVEL_TOP
      alternate:  if @else? then wrap-statement @else.compile o, LEVEL_TOP

  compileExpression: (o) ->
    {then: thn, else: els or Literal \void} = this

    if @void
      @then.void = @else.void = true

    if not @else and (@cond or @void)
      # optimize `if a then b` => `a && b`
      Binary \&& @if, thn .compile o
    else
      $.ConditionalExpression do
        test:       @if.compile o, LEVEL_PAREN
        consequent: @then.compile o, LEVEL_PAREN

        # alternate is _required_ for ternary expressions, so
        # use `void` if not specified
        alternate:  (@else or Literal \void).compile o, LEVEL_PAREN

  # Unfolds a node's child if soak,
  # then tuck the node under the created **If**.
  @unfoldSoak = (o, parent, name) ->
    if parent[name]unfoldSoak o
      parent[name] = that.then
      that <<< {parent.cond, parent.void, then: Chain parent}

  function wrap-statement js
    if js instanceof $.Statement
      js
    else
      $.ExpressionStatement expression: js


#### Label
# A labeled block or statement.
class exports.Label extends Node
  (@label or \_, @it) ->
    if fun = it instanceof [Fun, Class] and it or
             it.calling and it.it.head
      fun.name or fun <<< {name: @label, +labeled}
      return it

  ::{children, isCallable, isArray} = Parens::

  show: -> @label

  isStatement: YES

  getJump: (ctx or {}) ->
    (ctx.labels ?= []).push @label
    @it.getJump ctx <<< {+\break}

  makeReturn: -> @it.=makeReturn it; this

  compileNode: (o) ->
    {label, it} = this

    labels = o.labels = [...o.labels or []]

    @carp "duplicate label \"#label\"" if label in labels

    labels.push label

    $.LabeledStatement do
      label: $.Identifier label
      body:
        if it.isStatement!
          it.compile o
        else
          $.ExpressionStatement expression: it.compile o

#### Cascade
# `prog1` is 'with' when used with with:
#
# with @input
#   @output
#
# otherwise, `prog1` is `cascade`:
#
# @input
#   @output
class exports.Cascade extends Node
  (@input, @output, @prog1) ~>

  show: -> @prog1

  children: <[ input output ]>

  terminator: ''

  ::delegate <[ isCallable isArray isString isRegex ]> ->
    @[if @prog1 then \input else \output][it]!

  getJump: -> @output.getJump it

  makeReturn: (@ret) -> this

  compileNode: ({level}:o) ->
    {input, output, prog1, ref} = this

    if prog1 and (\ret of this or level and not @void)
      output.add (Literal(\..) <<< {+cascadee})

    if \ret of this
      output.=makeReturn @ret

    if ref
    then prog1 or output = Assign Var(ref), output
    else ref = o.scope.temporary \x

    if input instanceof Cascade
    then input <<< {ref}
    else input &&= Assign Var(ref), input

    if o.level is LEVEL_TOP
      o.level = LEVEL_PAREN

    out = Block output .compile o <<< ref: new String ref
    @carp "unreferred cascadee" if prog1 is \cascade and not o.ref.erred

    if level is LEVEL_TOP
      $.BlockStatement body:
        * input.compile o
        * out
    else
      $.SequenceExpression expressions:
        * input.compile o
        * out
#### JS
# Embedded JavaScript snippets.
class exports.JS extends Node
  (@code, @literal, @comment) ~>

  show: -> if @comment then @code else "`#{@code}`"

  terminator: ''

  ::isAssignable = ::isCallable = -> not @comment

  compile: ->
    console.error "JS used internally:#{if @comment then '(comment)' else ''} #{@code}"
    @code

#### Require
class exports.Require extends Node
  (@body) ~>

  children: <[ body ]>

  compile: (o) ->
    var chain
    strip-string = (val) ->
      if val == //^['"](.*)['"]$// then that.1 else val
    get-file-name = (val) ->
      strip-string val .split '/' .[*-1].split '.' .0
        .replace /-[a-z]/ig, -> it.char-at 1 .to-upper-case!

    get-value = (item) ~>
      | item instanceof Key     => item.name
      | item instanceof Var     => item.value
      | item instanceof Literal => item.value
      | item instanceof Index   => get-value item.key
      | item instanceof Chain   =>
        if item.tails?length
          chain := item.tails
        get-value item.head
      | otherwise               => item

    process-item = (item) ->
      chain := null
      [asg, value] = switch
      | item instanceof Prop    => [get-value item.key; item.val]
      | item instanceof Chain   =>
        if item.tails?length
          chain := item.tails
          [item.tails[*-1], item.head]
        else
          [item.head, item.head]
      | otherwise               => [item, item]

      asg = get-file-name get-value asg
      value = strip-string get-value value

      main = Chain Var 'require' .add Call [Literal "'#value'"]
      Assign (Var asg), (if chain then Chain main, chain else main) .compile o

    if @body.items?
      Block [process-item item for item in @body.items] .compile o
    else
      process-item @body .compile o

#### Util
# A wrapper node for utility functions.
class exports.Util extends Node
  (@verb) ~>

  {(Jump::)show}

  isCallable: YES

  compile: (o) ->
    Var util @verb .compile o

  ##### Util.Extends
  # An operator that sets up class-ical inheritance between two constructors,
  # returning the left one.
  @Extends = -> Call.make Util(\extend), &[0 1]

#### Vars
# Declares uninitialized variables.
class exports.Vars extends Node
  (@vars) ~>

  children: [\vars]

  makeReturn: THIS

  compile: (o, level) ->
    for {value}:v in @vars
      v.carp 'invalid variable declaration' unless v instanceof Var
      v.carp "redeclaration of \"#value\"" if o.scope.check value
      o.scope.declare value, v

    # emit EmptyStatement essentially, as variable declaration
    # is hoisted into the o.scope
    Literal \void .compile o, level

#### Parser Utils
# Helpers for modifying nodes in [parser](../lib/parser.js).

exports.L = (yylineno, node) -> node import line: yylineno + 1

exports.Decl = (type, nodes, lno) ->
  throw SyntaxError "empty #type on line #lno" unless nodes.0
  DECLS[type] nodes

DECLS =
  export: (lines) ->
    i = -1; out = Util \out
    while node = lines[++i]
      if node instanceof Block
        lines.splice i-- 1 ...node.lines
        continue
      if node instanceof Fun and node.name
        lines.splice i++ 0 Assign Chain(out, [Index Key that]), Var that
        continue
      lines[i] =
        if node.varName!
        or node instanceof Assign and node.left. varName!
        or node instanceof Class  and node.title?varName!
        then Assign Chain(out, [Index Key that]), node
        else Import out, node
    Block lines

  import: (lines, all) ->
    for line, i in lines then lines[i] = Import Literal(\this), line, all
    Block lines

  importAll: -> @import it, true

  const: (lines) ->
    for node in lines
      node.op is \= or node.carp 'invalid constant variable declaration'
      node.const = true
    Block lines

  var: Vars

##### Scope
# Regulates lexical scoping within LiveScript. As you
# generate code, you creat a tree of scopes in the same shape as the nested
# functions. Each scope knows about the function parameters and the variables
# declared within it, and has references to its parent/shared enclosing scopes.
!function Scope @parent, @shared
  @variables = {}
Scope ::=
  READ_ONLY: const:\constant function:\function undefined:\undeclared

  # Adds a new variable or overrides an existing one.
  # `type` is either a string enum of "const" or "var" or "reuse", or
  # "arg" or "function",  or an object with the field `value` (from the
  # `assign` method).
  #
  # The `value` fields are for variables that are
  # initialized in the VariableDeclarator, and the value SHOULD
  # be a valid LiveScript AST node.
  #
  # XXX as a hack for curry-code-check and utility functions,
  # `type.value` may also be a JS AST node, in which case `type.is-js` MUST
  # be true.
  # This is respected by the Scope#assign-js and Scope#require-util methods.
  #
  # variables of type 'function' or "arg" are only added to
  # be able to check existence within a scope. They are not
  # emitted in the VariableDeclaration at the top of the scope.
  add: (name, type, node) ->
    if node and t = @variables"#name."
      if @READ_ONLY[t] or @READ_ONLY[type]
        node.carp "redeclaration of #that \"#name\""
      else if t is type is \arg
        node.carp "duplicate parameter \"#name\""
      else if t is \upvar
        node.carp "accidental shadow of \"#name\""
      return name if t in <[ arg function ]>
    # Dot-suffix to bypass `Object::` members.
    @variables"#name." = type
    name

  get: (name) -> @variables"#name."

  # Declares a variable unless declared already.
  declare: (name, node, constant) ->
    if @shared
      return if @check name
      scope = that
    else
      scope = this
    scope.add name, (if constant then \const else \var), node

  # Ensures that an assignment is made at the top of this scope.
  assign: (name, value) -> @add name, {value}

  # Ensures a util function or helper assignment is declared as a statement at
  # the bottom of this scope, e.g. var slice$ = [].slice.
  require-util: (name, value) -> @add name, {value, +is-js}

  # Ensures that an assignment is made at the top of this scope for
  # already compiled JS AST.
  assign-js: (name, value) -> @add name, {value, +is-js}

  # If we need to store an intermediate result, find an available name for a
  # compiler-generated variable. `var$`, `var1$`, and so on.
  temporary: (name || \ref) ->
    until @variables"#name\$." in [\reuse void]
      name = if name.length < 2 and name < \z
        then String.fromCharCode name.charCodeAt! + 1
        else name.replace /\d*$/ -> ++it
    @add name + \$, \var

  # Allows a variable to be reused.
  free: (name) -> @add name, \reuse

  # Checks to see if a variable has already been declared.
  # Walks up the scope if `above` flag is specified.
  check: (name, above) ->
    if not name?
      throw new Error "checking for var with undefined name!"
    return type if (type = @variables"#name.") or not above
    @parent?check name, above

  # Checks if a variable can be reassigned.
  checkReadOnly: (name) ->
    return that if @READ_ONLY[@check name, true]
    @variables"#name." ||= \upvar
    ''

  # Concatenates the declarations into this scope.
  # returns a new array of statements.
  add-scope: (js-statements, o) ->
    vrs = []; asn = []; fun = []
    for name, type of @variables
      name.=slice 0 -1
      if type in <[ var const reuse ]>
        vrs.push $.VariableDeclarator do
          id: $.Identifier {name}
          init: null
      else if type.value
        if type.is-js and type.value.type is \FunctionDeclaration
          # XXX respect the caller's utility name by changing
          # the function declaration Identifier
          type.value.id = $.Identifier {name}
          fun.push type.value
        else
          asn.push $.VariableDeclarator do
            id: $.Identifier {name}
            init:
              if type.is-js
                type.value
              else
                type.value.compile o <<< {level: LEVEL_PAREN}

    decls = vrs ++ asn
    decl = if decls.length > 0
      [$.VariableDeclaration declarations: decls, kind: \var]
    else
      []

    return decl ++ js-statements ++ fun

##### Constants

function YES  then true
function NO   then false
function THIS then this
function VOID then void

unwrap-ast = (program-node) ->
  if program-node.body.0.type is \FunctionDeclaration
    program-node.body.0
  else
    # the only other util type is an expression like `[].slice`
    program-node.body.0.expression

# parse helper utils to JS AST on-demand
lazy-parse = (util-defs) ->
  with {}
    for let name, js of util-defs
      var ast
      ..[name] = ->
        if ast?
          return ast
        else
          ast ?:= unwrap-ast esprima.parse(js)

# XXX note that the function name is only present so that
# the string parses as a FunctionDeclaration in JS/esprima.
# Scope#add-scope will clobber the function name with whatever
# the caller to `util` wanted it to be.
UTILS = lazy-parse do
  # Creates an object's prototypal child, ensuring `__proto__`.
  clone: '''function clone$(it){
    function fun(){} fun.prototype = it;
    return new fun;
  }'''
  # Sets up `.prototype` between a pair of constructors
  # as well as `.constructor` and `.superclass` references.
  extend: '''function extend$(sub, sup){
    function fun(){} fun.prototype = (sub.superclass = sup).prototype;
    (sub.prototype = new fun).constructor = sub;
    if (typeof sup.extended == 'function') sup.extended(sub);
    return sub;
  }'''

  # Creates a bound method.
  bind: '''function bind$(obj, key, target){
    return function(){ return (target || obj)[key].apply(obj, arguments) };
  }'''

  # Copies properties from right to left.
  import: '''function import$(obj, src){
    var own = {}.hasOwnProperty;
    for (var key in src) if (own.call(src, key)) obj[key] = src[key];
    return obj;
  }'''
  importAll: '''function importAll$(obj, src){
    for (var key in src) obj[key] = src[key];
    return obj;
  }'''

  repeatString: '''function repeatString$(str, n){
    for (var r = ''; n > 0; (n >>= 1) && (str += str)) if (n & 1) r += str;
    return r;
  }'''
  repeatArray: '''function repeatArray$(arr, n){
    for (var r = []; n > 0; (n >>= 1) && (arr = arr.concat(arr)))
      if (n & 1) r.push.apply(r, arr);
    return r;
  }'''

  in: '''function in$(x, xs){
    var i = -1, l = xs.length >>> 0;
    while (++i < l) if (x === xs[i]) return true;
    return false;
  }'''

  out: '''typeof exports != 'undefined' && exports || this'''

  curry: '''function curry$(f, bound){
    var context,
    _curry = function(args) {
      return f.length > 1 ? function(){
        var params = args ? args.concat() : [];
        context = bound ? context || this : this;
        return params.push.apply(params, arguments) <
            f.length && arguments.length ?
          _curry.call(context, params) : f.apply(context, params);
      } : f;
    };
    return _curry();
  }'''

  flip: '''function flip$(f){
    return curry$(function (x, y) { return f(y, x); });
  }'''

  partialize: '''function partialize$(f, args, where){
    var context = this;
    return function(){
      var params = slice$.call(arguments), i,
          len = params.length, wlen = where.length,
          ta = args ? args.concat() : [], tw = where ? where.concat() : [];
      for(i = 0; i < len; ++i) { ta[tw[0]] = params[i]; tw.shift(); }
      return len < wlen && len ?
        partialize$.apply(context, [f, ta, tw]) : f.apply(context, ta);
    };
  }'''
  not: '''function not(x){ return !x; }'''

  # modified version of underscore.js's _.isEqual and eq functions
  deepEq: '''function deepEq$(x, y, type){
    var toString = {}.toString, hasOwnProperty = {}.hasOwnProperty,
        has = function (obj, key) { return hasOwnProperty.call(obj, key); };
    var first = true;
    return eq(x, y, []);
    function eq(a, b, stack) {
      var className, length, size, result, alength, blength, r, key, ref, sizeB;
      if (a == null || b == null) { return a === b; }
      if (a.__placeholder__ || b.__placeholder__) { return true; }
      if (a === b) { return a !== 0 || 1 / a == 1 / b; }
      className = toString.call(a);
      if (toString.call(b) != className) { return false; }
      switch (className) {
        case '[object String]': return a == String(b);
        case '[object Number]':
          return a != +a ? b != +b : (a == 0 ? 1 / a == 1 / b : a == +b);
        case '[object Date]':
        case '[object Boolean]':
          return +a == +b;
        case '[object RegExp]':
          return a.source == b.source &&
                 a.global == b.global &&
                 a.multiline == b.multiline &&
                 a.ignoreCase == b.ignoreCase;
      }
      if (typeof a != 'object' || typeof b != 'object') { return false; }
      length = stack.length;
      while (length--) { if (stack[length] == a) { return true; } }
      stack.push(a);
      size = 0;
      result = true;
      if (className == '[object Array]') {
        alength = a.length;
        blength = b.length;
        if (first) {
          switch (type) {
          case '===': result = alength === blength; break;
          case '<==': result = alength <= blength; break;
          case '<<=': result = alength < blength; break;
          }
          size = alength;
          first = false;
        } else {
          result = alength === blength;
          size = alength;
        }
        if (result) {
          while (size--) {
            if (!(result = size in a == size in b && eq(a[size], b[size], stack))){ break; }
          }
        }
      } else {
        if ('constructor' in a != 'constructor' in b || a.constructor != b.constructor) {
          return false;
        }
        for (key in a) {
          if (has(a, key)) {
            size++;
            if (!(result = has(b, key) && eq(a[key], b[key], stack))) { break; }
          }
        }
        if (result) {
          sizeB = 0;
          for (key in b) {
            if (has(b, key)) { ++sizeB; }
          }
          if (first) {
            if (type === '<<=') {
              result = size < sizeB;
            } else if (type === '<==') {
              result = size <= sizeB
            } else {
              result = size === sizeB;
            }
          } else {
            first = false;
            result = size === sizeB;
          }
        }
      }
      stack.pop();
      return result;
    }
  }'''

  # apply arguments to a constructor call, which can't
  # be done simply.
  splatNew: '''function splatNew$(clazz, args) {
    var ctor = function(){}, result, child, t;
    ctor.prototype = clazz.prototype;
    child = new ctor; result = clazz.apply(child, args);
    return (t = typeof result)  == "object" || t == "function"
           ? result || child
           : child;
  }'''

  # Shortcuts to speed up the lookup time for native methods.
  split    : "''.split"
  replace  : "''.replace"
  toString : '({}).toString'
  join     : '[].join'
  slice    : '[].slice'
  splice   : '[].splice'

# Each level indicates a node's position in the AST.
LEVEL_TOP    = 0  # ...;
LEVEL_PAREN  = 1  # (...)
LEVEL_LIST   = 2  # [...]
LEVEL_COND   = 3  # ... ? x : y
LEVEL_OP     = 4  # !...
LEVEL_CALL   = 5  # ...()

# Operator precedences.
let @ = PREC = {unary: 0.9}
  @\&& = @\|| = @\xor                             = 0.2
  @\.&.  = @\.^.  = @\.|.                         = 0.3
  @\== = @\!= = @\~= = @\!~= = @\=== = @\!==      = 0.4
  @\<  = @\>  = @\<=  = @\>= = @of = @instanceof  = 0.5
  @\<<= = @\>>= = @\<== = @\>== = @\++            = 0.5
  @\.<<. = @\.>>. = @\.>>>.                       = 0.6
  @\+  = @\-                                      = 0.7
  @\*  = @\/  = @\%                               = 0.8

TAB = ' ' * 2

ID = /^(?!\d)[\w$\xAA-\uFFDC]+$/

SIMPLENUM = /^\d+$/

##### Helpers

# Declares a utility function at the top level.
function util then Scope.root.require-util it+\$ UTILS[it]!

function fold f, memo, xs
  for x in xs then memo = f memo, x
  memo
