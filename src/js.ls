# Parser API AST nodes
#
# https://developer.mozilla.org/en-US/docs/SpiderMonkey/Parser_API

export
  class Node
    # XXX while porting compilation to JS AST, nodes will usually
    # end up in string concatenation.
    toString: -> "«#{@type}#{JSON.stringify strip-type this}»"

    function strip-type
      # XXX remove `type` field
      with {}
        for k, v of it
          ..[k] = v unless k is \type

  # setup superclass inheritance for `instanceof` checking
  # not entirely necessary, but might be useful
  class Statement extends Node
  class Declaration extends Statement
  class Expression extends Node

# map of node name to superclass.
const NODES =
  Program               : Node

  EmptyStatement        : Statement
  BlockStatement        : Statement
  ExpressionStatement   : Statement
  IfStatement           : Statement
  LabeledStatement      : Statement
  BreakStatement        : Statement
  ContinueStatement     : Statement
  WithStatement         : Statement
  SwitchStatement       : Statement
  ReturnStatement       : Statement
  ThrowStatement        : Statement
  TryStatement          : Statement
  WhileStatement        : Statement
  DoWhileStatement      : Statement
  ForStatement          : Statement
  ForInStatement        : Statement
  DebuggerStatement     : Statement

  FunctionDeclaration   : Declaration
  VariableDeclaration   : Declaration

  VariableDeclarator    : Node

  ThisExpression        : Expression
  ArrayExpression       : Expression
  ObjectExpression      : Expression
  FunctionExpression    : Expression
  SequenceExpression    : Expression
  UnaryExpression       : Expression
  BinaryExpression      : Expression
  UpdateExpression      : Expression
  LogicalExpression     : Expression
  ConditionalExpression : Expression
  NewExpression         : Expression
  CallExpression        : Expression
  MemberExpression      : Expression

  SwitchCase            : Node
  CatchClause           : Node

  Identifier            : Expression
  Literal               : Expression

for let node, zuper of NODES
  exports[node] = class extends zuper
    # instead of tuple constructors, take an object for readability
    # e.g. Program(body: [...])
    (fields) ~> import fields
    @displayName = node
    type: node
