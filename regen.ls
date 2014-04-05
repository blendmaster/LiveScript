require! escodegen
require! esprima

json = ''
process.stdin
  ..resume!
  ..on \data !-> json += it
  ..once \end !->
    console.log escodegen.generate esprima.parse json

