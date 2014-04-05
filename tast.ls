require! './src/ast'
require! util
require! escodegen

json = ''
process.stdin
  ..resume!
  ..on \data !-> json += it
  ..once \end !->
    try
      if process.argv.2?
        console.log JSON.stringify do
          ast.parse json .compile-root {+bare}
          null
          '  '
      else
        console.log escodegen.generate do
          ast.parse json .compile-root {+bare}
    catch
      console.error e
      console.error util.inspect do
        ast.parse json .compile-root {+bare}
        depth: 20, colors: true
