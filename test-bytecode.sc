import .bcparser
import .vm
local program = (bcparser.parse "tests/hello_world.bc")
vm.execute program
