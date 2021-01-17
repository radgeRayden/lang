import .bcparser
import .vm
local program = (bcparser.parse "hello_world.bc")
vm.execute program
