import .bcparser
import .vm
local program = (bcparser.parse "tests/hello_world.bc")
vm.execute program
local program = (bcparser.parse "tests/control_flow.bc")
vm.execute program
