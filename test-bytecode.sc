using import String

import .bcparser
import .vm

local tests =
    arrayof String
        "hello_world.bc"
        "control_flow.bc"
        # "logic.bc"

for tst in tests
    path := (String "tests/") .. tst
    local machine = (vm.VM (bcparser.parse path))
    'execute machine
