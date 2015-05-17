-record(expect, {
        module,
        funName,
        args,
        return,
        extra
    }).

-record(argTest, {
        value,
        check_value = false,
        type,
        validation
    }).

-record(returnTest, {
        value,
        check_value = false,
        type,
        validation
    }).

-record(extraTest, {
        ms,
        times,
        precondition,
        postcondition,
        check_args_size = true
    }).

-record(checkMatch, {
        moduleName = true,
        funName = true,
        argsSize = true,
        argsTypes = false,
        argsValues = false,
        argsValidations = false
    }).
