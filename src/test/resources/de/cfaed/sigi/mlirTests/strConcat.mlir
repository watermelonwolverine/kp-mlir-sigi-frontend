module {
    func.func private @"sigi::pp"(!sigi.stack) -> !sigi.stack attributes { sigi.builtinfunc }
    // apply: str, (str -> str) -> str
    func.func private @apply(%s0: !sigi.stack) -> !sigi.stack {
        // -> \f;
        %s1, %v1_f = sigi.pop %s0: !closure.box<(!sigi.stack) -> !sigi.stack> // f: (str -> str)
        %s2 = closure.call %v1_f (%s1) : !closure.box<(!sigi.stack) -> !sigi.stack> // call f: str -> str
        return %s2: !sigi.stack
    }
    // show: str ->
    func.func private @show(%s0: !sigi.stack) -> !sigi.stack {
        %s1 = func.call @"sigi::pp"(%s0) : (!sigi.stack) -> !sigi.stack // str -> str
        %s2, %v1 = sigi.pop %s1: !sigi.str // pop intrinsic
        return %s2: !sigi.stack
    }
    // helloworld0: ->
    func.func private @helloworld0(%s0: !sigi.stack) -> !sigi.stack {
        %v1 = sigi.constant "Hello ": !sigi.str
        %s1 = sigi.push %s0, %v1: !sigi.str
        %v2 = sigi.constant "World!": !sigi.str
        %s2 = sigi.push %s1, %v2: !sigi.str
        // &
        %s3, %v3 = sigi.pop %s2: !sigi.str
        %s4, %v4 = sigi.pop %s3: !sigi.str
        %v5 = sigi.concat_str %v4, %v3: !sigi.str
        %s5 = sigi.push %s4, %v5: !sigi.str
        %s6 = func.call @show(%s5) : (!sigi.stack) -> !sigi.stack // str ->
        return %s6: !sigi.stack
    }
    // helloworld1: ->
    func.func private @helloworld1(%s0: !sigi.stack) -> !sigi.stack {
        %v1 = sigi.constant "Hello ": !sigi.str
        %s1 = sigi.push %s0, %v1: !sigi.str
        %v2 = sigi.constant "World!": !sigi.str
        %s2 = sigi.push %s1, %v2: !sigi.str
        // &
        %s3, %v3 = sigi.pop %s2: !sigi.str
        %s4, %v4 = sigi.pop %s3: !sigi.str
        %v5 = sigi.concat_str %v4, %v3: !sigi.str
        %s5 = sigi.push %s4, %v5: !sigi.str
        %s6 = func.call @show(%s5) : (!sigi.stack) -> !sigi.stack // str ->
        return %s6: !sigi.stack
    }
    // helloworld2: ->
    func.func private @helloworld2(%s0: !sigi.stack) -> !sigi.stack {
        %v1 = sigi.constant "Hello ": !sigi.str
        %s1 = sigi.push %s0, %v1: !sigi.str
        %v2 = closure.box [] (%s2 : !sigi.stack) -> !sigi.stack { // str -> str
            %v3 = sigi.constant "World!": !sigi.str
            %s3 = sigi.push %s2, %v3: !sigi.str
            // &
            %s4, %v4 = sigi.pop %s3: !sigi.str
            %s5, %v5 = sigi.pop %s4: !sigi.str
            %v6 = sigi.concat_str %v5, %v4: !sigi.str
            %s6 = sigi.push %s5, %v6: !sigi.str
            closure.return %s6: !sigi.stack
        }
        %s7 = sigi.push %s1, %v2: !closure.box<(!sigi.stack) -> !sigi.stack>
        %s8 = func.call @apply(%s7) : (!sigi.stack) -> !sigi.stack // str, (str -> str) -> str
        %s9 = func.call @show(%s8) : (!sigi.stack) -> !sigi.stack // str ->
        return %s9: !sigi.stack
    }
    // concatWorld: str -> str
    func.func private @concatWorld(%s0: !sigi.stack) -> !sigi.stack {
        %v1 = sigi.constant "World!": !sigi.str
        %s1 = sigi.push %s0, %v1: !sigi.str
        // &
        %s2, %v2 = sigi.pop %s1: !sigi.str
        %s3, %v3 = sigi.pop %s2: !sigi.str
        %v4 = sigi.concat_str %v3, %v2: !sigi.str
        %s4 = sigi.push %s3, %v4: !sigi.str
        return %s4: !sigi.stack
    }
    // helloworld3: ->
    func.func private @helloworld3(%s0: !sigi.stack) -> !sigi.stack {
        %v1 = sigi.constant "Hello ": !sigi.str
        %s1 = sigi.push %s0, %v1: !sigi.str
        %s2 = func.call @concatWorld(%s1) : (!sigi.stack) -> !sigi.stack // str -> str
        %s3 = func.call @show(%s2) : (!sigi.stack) -> !sigi.stack // str ->
        return %s3: !sigi.stack
    }
    // __main__: ->
    func.func @__main__(%s0: !sigi.stack) -> !sigi.stack attributes {sigi.main} {
        %s1 = func.call @helloworld0(%s0) : (!sigi.stack) -> !sigi.stack // ->
        %s2 = func.call @helloworld1(%s1) : (!sigi.stack) -> !sigi.stack // ->
        %s3 = func.call @helloworld2(%s2) : (!sigi.stack) -> !sigi.stack // ->
        %s4 = func.call @helloworld3(%s3) : (!sigi.stack) -> !sigi.stack // ->
        return %s4: !sigi.stack
    }
}
