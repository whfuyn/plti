module BigStep = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)

  let rec eval = expr => {
    switch expr {
    | Cst(i) => i
    | Add(expr1, expr2) => eval(expr1) + eval(expr2)
    | Mul(expr1, expr2) => eval(expr1) * eval(expr2)
    }
  }
}

module SmallStep = {
  type instr = Cst(int) | Add | Mul
  type instrs = list<instr>
  type operand = int
  type stack = list<operand>

  let rec eval = (instrs: instrs, stack: stack) => {
    switch (instrs, stack) {
    | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stack})
    | (list{Add, ...rest}, list{a, b, ...stack}) => eval(rest, list{a + b, ...stack})
    | (list{Mul, ...rest}, list{a, b, ...stack}) => eval(rest, list{a * b, ...stack})
    | (list{}, list{res, ..._}) => res
    | _ => assert false
    }
  }
}

let rec compile = (src: BigStep.expr): SmallStep.instrs => {
  switch src {
  | Cst(i) => list{Cst(i)}
  | Add(expr1, expr2) => {
      let target1 = compile(expr1)
      let target2 = compile(expr2)
      Belt.List.concatMany([target1, target2, list{Add}])
    }

  | Mul(expr1, expr2) => {
      let target1 = compile(expr1)
      let target2 = compile(expr2)
      Belt.List.concatMany([target1, target2, list{Mul}])
    }
  }
}

module Tests = {
  let test_compile = (src: BigStep.expr) => {
    let compiled = compile(src)
    let computed = SmallStep.eval(compiled, list{})
    assert (computed == BigStep.eval(src))
  }

  let basic_test = () => {
    let tests = [
      BigStep.Cst(42),
      Add(Cst(1), Cst(2)),
      Mul(Cst(1), Cst(2)),
      Add(Add(Cst(1), Cst(2)), Cst(3)),
      Mul(Mul(Cst(1), Cst(2)), Cst(3)),
      Add(Mul(Cst(1), Cst(2)), Cst(3)),
      Mul(Add(Cst(1), Cst(2)), Cst(3)),
    ]
    Belt.Array.forEachWithIndex(tests, (i, t) => {
      test_compile(t)
      let i = i + 1
      Js.log(j`test $i passed`)
    })
  }
}

Tests.basic_test()
