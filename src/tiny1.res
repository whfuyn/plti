open Belt

module BigStep = {
  type env = list<(string, int)>

  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(string)
    | Let(string, expr, expr)

  let rec eval = (expr, env) => {
    switch expr {
    | Cst(i) => i
    | Add(expr1, expr2) => eval(expr1, env) + eval(expr2, env)
    | Mul(expr1, expr2) => eval(expr1, env) * eval(expr2, env)
    | Var(name) => env->List.getAssoc(name, (a, b) => a == b)->Option.getExn
    | Let(name, let_expr, in_expr) => eval(in_expr, list{(name, eval(let_expr, env)), ...env})
    }
  }
}

module SmallStep = {
  type instr = Cst(int) | Add | Mul
  type instrs = list<instr>
  type operand = int
  type stack = list<operand>

  type env = list<(string, instrs)>

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

let rec compile = (src: BigStep.expr, env: SmallStep.env): SmallStep.instrs => {
  switch src {
  | Cst(i) => list{Cst(i)}
  | Add(expr1, expr2) => {
      let target1 = compile(expr1, env)
      let target2 = compile(expr2, env)
      Belt.List.concatMany([target1, target2, list{Add}])
    }

  | Mul(expr1, expr2) => {
      let target1 = compile(expr1, env)
      let target2 = compile(expr2, env)
      List.concatMany([target1, target2, list{Mul}])
    }

  | Var(name) => env->List.getAssoc(name, (a, b) => a == b)->Option.getExn

  | Let(name, let_expr, in_expr) => {
      let target = compile(let_expr, env)
      compile(in_expr, list{(name, target), ...env})
    }
  }
}

module Tests = {
  let test_compile = (src: BigStep.expr) => {
    let compiled = compile(src, list{})
    let computed = SmallStep.eval(compiled, list{})
    assert (computed == BigStep.eval(src, list{}))
  }

  let let_var_test = () => {
    let tests = [
      (BigStep.Let("a", Cst(5), Mul(Var("a"), Var("a"))), 25),
      (Let("a", Mul(Cst(5), Cst(5)), Mul(Var("a"), Var("a"))), 625),
      (Let("a", Cst(2), Let("b", Mul(Cst(5), Cst(5)), Mul(Var("b"), Var("a")))), 50),
    ]
    Js.log("let_var_test")
    tests->Array.forEachWithIndex((i, (t, res)) => {
      assert (BigStep.eval(t, list{}) == res)
      let i = i + 1
      Js.log(j`test $i passed`)
    })
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
      Let("a", Cst(5), Mul(Var("a"), Var("a"))),
      Let("a", Mul(Cst(5), Cst(5)), Mul(Var("a"), Var("a"))),
      Let("a", Cst(2), Let("b", Mul(Cst(5), Cst(5)), Mul(Var("b"), Var("a")))),
    ]
    Js.log("basic_test")
    tests->Array.forEachWithIndex((i, t) => {
      test_compile(t)
      let i = i + 1
      Js.log(j`test $i passed`)
    })
  }
}

Tests.let_var_test()
Tests.basic_test()
