// I suddenly notice that I'm using rust's naming convention, not rescript's.
// _(:з」∠)_

open Belt

exception Anyhow(string)

let panic = (msg: string) => {
  Js.log(j`\nPanic: $msg`)
  raise(Anyhow(msg))
}

module BigStep = {
  type env = list<(string, int)>

  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(string)
    | Let(string, expr, expr)

  let rec eval = (expr, env): int => {
    switch expr {
    | Cst(i) => i
    | Add(expr1, expr2) => eval(expr1, env) + eval(expr2, env)
    | Mul(expr1, expr2) => eval(expr1, env) * eval(expr2, env)
    // The use of \"==" is a bit of hacky. Prefer to use `(a, b) => a == b` instead.
    | Var(name) => env->List.getAssoc(name, \"==")->Option.getExn
    | Let(name, let_expr, in_expr) => eval(in_expr, list{(name, eval(let_expr, env)), ...env})
    }
  }
}

module SmallStep = {
  type instr = Cst(int) | Add | Mul | Var(int) | Pop | Swap
  type instrs = list<instr>
  type operand = int
  type stack = list<operand>

  type env = list<(string, instrs)>

  let rec eval = (instrs: instrs, stack: stack): int => {
    switch (instrs, stack) {
    | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stack})
    | (list{Add, ...rest}, list{a, b, ...stack}) => eval(rest, list{a + b, b, ...stack})
    | (list{Mul, ...rest}, list{a, b, ...stack}) => eval(rest, list{a * b, b, ...stack})
    | (list{Var(k), ...rest}, stack) => eval(rest, list{stack->List.getExn(k), ...stack})
    | (list{Pop, ...rest}, stack) => eval(rest, stack->List.drop(1)->Option.getExn)
    | (list{Swap, ...rest}, list{a, b, ...stack}) => eval(rest, list{b, a, ...stack})
    | (list{}, list{res, ..._}) => res
    | _ => panic("unexpected vm state")
    }
  }
}

let rec compile_direct = (src: BigStep.expr, env: SmallStep.env): SmallStep.instrs => {
  switch src {
  | Cst(i) => list{Cst(i)}
  | Add(expr1, expr2) => {
      let target1 = compile_direct(expr1, env)
      let target2 = compile_direct(expr2, env)
      Belt.List.concatMany([target1, target2, list{Add, Swap, Pop}])
    }

  | Mul(expr1, expr2) => {
      let target1 = compile_direct(expr1, env)
      let target2 = compile_direct(expr2, env)
      List.concatMany([target1, target2, list{Mul, Swap, Pop}])
    }

  | Var(name) => env->List.getAssoc(name, \"==")->Option.getExn

  | Let(name, let_expr, in_expr) => {
      let target = compile_direct(let_expr, env)
      compile_direct(in_expr, list{(name, target), ...env})
    }
  }
}

module NameLess = {
  type cenv = list<string>
  type stack = list<int>

  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)

  // `find` not found in Belt.List, so.
  // Feels like Haskell.
  let rec find_index = (cenv: cenv, key: string): option<int> => {
    switch cenv {
    | list{item, ...rest} =>
      if item == key {
        Some(0)
      } else {
        switch rest->find_index(key) {
        | Some(i) => Some(i + 1)
        | None => None
        }
      }
    | list{} => None
    }
  }

  let rec to_nameless = (src: BigStep.expr, cenv: cenv): expr => {
    switch src {
    | Cst(i) => Cst(i)
    | Add(expr1, expr2) => Add(to_nameless(expr1, cenv), to_nameless(expr2, cenv->List.add("#STACK_VAR")))
    | Mul(expr1, expr2) => Mul(to_nameless(expr1, cenv), to_nameless(expr2, cenv->List.add("#STACK_VAR")))
    | Var(name) => Var(cenv->find_index(name)->Option.getExn)
    | Let(name, let_expr, in_expr) =>
      Let(to_nameless(let_expr, cenv), to_nameless(in_expr, cenv->List.add(name)))
    }
  }

  let rec compile_nameless = (src: expr): SmallStep.instrs => {
    switch src {
    | Cst(i) => list{Cst(i)}
    | Add(expr1, expr2) => {
        let target1 = compile_nameless(expr1)
        let target2 = compile_nameless(expr2)
        Belt.List.concatMany([target1, target2, list{Add, Swap, Pop}])
      }

    | Mul(expr1, expr2) => {
        let target1 = compile_nameless(expr1)
        let target2 = compile_nameless(expr2)
        Belt.List.concatMany([target1, target2, list{Mul, Swap, Pop}])
      }

    | Var(k) => list{Var(k)}

    | Let(expr1, expr2) => List.concatMany([compile_nameless(expr1), compile_nameless(expr2)])
    }
  }
}
module Tests = {
  let test_compile = (src: BigStep.expr) => {
    let instrs1 = compile_direct(src, list{})
    let computed1 = SmallStep.eval(instrs1, list{})

    let nameless = NameLess.to_nameless(src, list{})
    let instrs2 = NameLess.compile_nameless(nameless)
    let computed2 = SmallStep.eval(instrs2, list{})
    // Js.log(j`$computed1 $computed2`)
    assert (computed1 == BigStep.eval(src, list{}))
    assert (computed2 == BigStep.eval(src, list{}))
    assert (computed1 == computed2)
  }

  let find_index_test = () => {
    Js.log("find_index_test")
    let cenv = list{}
    assert (cenv->NameLess.find_index("0") == None)
    let cenv = list{"0"}
    assert (cenv->NameLess.find_index("0") == Some(0))
    let cenv = list{"0", "1", "2", "3"}
    assert (cenv->NameLess.find_index("0") == Some(0))
    assert (cenv->NameLess.find_index("2") == Some(2))
    assert (cenv->NameLess.find_index("3") == Some(3))
    Js.log("passed")
  }

  let let_var_test = () => {
    let tests = [
      (BigStep.Let("a", Cst(5), Mul(Var("a"), Var("a"))), 25),
      (Let("a", Mul(Cst(5), Cst(5)), Mul(Var("a"), Var("a"))), 625),
      (Let("a", Cst(2), Let("b", Mul(Cst(5), Cst(5)), Mul(Var("b"), Var("a")))), 50),
      (Let("a", Cst(1), Let("a", Cst(2), Var("a"))), 2),
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
      // ^
      // | This it's a debug scratch of the previous test, it output 625 instead of 50
      // before I fix it with the `#STACK_VAR` padding.
      // Let(Cst(2), Let(Mul(Cst(5), Cst(5)), Mul(Var(0), Var(1))))))  <- nameless
      // [2]  <- stack state
      // [25, 2]
      // [25, 25, 2]
      // [25, 25, 2]
      // [25, 25, 25, 2]
      Let("a", Cst(1), Let("a", Cst(2), Var("a"))),
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
Tests.find_index_test()
Tests.basic_test()
