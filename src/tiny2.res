// I suddenly notice that I'm using rust's naming convention, not rescript's.
// _(:з」∠)_

open Belt

exception Anyhow(string)

let panic = (msg: string) => {
  Js.log(j`\nPanic: $msg`)
  raise(Anyhow(msg))
}

let print_arr = (arr: array<int>) => {
  arr->Array.forEach(x => Js.log(x))
}

// `find` not found in Belt.List, so.
// Feels like Haskell.
let rec rfind = (ls: list<'a>, key: 'a): option<int> => {
  switch ls {
  | list{item, ...rest} =>
      // Later match first
      switch rest->rfind(key) {
      | Some(i) => Some(i + 1)
      | None if item == key => Some(0)
      | None => None
      }
  | list{} => None
  }
}


module Ast = {
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

module NameLess = {
  type env = list<int>
  type cenv = list<string>
  type stack = list<int>

  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)

  let rec eval = (expr: expr, env: env): int => {
    switch expr {
    | Cst(i) => i
    | Add(expr1, expr2) => eval(expr1, env) + eval(expr2, env)
    | Mul(expr1, expr2) => eval(expr1, env) * eval(expr2, env)
    | Var(i) => env->List.get(i)->Option.getExn
    | Let(let_expr, in_expr) => eval(in_expr, list{eval(let_expr, env), ...env})
    }
  }

  let rec from_ast = (src: Ast.expr, cenv: cenv): expr => {
    switch src {
    | Cst(i) => Cst(i)
    | Add(expr1, expr2) => Add(from_ast(expr1, cenv), from_ast(expr2, cenv))
    | Mul(expr1, expr2) => Mul(from_ast(expr1, cenv), from_ast(expr2, cenv))
    | Var(name) => Var(cenv->rfind(name)->Option.getExn)
    | Let(name, let_expr, in_expr) =>
      Let(from_ast(let_expr, cenv), from_ast(in_expr, List.concat(cenv, list{name})))
    }
  }

  let rec display = (expr): string => {
    switch expr {
      | Cst(i) => j`Cst($i)`
      | Add(expr1, expr2) => "Add(" ++ display(expr1) ++ ", " ++ display(expr2) ++ ")"
      | Mul(expr1, expr2) => "Mul(" ++ display(expr1) ++ ", " ++ display(expr2) ++ ")"
      | Var(i) => j`Var($i)`
      | Let(expr1, expr2) => "Let(" ++ display(expr1) ++ ", " ++ display(expr2) ++ ")"
    }
  }

  let print = (expr) => {
    let s = display(expr)
    Js.log(j`$s`)
  }
}

module VM = {
  type instr = Cst(int) | Add | Mul | Var(int) | Pop | Swap
  type instrs = list<instr>
  type operand = int
  type stack = list<operand>

  let display = (instr): string => {
    switch instr {
      | Cst(i) => j`Cst($i)`
      | Add => "Add"
      | Mul => "Mul"
      | Var(i) => j`Var($i)`
      | Pop => "Pop"
      | Swap => "Swap"
    }
  }

  let rec print = (instrs) => {
    switch instrs {
      |list{} => ()
      |list{hd, ...rest} => {
        let s = display(hd)
        Js.log(j`$s `)
        print(rest)
      }
    }
  }

  let rec eval = (instrs: instrs, stack: stack): int => {
    switch (instrs, stack) {
    | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stack})
    | (list{Add, ...rest}, list{a, b, ...stack}) => eval(rest, list{a + b, ...stack})
    | (list{Mul, ...rest}, list{a, b, ...stack}) => eval(rest, list{a * b, ...stack})
    | (list{Var(k), ...rest}, stack) => eval(rest, list{stack->List.getExn(k), ...stack})
    | (list{Pop, ...rest}, stack) => eval(rest, stack->List.drop(1)->Option.getExn)
    | (list{Swap, ...rest}, list{a, b, ...stack}) => eval(rest, list{b, a, ...stack})
    | (list{}, list{res, ..._}) => res
    | _ => panic("unexpected vm state")
    }
  }

  let alloc_tmp = (lvp) => {
    open Js.Array
    switch lvp->pop {
      | Some(record) => {
        let _ = push(record + 1, lvp)
      }
      | None => ()
    }
  }

  let rec from_nameless = (expr: NameLess.expr,  /* local variable position */ lvp: array<int>): instrs => {
    open Js.Array
    switch expr {
      | Cst(i) => list{Cst(i)}
      | Add(expr1, expr2) => {
        let instrs1 = from_nameless(expr1, lvp)
        alloc_tmp(lvp)
        let instrs2 = from_nameless(expr2, lvp)
        List.concatMany([instrs1, instrs2, list{Add}])
      }
      | Mul(expr1, expr2) => {
        let instrs1 = from_nameless(expr1, lvp)
        alloc_tmp(lvp)
        let instrs2 = from_nameless(expr2, lvp)
        List.concatMany([instrs1, instrs2, list{Mul}])
      }
      | Var(k) => {
        // Calculate the rfind of the refered local variable. e.g.
        // the lvp for stack:
        // [var_0, tmp, tmp, var_1, tmp, var_2]
        // is
        // [2, 1, 0]
        let pos = reduce(\"+", 0, sliceFrom(k, lvp))
        // Js.log("lvp")
        // print_arr(lvp)
        // Js.log("lvp done")
        // alloc_tmp(lvp)
        list{Var(pos)}
      }
      | Let(let_expr, in_expr) => {
        let let_instrs = from_nameless(let_expr, lvp)
        // Record a new local variable which is at the top of the stack (i.e. offset 0)
        let _ = push(0, lvp)
        let in_instrs = from_nameless(in_expr, lvp)
        let _ = pop(lvp)
        List.concatMany([let_instrs, in_instrs, list{Swap, Pop}])
      }
    }
  }
  let from_nameless = (expr) => from_nameless(expr, [])

  let rec from_ast = (src: Ast.expr, env: list<(string, instrs)>): instrs => {
    switch src {
    | Cst(i) => list{Cst(i)}
    | Add(expr1, expr2) => {
        let target1 = from_ast(expr1, env)
        let target2 = from_ast(expr2, env)
        List.concatMany([target1, target2, list{Add}])
      }
    | Mul(expr1, expr2) => {
        let target1 = from_ast(expr1, env)
        let target2 = from_ast(expr2, env)
        List.concatMany([target1, target2, list{Mul}])
      }
    | Var(name) => env->List.getAssoc(name, \"==")->Option.getExn
    | Let(name, let_expr, in_expr) => {
        let target = from_ast(let_expr, env)
        from_ast(in_expr, list{(name, target), ...env})
      }
    }
  }
}

module Tests = {
  let test_compile = (src: Ast.expr) => {
    let ast_instrs = VM.from_ast(src, list{})
    let ast_res = VM.eval(ast_instrs, list{})
    // Js.log("eval ok")

    let nameless = NameLess.from_ast(src, list{})
    // Js.log("from ast ok")
    // NameLess.print(nameless)
    let nameless_instrs = VM.from_nameless(nameless)
    // Js.log("from nameless ok")
    // VM.print(nameless_instrs)
    let nameless_res = VM.eval(nameless_instrs, list{})
    // Js.log("vm eval ok")
    let ans = Ast.eval(src, list{})
    // Js.log("ast eval ok")
    // Js.log(j`$ans $ast_res $nameless_res`)
    assert (ast_res == ans)
    assert (nameless_res == ans)
  }

  let test_rfind = () => {
    Js.log("test_rfind")
    let cenv = list{}
    assert (cenv->rfind("0") == None)
    let cenv = list{"0"}
    assert (cenv->rfind("0") == Some(0))
    let cenv = list{"0", "0"}
    assert (cenv->rfind("0") == Some(1))
    let cenv = list{"0", "1", "0", "3"}
    assert (cenv->rfind("0") == Some(2))
    let cenv = list{"0", "1", "2", "3"}
    assert (cenv->rfind("0") == Some(0))
    assert (cenv->rfind("2") == Some(2))
    assert (cenv->rfind("3") == Some(3))
    Js.log("passed")
  }

  let test_let_var = () => {
    let tests = [
      (Ast.Let("a", Cst(5), Mul(Var("a"), Var("a"))), 25),
      (Let("a", Mul(Cst(5), Cst(5)), Mul(Var("a"), Var("a"))), 625),
      (Let("a", Cst(2), Let("b", Mul(Cst(5), Cst(5)), Mul(Var("b"), Var("a")))), 50),
      (Let("a", Cst(1), Let("a", Cst(2), Var("a"))), 2),
      (Let("a", Let("a", Cst(1), Add(Var("a"), Var("a"))), Var("a")), 2),
    ]
    Js.log("test_let_var")
    tests->Array.forEachWithIndex((i, (t, res)) => {
      assert (Ast.eval(t, list{}) == res)
      let i = i + 1
      Js.log(j`test $i passed`)
    })
    Js.log("passed")
  }

  let test_eval_nameless = () => {
      let tests = [
        (NameLess.Cst(42), 42),
        (Add(Cst(2), Cst(3)), 5),
        (Mul(Cst(2), Cst(3)), 6),
        (Let(Cst(42), Var(0)), 42),
        (Let(Let(Cst(1), Add(Var(0), Var(0))), Var(0)), 2),
        (Let(Add(Cst(2), Cst(2)), Mul(Var(0), Var(0))), 16),
        (Let(Cst(1), Let(Cst(2), Let(Cst(3), Mul(Var(0), Add(Var(1), Var(2)))))), 9),
      ]
      Js.log("test_eval_nameless")
      tests->Array.forEachWithIndex((i, (t, res)) => {
        assert (NameLess.eval(t, list{}) == res)
        let i = i + 1
        Js.log(j`test $i passed`)
      })
      Js.log("passed")
  }

  let basic_test = () => {
    let tests = [
      Ast.Cst(42),
      Add(Cst(1), Cst(2)),
      Mul(Cst(1), Cst(2)),
      Add(Add(Cst(1), Cst(2)), Cst(3)),
      Mul(Mul(Cst(1), Cst(2)), Cst(3)),
      Add(Mul(Cst(1), Cst(2)), Cst(3)),
      Mul(Add(Cst(1), Cst(2)), Cst(3)),
      Let("a", Cst(5), Mul(Var("a"), Var("a"))),
      Let("a", Mul(Cst(5), Cst(5)), Mul(Var("a"), Var("a"))),
      Let("a", Cst(2), Let("b", Mul(Cst(5), Cst(5)), Mul(Var("b"), Var("a")))),
      Let("a", Cst(1), Let("a", Cst(2), Var("a"))),
      Let("a", Let("a", Cst(1), Add(Var("a"), Var("a"))), Var("a")),
    ]
    Js.log("basic_test")
    tests->Array.forEachWithIndex((i, t) => {
      test_compile(t)
      let i = i + 1
      Js.log(j`test $i passed`)
    })

    Js.log("passed")
  }

  let run_tests = () => {
    test_let_var()
    test_rfind()
    basic_test()
    test_eval_nameless()
  }
}


Tests.run_tests()
