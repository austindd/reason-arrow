module type Intf = {
  type t(_) =
    | Func('a => 'b): t('a => 'b)
    | Pipe(t('a => 'b), t('b => 'c)): t('a => 'c);

  /*
      [Arrow.eval] takes an [Arrow.t(a => b)] value (a data structure
      representing a series of composed functions) and evaluates it with
      an argument of type [a], returning a value of type [b]. This function
      is trampolined and fully tail-recursive, so it will be stack-safe as
      long as each of the internally composed functions are stack-safe.
   */

  let eval: (t('a => 'b), 'a) => 'b;
  /*
     [Arrow.identity] is the standard [identity] function lifted into the
     [Arrow] context. Evaluating [Arrow.identity] with an argument of type
     [a] will return the exact same value [a].
   */
  let identity: t('a => 'a);

  /*
     [Arrow.pipe] takes an [Arrow.t(a => b)] value and a second function
     of type [b => c], returning a new [Arrow.t(a => c)].

     Running [eval(pipe(pure(aB), bC), a)] is the same as running
     [bC(aB(a))].
   */
  let pipe: (t('a => 'b), 'b => 'c) => t('a => 'c);

  /*
     [Arrow.compose] takes an [Arrow.t(b => c)] value and a second function
     of type [a => b], returning a new [Arrow.t(a => c)].

     Running [eval(compose(pure(bC), aB), a)] is the same as running
     [bC(aB(a))].

     E.g.
   */
  let compose: (t('b => 'c), 'a => 'b) => t('a => 'c);

  /*
     [Arrow.concat] takes an [Arrow.t(a => b)] and an [Arrow.t(b => c)]
     and composes them from left to right, returning a new [Arrow.t(a => c)].
   */
  let concat: (t('a => 'b), t('b => 'c)) => t('a => 'c);

  /*
     [Arrow.merge] is an alias for [Arrow.concat].
   */
  let merge: (t('a => 'b), t('b => 'c)) => t('a => 'c);

  /*
     [Arrow.pure] takes a function of type [a => b] and lifts it into the
     [Arrow] context, returning an [Arrow.t(a => b)].
   */
  let pure: ('a => 'b) => t('a => 'b);

  /*
     [Arrow.map] takes an [Arrow.t('a => 'b)] along with a higher-order
     "mapping" function that transforms [a => b] into [c => d], and returns
     a new [Arrow.t(c => d)].

     This is useful when you want to lazily convert the function inside the
     [Arrow] context into a different function, while remaining within
     the [Arrow] context.
   */
  let map: (t('a => 'b), ('a => 'b, 'c) => 'd) => t('c => 'd);
  let apply: (t(('a => 'b, 'c) => 'd), t('a => 'b)) => t('c => 'd);
  let join: (t('a => t('b => 'c)), 'a) => t('b => 'c);
  let bind: (t('a => 'b), ('a => 'b) => t('c => 'd)) => t('c => 'd);
  let lift: (('a => 'b, 'c) => 'd, t('a => 'b)) => t('c => 'd);
  let lift2:
    (('a => 'b, 'c => 'd, 'e) => 'f, t('a => 'b), t('c => 'd)) =>
    t('e => 'f);
};

module Impl: Intf = {
  external identity: 'a => 'a = "%identity";

  type t(_) =
    | Func('a => 'b): t('a => 'b)
    | Pipe(t('a => 'b), t('b => 'c)): t('a => 'c);

  let identity = Func(identity);

  let eval = (__arrow, __arg) => {
    let rec loop: type a b c. (a, t(b => c), t(a => b)) => c =
      (acc, stack, arrow) => {
        switch (arrow) {
        | Func(f1) =>
          let result = f1(acc);
          switch (stack) {
          | Func(ret) => ret(result)
          | Pipe(arrow2, next) => loop(result, next, arrow2)
          };
        | Pipe(arrowL1, arrowR1) => loop(acc, Pipe(arrowR1, stack), arrowL1)
        };
      };
    loop(__arg, identity, __arrow);
  };

  let pipe: (t('a => 'b), 'b => 'c) => t('a => 'c) =
    (arrowF, g) => Pipe(arrowF, Func(g));

  let compose: (t('b => 'c), 'a => 'b) => t('a => 'c) =
    (arrowG, f) => Pipe(Func(f), arrowG);

  let concat: (t('a => 'b), t('b => 'c)) => t('a => 'c) =
    (arrowF, arrowG) => Pipe(arrowF, arrowG);

  let merge = concat;

  let pure: ('a => 'b) => t('a => 'b) = f => Func(f);

  let join: (t('a => t('b => 'c)), 'a) => t('b => 'c) = eval;

  let map: (t('a => 'b), ('a => 'b, 'c) => 'd) => t('c => 'd) =
    (arrowF: t('a => 'b), fToG: ('a => 'b, 'c) => 'd) => {
      let f = eval(arrowF);
      let g = fToG(f);
      Func(g);
    };

  let apply: (t(('a => 'b, 'c) => 'd), t('a => 'b)) => t('c => 'd) =
    (arrowFF, arrowG) => {
      let ff = eval(arrowFF);
      let g = eval(arrowG);
      Func(ff(g));
    };

  let bind: (t('a => 'b), ('a => 'b) => t('c => 'd)) => t('c => 'd) =
    (arrowF, fToArrowG) => {
      let f = eval(arrowF);
      let arrowG = fToArrowG(f);
      let g = eval(arrowG);
      Func(g);
    };

  let lift: (('a => 'b, 'c) => 'd, t('a => 'b)) => t('c => 'd) =
    (fToG, arrowF) => Func(arg => fToG(eval(arrowF), arg));

  let lift2:
    (('a => 'b, 'c => 'd, 'e) => 'f, t('a => 'b), t('c => 'd)) =>
    t('e => 'f) =
    (
      fToGToH: ('a => 'b, 'c => 'd, 'e) => 'f,
      arrowF: t('a => 'b),
      arrowG: t('c => 'd),
    ) => {
      let arrowH = Func(arg => fToGToH(eval(arrowF), eval(arrowG), arg));
      arrowH;
    };
};

include Impl;

let add = (a, b) => a + b;
