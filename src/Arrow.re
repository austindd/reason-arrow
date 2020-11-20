module type Intf = {
  type t(_, _) =
    | Func('a => 'b): t('a, 'b)
    | Pipe(t('a, 'b), t('b, 'c)): t('a, 'c);

  /*
     [Arrow.identity] is the standard [a => a] (identity) function lifted
     into the [Arrow] context. Evaluating [Arrow.identity] with an argument
     of type [a] will return the exact same value [a].
   */
  let identity: t('a, 'a);

  /*
     [Arrow.returnA] is an alias of [Arrow.identity].
   */
  let returnA: t('a, 'a);

  /*
      [Arrow.runF] takes an [Arrow.t(a, b)] value (a data structure
      representing a series of composed functions) and evaluates it with
      an argument of type [a], returning a value of type [b]. This function
      is trampolined and fully tail-recursive, so it will be stack-safe as
      long as each of the internally composed functions are stack-safe.
   */
  let runF: (t('a, 'b), 'a) => 'b;

  let pipeR: (t('a, 'b), 'b => 'c) => t('a, 'c);
  let pipeL: ('a => 'b, t('b, 'c)) => t('a, 'c);
  let composeR: (t('b, 'c), 'a => 'b) => t('a, 'c);
  let composeL: ('b => 'c, t('a, 'b)) => t('a, 'c);

  /*
     [Arrow.pipe] takes an [Arrow.t(a , b)] and an [Arrow.t(b , c)]
     and composes them from left to right, returning a new [Arrow.t(a , c)].
   */
  let pipe: (t('a, 'b), t('b, 'c)) => t('a, 'c);

  /*
     [Arrow.compose] takes an [Arrow.t(b , c)] and an [Arrow.t(a , b)]
     and composes them from right to left, returning a new [Arrow.t(a , c)].
   */
  let compose: (t('b, 'c), t('a, 'b)) => t('a, 'c);

  let first: t('a, 'b) => t(('a, 'c), ('b, 'c));
  let second: t('a, 'b) => t(('x, 'a), ('x, 'b));
  let loop: (t(('a, 'c), ('b, 'c)), 'c) => t('a, 'b);
  let split: t('a, ('a, 'a));
  let unsplit: (('a, 'b) => 'c) => t(('a, 'b), 'c);

  let left: t('a, 'b) => t(Either.t('a, 'c), Either.t('b, 'c));
  let right: t('a, 'b) => t(Either.t('c, 'a), Either.t('c, 'b));
  let okChannel: t('a, 'b) => t(result('a, 'err), result('b, 'err));
  let errorChannel: t('a, 'b) => t(result('ok, 'a), result('ok, 'b));
  let zip: (t('a, 'b), t('c, 'd)) => t(('a, 'c), ('b, 'd));

  /*
     [Arrow.arrow] takes a function of type [a => b] and lifts it into the
     [Arrow] context, returning an [Arrow.t(a , b)].
   */
  let arrow: ('a => 'b) => t('a, 'b);

  /*
     [Arrow.pure] is an alias of [Arrow.arrow].
   */
  let pure: ('a => 'b) => t('a, 'b);

  /*
     [Arrow.map] takes an [Arrow.t('a , 'b)] along with a higher-order
     "mapping" function that transforms [a => b] into [c => d], and returns
     a new [Arrow.t(c , d)].

     This is useful when you want to lazily convert the function inside the
     [Arrow] context into a different function, while remaining within
     the [Arrow] context.
   */
  let map: (t('a, 'b), ('a => 'b, 'c) => 'd) => t('c, 'd);
  let apply: (t('a => 'b, 'c => 'd), t('a, 'b)) => t('c, 'd);
  let join: (t('a, t('b, 'c)), 'a) => t('b, 'c);
  let bind: (t('a, 'b), ('a => 'b) => t('c, 'd)) => t('c, 'd);
  let lift: (('a => 'b, 'c) => 'd, t('a, 'b)) => t('c, 'd);
  let lift2:
    (('a => 'b, 'c => 'd, 'e) => 'f, t('a, 'b), t('c, 'd)) => t('e, 'f);

  module Infix: {
    let (>>^): (t('a, 'b), 'b => 'c) => t('a, 'c);
    let (^>>): ('a => 'b, t('b, 'c)) => t('a, 'c);
    let (<<^): (t('b, 'c), 'a => 'b) => t('a, 'c);
    let (^<<): ('b => 'c, t('a, 'b)) => t('a, 'c);
    let (>>>): (t('a, 'b), t('b, 'c)) => t('a, 'c);
    let (<<<): (t('b, 'c), t('a, 'b)) => t('a, 'c);
    let (<$>): (t('a, 'b), ('a => 'b, 'c) => 'd) => t('c, 'd);
    let (<*>): (t('a => 'b, 'c => 'd), t('a, 'b)) => t('c, 'd);
    let (>>=): (t('a, 'b), ('a => 'b) => t('c, 'd)) => t('c, 'd);
  };
};

module Impl: Intf = {
  external identity: 'a => 'a = "%identity";

  type t(_, _) =
    | Func('a => 'b): t('a, 'b)
    | Pipe(t('a, 'b), t('b, 'c)): t('a, 'c);

  let identity = Func(identity);
  let returnA = identity;

  let runF: (t('a, 'b), 'a) => 'b =
    (__arrow, __arg) => {
      let rec loop: type a b c. (a, t(b, c), t(a, b)) => c =
        (acc, stack, arrow) => {
          switch (arrow) {
          | Func(f1) =>
            let result = f1(acc);
            switch (stack) {
            | Func(ret) => ret(result)
            | Pipe(arrow2, next) => loop(result, next, arrow2)
            };
          | Pipe(arrowL1, arrowR1) =>
            loop(acc, Pipe(arrowR1, stack), arrowL1)
          };
        };
      loop(__arg, identity, __arrow);
    };

  let pipeR: (t('a, 'b), 'b => 'c) => t('a, 'c) =
    (arrow_ab, bc) => Pipe(arrow_ab, Func(bc));

  let pipeL: ('a => 'b, t('b, 'c)) => t('a, 'c) =
    (ab, arrow_bc) => Pipe(Func(ab), arrow_bc);

  let composeR: (t('b, 'c), 'a => 'b) => t('a, 'c) =
    (arrow_bc, ab) => Pipe(Func(ab), arrow_bc);

  let composeL: ('b => 'c, t('a, 'b)) => t('a, 'c) =
    (bc, arrow_ab) => Pipe(arrow_ab, Func(bc));

  let pipe: (t('a, 'b), t('b, 'c)) => t('a, 'c) =
    (arrow_ab, arrow_bc) => Pipe(arrow_ab, arrow_bc);

  let compose: (t('b, 'c), t('a, 'b)) => t('a, 'c) =
    (arrow_bc, arrow_ab) => pipe(arrow_ab, arrow_bc);

  let first: t('a, 'b) => t(('a, 'x), ('b, 'x)) =
    arrow_ab => Func(((a, x)) => (runF(arrow_ab, a), x));

  let second: t('a, 'b) => t(('x, 'a), ('x, 'b)) =
    arrow_ab => Func(((x, a)) => (x, runF(arrow_ab, a)));

  let split: t('a, ('a, 'a)) = Func(x => (x, x));

  let unsplit: type a b c. ((a, b) => c) => t((a, b), c) =
    ab_c => Func(((a, b)) => ab_c(a, b));

  let left: type a b c. t(a, b) => t(Either.t(a, c), Either.t(b, c)) =
    arrowAb =>
      Func(
        eitherAc =>
          switch (eitherAc) {
          | Left(a) => Left(runF(arrowAb, a))
          | Right(r) => Right(r)
          },
      );

  let right: type a b c. t(a, b) => t(Either.t(c, a), Either.t(c, b)) =
    arrowAb =>
      Func(
        eitherAc =>
          switch (eitherAc) {
          | Left(l) => Left(l)
          | Right(a) => Right(runF(arrowAb, a))
          },
      );

  let okChannel: type a b err. t(a, b) => t(result(a, err), result(b, err)) =
    arrowAb =>
      Func(
        resultAc =>
          switch (resultAc) {
          | Ok(a) => Ok(runF(arrowAb, a))
          | Error(err) => Error(err)
          },
      );

  let errorChannel: type a b ok. t(a, b) => t(result(ok, a), result(ok, b)) =
    arrowAb =>
      Func(
        resultAc =>
          switch (resultAc) {
          | Ok(ok) => Ok(ok)
          | Error(a) => Error(runF(arrowAb, a))
          },
      );

  let zip: (t('a, 'b), t('c, 'd)) => t(('a, 'c), ('b, 'd)) =
    (arrowAb, arrowCd) => {
      let ab = runF(arrowAb);
      let cd = runF(arrowCd);
      let f = ((a, c)) => (ab(a), cd(c));
      Func(f);
    };

  let loop: type a b c. (t((a, c), (b, c)), c) => t(a, b) =
    (arrow_ac_bc, c) => {
      let ac_bc: ((a, c)) => (b, c) = runF(arrow_ac_bc);
      Func(
        (a: a) => {
          let (b, _c) = ac_bc((a, c));
          b;
        },
      );
    };

  let pure: ('a => 'b) => t('a, 'b) = f => Func(f);

  let arrow = pure;

  let join: (t('a, t('b, 'c)), 'a) => t('b, 'c) = runF;

  let map: type a b c d. (t(a, b), (a => b, c) => d) => t(c, d) =
    (arrow_ab: t(a, b), ab_cd: (a => b, c) => d) => {
      let ab = runF(arrow_ab);
      let cd = ab_cd(ab);
      Func(cd);
    };

  let apply: (t('a => 'b, 'c => 'd), t('a, 'b)) => t('c, 'd) =
    (arrow_ab_cd, arrow_ab) => {
      let ab_cd = runF(arrow_ab_cd);
      let ab = runF(arrow_ab);
      let cd = ab_cd(ab);
      Func(cd);
    };

  let bind: (t('a, 'b), ('a => 'b) => t('c, 'd)) => t('c, 'd) =
    (arrowAb, ab_arrowCd) => {
      let ab = runF(arrowAb);
      let arrowCd = ab_arrowCd(ab);
      arrowCd;
    };

  let lift: (('a => 'b, 'c) => 'd, t('a, 'b)) => t('c, 'd) =
    (fToG, arrowF) => Func(arg => fToG(runF(arrowF), arg));

  let lift2:
    (('a => 'b, 'c => 'd, 'e) => 'f, t('a, 'b), t('c, 'd)) => t('e, 'f) =
    (
      fToGToH: ('a => 'b, 'c => 'd, 'e) => 'f,
      arrowF: t('a, 'b),
      arrowG: t('c, 'd),
    ) =>
      Func(arg => fToGToH(runF(arrowF), runF(arrowG), arg));

  module Infix = {
    let (^>>) = pipeL;
    let (>>^) = pipeR;
    let (^<<) = composeL;
    let (<<^) = composeR;
    let (>>>) = pipe;
    let (<<<) = compose;
    let (<$>) = map;
    let (<*>) = apply;
    let (>>=) = bind;
  };
};

include Impl;
