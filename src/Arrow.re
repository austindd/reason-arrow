module type Intf = {
  type t(_) =
    | Func('a => 'b): t('a => 'b)
    | Pipe(t('a => 'b), t('b => 'c)): t('a => 'c);
  let identity: t('a => 'a);
  let pure: ('a => 'b) => t('a => 'b);
  let pipe: (t('a => 'b), t('b => 'c)) => t('a => 'c);
  let compose: (t('b => 'c), t('a => 'b)) => t('a => 'c);
  let eval: (t('a => 'b), 'a) => 'b;
  let map: (t('a => 'b), ('a => 'b, 'c) => 'd) => t('c => 'd);
  let apply: (t(('a => 'b, 'c) => 'd), t('a => 'b)) => t('c => 'd);
  let bind: (t('a => 'b), ('a => 'b) => t('c => 'd)) => t('c => 'd);
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

  let pure: ('a => 'b) => t('a => 'b) = f => Func(f);

  let pipe: (t('a => 'b), t('b => 'c)) => t('a => 'c) =
    (arrowF, arrowG) => Pipe(arrowF, arrowG);

  let compose: (t('b => 'c), t('a => 'b)) => t('a => 'c) =
    (arrowF, arrowG) => Pipe(arrowG, arrowF);

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
};

include Impl;
