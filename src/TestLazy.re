module Lazy = {
  type t('a) = Arrow2.t(unit, 'a);

  let pure: 'a => t('a) = a => Arrow2.pure(() => a);

  let defer: (unit => 'a) => t('a) = Arrow2.pure;

  let map: (t('a), 'a => 'b) => t('b) =
    (mA, aToB) => {
      Arrow2.Infix.(mA >>^ aToB);
    };

  let bind: (t('a), 'a => t('b)) => t('b) =
    (mA, aToMB) => {
      open! Arrow2;
      open! Arrow2.Infix;
      let arrowAb = Func(a => runF(aToMB(a), ()));
      Pipe(mA, arrowAb);
    };

  let eval: t('a) => 'a = Arrow2.runF(_, ());
};

let computation = {
  open! Lazy;
  let lazyVal =
    ref(
      defer(() => 0)
    );

  for (i in 1 to 1000000) {
    if (i mod 100000 === 0) {
      Js.log(i);
    };

    lazyVal :=
      (lazyVal^)
      ->map(n => n + 1)
      ->bind(n =>
          defer(() => {
            n + 1;
          })
        );
  };

  lazyVal^;
};

Js.log(computation);

let result = Lazy.eval(computation);

Js.log(result);
