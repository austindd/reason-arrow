let addFloat = (a, b) => a +. b;
let subFloat = (a, b) => a -. b;
let mulFloat = (a, b) => a *. b;
let divFloat = (a, b) => a /. b;

// useIntegersInstead: (float => float) => (int => int)
let useIntegersInstead = (fn, a) => fn(float_of_int(a))->int_of_float;

// myArrow: Arrow.t(int => int)
let myArrow = Arrow.({
  pure(addFloat(_, 4.))
    ->pipeR(mulFloat(_, 3.))
    ->pipeR(subFloat(_, 4.))
    ->pipeR(divFloat(_, 1.))
    ->map(_, useIntegersInstead(_))
});

let result = Arrow.eval(myArrow, 0); // 8

