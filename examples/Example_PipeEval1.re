let add = (a, b) => a + b;
let sub = (a, b) => a - b;
let mul = (a, b) => a * b;
let div = (a, b) => a / b;

// myArrow: Arrow.t(int => int);
let myArrow = Arrow.({
  pure(add(_, 4))
    ->pipeR(mul(_, 3))
    ->pipeR(sub(_, 4))
    ->pipeR(div(_, 1))
});

// ((((0 + 4) * 3) - 4) / 1) = 8
let result = Arrow.runF(myArrow, 0);
