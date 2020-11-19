let add = (a, b) => a + b;
let sub = (a, b) => a - b;
let mul = (a, b) => a * b;
let div = (a, b) => a / b;

// let myArrow = x => ((((x + 4) * 3) - 4) / 1);
let myArrow = Arrow.({
  pure(add(_, 4))
    ->pipeR(mul(_, 3))
    ->pipeR(sub(_, 4))
    ->pipeR(div(_, 1))
});

// ((((0 + 4) * 3) - 4) / 1) = 8
let result = Arrow.eval(myArrow, 0);

Js.log(result); // 8;

