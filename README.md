# reason-arrow
Stack-safe function composition in ReasonML/ReScript

## Usage

Use `Arrow` to compose functions lazily, while also maintaining stack safety, no matter how many functions you compose.

```reason
let add = (a, b) => a + b;
let sub = (a, b) => a - b;
let mul = (a, b) => a * b;
let div = (a, b) => a / b;

// myArrow: Arrow.t(int, int);
let myArrow = Arrow.({
  pure(add(_, 4))
    ->pipeR(mul(_, 3))
    ->pipeR(sub(_, 4))
    ->pipeR(div(_, 1))
});

// ((((0 + 4) * 3) - 4) / 1) = 8
let result = Arrow.runF(myArrow, 0);
```

We get nice abstractions over lazy function composition. Here is an example of using `pipeR` with `map`:

```reason
let addFloat = (a, b) => a +. b;
let subFloat = (a, b) => a -. b;
let mulFloat = (a, b) => a *. b;
let divFloat = (a, b) => a /. b;

// useIntegersInstead: (float => float) => (int => int)
let useIntegersInstead = (fn, a) => fn(float_of_int(a))->int_of_float;

// myArrow: Arrow.t(int, int)
let myArrow = Arrow.({
  pure(addFloat(_, 4.))
    ->pipeR(mulFloat(_, 3.))
    ->pipeR(subFloat(_, 4.))
    ->pipeR(divFloat(_, 1.))
    ->map(_, useIntegersInstead(_))
});

let result = Arrow.runF(myArrow, 0); // 8
```
