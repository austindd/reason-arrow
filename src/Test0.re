
let add = (a, b) => a + b;
let sub = (a, b) => a - b;

let rec fib = (n, cont) => {
  if (n < 3) {
    Arrow.eval(cont, 1);
  } else {
    Arrow.eval(cont, n - 1) + Arrow.eval(cont, n - 2);
  }
}
let fib = fib(_, Arrow.pure(fib(_, Arrow.identity)));

let a = fib(1);
let b = fib(4);
let c = fib(6);
let d = fib(10);

Js.log4(a, b, c, d);
