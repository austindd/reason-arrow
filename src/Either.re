/*
   This module is just for demonstration purposes. I recommend
   selecting an 'Either' data type from an established library.
 */
module Impl = {
  type t('a, 'b) =
    | Left('a)
    | Right('b);

  let left = x => Left(x);

  let right = x => Right(x);

  let mapLeft: (t('a, 'b), 'a => 'c) => t('c, 'b) =
    (either, aToC) =>
      switch (either) {
      | Left(a) => Left(aToC(a))
      | Right(c) => Right(c)
      };

  let mapRight: (t('a, 'b), 'b => 'c) => t('a, 'c) =
    (either, bToC) =>
      switch (either) {
      | Left(a) => Left(a)
      | Right(b) => Right(bToC(b))
      };

};

include Impl;
