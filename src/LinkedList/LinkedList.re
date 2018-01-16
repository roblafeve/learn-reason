type t('a) =
  | Empty
  | Head('a, t('a));

let make = (x: 'a) => Head(x, Empty);

let prepend = (x, y) => Head(x, y);

/*
 let length: list('a) => int;
 Return the length (number of elements) of the given list.
 */
let length = (ll: t('a)) : int => {
  let rec length_ = (a, i) =>
    switch a {
    | Empty => i
    | Head(_, y) => length_(y, i + 1)
    };
  length_(ll, 0);
};

/*
 let hd: list('a) => 'a;
 Return the first element of the given list. Raise Failure "hd" if the list is empty.
 */
let hd = (y: t('a)) =>
  switch y {
  | Empty => None
  | Head(x, _) => Some(x)
  };

/*
 let tl: list('a) => list('a);
 Return the given list without its first element. Raise Failure "tl" if the list is empty.
 */
let rec tl = (y: t('a)) =>
  switch y {
  | Empty => Empty
  | Head(x, Empty) => make(x)
  | Head(_, y) => tl(y)
  };

/*
 let nth: (list('a), int) => 'a;
 Return the n-th element of the given list. The first element (head of the list) is at position 0. Raise Failure "nth" if the list is too short. Raise Invalid_argument "List.nth" if n is negative.
 */
/*
 let rev: list('a) => list('a);
 List reversal.
 */
let rev = ll => {
  let rec rev_ = (ll1_, ll2_) =>
    switch ll1_ {
    | Empty => ll2_
    | Head(x, y) => rev_(y, prepend(x, ll2_))
    };
  rev_(ll, Empty);
};

/*
 let append: (list('a), list('a)) => list('a);
 Catenate two lists. Same function as the infix operator @. Not tail-recursive (length of the first argument). The @ operator is not tail-recursive either.
 */
/*
 let rev_append: (list('a), list('a)) => list('a);
 List.rev_append l1 l2 reverses l1 and concatenates it to l2. This is equivalent to List.rev l1 @ l2, but rev_append is tail-recursive and more efficient.
 */
/*
 let concat: list(list('a)) => list('a);
 Concatenate a list of lists. The elements of the argument are all concatenated together (in the same order) to give the result. Not tail-recursive (length of the argument + length of the longest sub-list).
 */
/*
 let flatten: list(list('a)) => list('a);
 Same as concat. Not tail-recursive (length of the argument + length of the longest sub-list).
 */
/*
 let map: ('a => 'b, list('a)) => list('b);
 List.map f [a1; ...; an] applies function f to a1, ..., an, and builds the list [f a1; ...; f an] with the results returned by f. Not tail-recursive.
 */
let map = (f: 'a => 'b, ll: t('a)) : t('b) => {
  let rec map_ = (f, ll1_, ll2_) =>
    switch ll1_ {
    | Empty => ll2_
    | Head(x, y) => map_(f, y, prepend(f(x), ll2_))
    };
  map_(f, ll, Empty) |> rev;
};