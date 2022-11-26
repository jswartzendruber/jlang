main :: () {
     let a: i64 = fib(12);
     print_int(a);
}

fib :: (n : i64) -> i64 {
    if n == 0 || n == 1 {
       return n;
    }
    return fib(n - 1) + fib(n - 2);
}