main :: () {
     print_int(fib(10));
}

fib :: (n : i64) -> i64 {
    if n == 0 || n == 1 {
       return n;
    }
    return fib(n - 1) + fib(n - 2);
}