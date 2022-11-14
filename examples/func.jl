main := () {
    result := square(2);
    printLine(result);
}

square := (i : i64) -> i64 {
    return i * i;
}