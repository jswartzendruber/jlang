enum ExitCode {
    Success := 0,
    IOError := 1,
    FatalError := 2,
}

main := () {
    printLine(ExitCode::Success) // Expect 0
}