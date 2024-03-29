/*
 * testing.i -
 *
 * Functions for testing Yorick.
 */

local test_npassed;
local test_nfailed;
func test_init(reset)
/* DOCUMENT test_init, reset;

     Initialize global variables for testing functions. If `reset` is true, all
     counters are cleared.

   SEE ALSO `test_assert`, `test_eval`, and `test_summary`.
 */
{
    extern test_npassed, test_nfailed;
    if (reset || is_void(test_npassed)) test_npassed = 0;
    if (reset || is_void(test_nfailed)) test_nfailed = 0;
}
test_init;

func test_summary(nil)
/* DOCUMENT test_summary;

     Print summary of all tests launched so far or until last `test_init, 1;`.

   SEE ALSO `test_assert`, `test_eval`, and `test_init`.
 */
{
    write, format="\nNumber of successful test(s) ---> %5d\n", test_npassed;
    write, format="Number of failures(s) ----------> %5d\n", test_nfailed;
}

func test_eval(expr)
/* DOCUMENT test_eval, expr;

     Evaluate textual expression to be tested in the current scope. The result
     of the expression must be true to be considered as successful. If the
     result of the expression is false, a message is printed and the error
     counter incremented.

   SEE ALSO `test_assert`, `test_init`, and `test_summary`.
 */
{
    local _test_result;
    include, ["_test_result = ("+expr+");"], 1;
    if (_test_result) {
        ++test_npassed;
    } else {
        ++test_nfailed;
        write, format="TEST FAILED: %s\n", expr;
    }
}
errs2caller, test_eval;

func test_assert(expr, a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
/* DOCUMENT test_assert, expr, mesg, ...;

     If `expr` is true, just increment the number of successful tests.
     Otherwise, increment the number of failed tests and print message `msg`.
     If subsequent arguments are provided, it is assumed that `msg` is a format
     string for these arguments.

   SEE ALSO `test_eval`, `test_init`, and `test_summary`.
 */
{
    if (expr) {
        ++test_npassed;
    } else {
        ++test_nfailed;
        if (is_void(a1)) {
            write, format="ASSERTION FAILED: %s\n", a0;
        } else {
            f =  "ASSERTION FAILED: " + a0 + "\n";
            if (is_void(a2)) {
                write, format=f, a1;
            } else if (is_void(a3)) {
                write, format=f, a1, a2;
            } else if (is_void(a4)) {
                write, format=f, a1, a2, a3;
            } else if (is_void(a5)) {
                write, format=f, a1, a2, a3, a4;
            } else if (is_void(a6)) {
                write, format=f, a1, a2, a3, a4, a5;
            } else if (is_void(a7)) {
                write, format=f, a1, a2, a3, a4, a5, a6;
            } else if (is_void(a8)) {
                write, format=f, a1, a2, a3, a4, a5, a6, a7;
            } else if (is_void(a9)) {
                write, format=f, a1, a2, a3, a4, a5, a6, a7, a8;
            } else {
                write, format=f, a1, a2, a3, a4, a5, a6, a7, a8, a9;
            }
        }
    }
}
errs2caller, test_assert;
