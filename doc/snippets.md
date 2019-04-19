# Snippets

Usage: type one of the following trigger keywords, and type <kbd>C-c y</kbd>.

## C++ mode

Keyword         | Description
----------------|-----------------------------------------------------------
assert          | `BSLS_ASSERT_OPT`
cat             | `BALL_LOG_SET_CATEGORY`
debug           | `BALL_LOG_DEBUG`
error           | `BALL_LOG_ERROR`
info            | `BALL_LOG_INFO`
trace           | `BALL_LOG_TRACE`
warn            | `BALL_LOG_WARN`
bind            | `bdef_BindUtil::bind`
cerr            | `std::cerr`
cout            | `std::cout`
main            | main function
now             | `bslmt::SystemTime::nowMonotonicClock`
return          | return statement
sleep           | `bslmt::ThreadUtil::sleep`
lock            | lock guard
unlock          | release a lock guard

File templates:

Keyword         | Description
----------------|-----------------------------------------------------------
.h              | header file template
.cpp            | cpp file template
.t.cpp          | test driver file template
