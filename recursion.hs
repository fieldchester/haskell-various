-- Simple Method for tail recursion
-- --------------------------------

-- 1) n is not increased, it stays constant as it "goes along".
-- 2) During recursion: Ingore the result at each step.
-- 3) At the end: Show the result of course.

-- Fibonachi
-- ---------
-- Gessel test: N is a Fibonacci number if and only if 5 N2 + 4 or 5 N2 – 4 is a square number.

-- sum 0 1 = 0 + 1 = 1
--   sum 1 1 = 1 + 1 = 2
--     sum 1 2 = 3
--       sum 2 3 = 5

-- Ignore the result
-- sum 0 1   sum 1 1   sum 1 2   sum 2 3

-- also to beginners@Haskell.org
fibu n = fibuHelp 0 1 n where
    fibuHelp x y n
        | y == n = x + y
        | y > n = error "not af fibunacci nubmer"
        | otherwise = fibuHelp y (x + y) n


-- Factorial
-- ---------
-- 0 prod 0 = 1
-- 1 prod 0 * 1 = 1
-- 2 prod 0 * 1 * 2 = 2
-- 3 prod 0 * 1 * 2 * 3 = 6
-- 4 prod 0 * 1 * 2 * 3 * 4 = 24
-- 5 prod 0 * 1 * 2 * 3 * 4 * 5 = 120

-- -1 
-- 0 prod -1 * 0 = 1

-- 1 prod 0 * 1 = 1
-- 2         prod 1 * 2 = 2
-- 3                prod  2 * 3 = 6
-- 4                         prod 6 * 4 = 24
-- 5                                 prod 24 * 5 = 120
-- 6                                          prod 120 * 6 = 720

---1/3
---1/3-3=1
-- -2    1*-2(=-3+1)=-2
-- -1           prod -2*-1(=-2+1)=1  : Nonsense
--  0                       prod  1*0(=-1+1)=1 <- !
--  1                                   prod 1* 1(=0+1) = 1
--  2                                                prod 1*2(=1+1)=2
--  3                                                         prod  2*3(=2+1)=6
--  4                                                                    prod 6*4(=3+1)=24
--  5                                                                              prod 24*5(=4+1)=120
--  6                                                                                         prod 120*6(=5+1)=720

-- Ignore the result
--                                      prod 1* (0+1) pr  1*(1+1) p 2*(2+1) p 6*(3+1)

factorial n = factorialHelp 1 0 n where
    factorialHelp x y n
        | n < 0 =     error "only positive whole numbers"
        | n == 1 =    1
        | y == n - 1 =    x * (y + 1)
        | otherwise = factorialHelp (x * (y + 1)) (y + 1) n
