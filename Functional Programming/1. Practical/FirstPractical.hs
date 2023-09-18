{- Practical 1: Factoring Numbers

Here is a simple method for finding the smallest prime factor of a positive
integer: -}

factor :: Integer -> (Integer, Integer)
factor n = factorFrom 2 n

factorFrom :: Integer -> Integer -> (Integer, Integer)
factorFrom m n  | r == 0    = (m,q)
                | otherwise = factorFrom (m+1) n
    where (q,r) = n `divMod` m

{- for example

*Main> factor 7654321
(19,402859)

because 

*Main> 19 * 402859
7654321

Repeatedly extracting the smallest factor will return a list
of prime factors: 
-}





{- for example *Main> factor 123456789
(3,41152263)
*Main> factors 123456789
[3,3,3607,3803]

Exercise 1:  factor 0 = (2,0) because it goes to factorFrom 2 0 and (q,r)=(0,0) as r=0 it returns (m,q)=(2,0)
		factor 1 runs forever because there are not exist any number divides 1 and greater than 2
Exercise 2: I checked my answer and its true.
Exercise 3: if d is a factor of n then n/d is also factor of n. Suppose that d is the smallest factor of n and bigger than sqrt(n) and less than n. Then n/d is also factor of n and n/d is bigger than 1 and less than sqrt(n). This is contradicts with assumption. Therefore, such d is not exist.
-}

factor1 :: Integer -> (Integer, Integer)
factor1 n = factorFrom1 2 n

factorFrom1 :: Integer -> Integer -> (Integer, Integer)
factorFrom1 m n | r == 0    = (m,q)
                | n<=m*m    = (n,1)
                | otherwise = factorFrom1 (m+1) n
    where (q,r) = n `divMod` m

{- It matters in numbers that are square of prime numbers. For example, Let consider the case n=4:
if r == 0    = (m,q) is the first one then it returns (2,2) however 
if | n<=m*m    = (n,1) is the first one it returns (4,1) .
In the worst case, it can go from 2 to ceil (sqrt(n)) one by one so approximately there are sqrt(n) recursive calls in the worst case.

Exercise 4: n<m*m equivalent to q<m as q=floor(n/m)<n/m<m and if q<m then n=q*m+r<m(m-1)+m=m*m. And, if n=m*m function drops to first guard.
Because calculating m*m take more time. 
-}
factor2 :: Integer -> (Integer, Integer)
factor2 n = factorFrom2 2 n

factorFrom2 :: Integer -> Integer -> (Integer, Integer)
factorFrom2 m n | r == 0    = (m,q)
                | q < m    = (n,1)
                | otherwise = factorFrom2 (m+1) n
    where (q,r) = n `divMod` m
    
{- Exercise5: It need to take half of the cases so it is two times faster than factor2 
-}

factor3 :: Integer -> (Integer, Integer)
factor3 n = factorFrom3 2 n

factorFrom3 :: Integer -> Integer -> (Integer, Integer)
factorFrom3 m n | r == 0    = (m,q)
                | q < m    = (n,1)
                | m==2     = factorFrom3 3 n
                | otherwise = factorFrom3 (m+2) n
                
    where (q,r) = n `divMod` m
    
{- Exercise 6: 
Some test results: 

Test 1: 
factor3 531106861
(22123,24007)
(0.01 secs, 5,206,896 bytes)
Test 2: 
factor3 28934620927
(161053,179659)
(0.07 secs, 37,440,128 bytes)
Test 3:
factor3 403236371179
(511583,788213)
(0.20 secs, 118,763,088 bytes)

-}
factor4 :: Integer -> (Integer, Integer)
factor4 n = factorFrom4 2 n 2

factorFrom4 :: Integer -> Integer -> Integer -> (Integer, Integer)
factorFrom4 m n s | r == 0    = (m,q)
                  | q < m    = (n,1)
                  | m==2     = factorFrom4 3 n s
                  | m==3     = factorFrom4 5 n s
                  | otherwise = factorFrom4 (m+s) n (6-s)
                
    where (q,r) = n `divMod` m
{- Exercise7: 
Some test results: 
Test1: 
factor4 531106861
(22123,24007)
(0.01 secs, 4,440,328 bytes)

Test2: 
28934620927
(161053,179659)
(0.06 secs, 31,857,320 bytes)

Test 3: 
factor4 403236371179
(511583,788213)
(0.16 secs, 101,028,376 bytes)

Exercise8: Determining whether a number is prime is also take time as much as the factor function. Therefore using only prime numbers also take a lot of time.
-}
factors :: Integer -> [Integer]
factors n = factorsFrom 2 n

factorsFrom :: Integer -> Integer -> [Integer]
factorsFrom m n | n == 1    = []
                | otherwise = p:factorsFrom p q
   where (p,q) = factorFrom m n
{- Exercise9: -}  
factors2 :: Integer -> [Integer]
factors2 n = factorsFrom2 2 n

factorsFrom2 :: Integer -> Integer -> [Integer]
factorsFrom2 m n | n == 1    = []
                 | otherwise = p:factorsFrom2 p q
   where (p,q) = factorFrom4 m n (if m `mod` 6 == 1 then 4 else 2)
{- Exercise10: 

factors 8616460799
[89681,96079]
(0.05 secs, 34,657,320 bytes)

factors2 8616460799
[89681,96079]
(0.03 secs, 17,766,784 bytes)

factors 68491077245397791
[157483,458959,947603]
(0.44 secs, 341,213,256 bytes)

factors2 68491077245397791
[157483,458959,947603]
(0.16 secs, 90,645,288 bytes)

factors 47258036049431723
[89653,426787,1235093]
(0.56 secs, 444,709,656 bytes)

factors2 47258036049431723
[89653,426787,1235093]
(0.14 secs, 84,296,680 bytes)


Exercise 11 : if r<0 then decrease y by one. do it until r>=0 if r will become 0 its done if r>0 then there is not solution for x=p . So narrowed x>=p+1 .
if r>0 then increase y by one. do it until r<=0 if r will become 0 its done if r<0 then there is not solution for x=p. So narrowed x>=p+1 .

(n+1)^2-n^2=2*n+1 so there exist (p,q) such that p^2-q^2=2*n+1
-}
searchNegative :: Integer -> Integer -> Integer -> (Integer,Integer)
searchNegative p q n | r==0 = (p,q)
		     | q==0 = search (p+1) q n 
		     | r<0 = searchNegative p (q-1) n
		     | r>0 = search (p+1) q n 
	where r = p*p-q*q-n

searchPositive :: Integer -> Integer -> Integer -> (Integer,Integer)
searchPositive  p q n | r==0 = (p,q)
		      | r>0 = searchPositive p (q+1) n
		      | r<0 = search (p+1) q n 
	where r = p*p-q*q-n


search:: Integer -> Integer -> Integer -> (Integer,Integer)
search p q n | r==0 = (p,q)
	     | r< 0 = searchNegative p q n 
	     | r> 0 = searchPositive p q n 
   where r = p*p-q*q-n
   
fermat:: Integer -> (Integer,Integer)
fermat n = ((u+v),(u-v))
 where (u,v) =search (isqrt(n)) 0 n
 
 {- Exercise 12:  the smallest possible number of x is ceil of square root of n as x*x=y*y+n>n. 
 -}
isqrt :: Integer -> Integer
isqrt = truncate . sqrt . fromInteger

 {- Exercise 13:
 fermat 8616460799
(96079,89681)
(0.01 secs, 1,686,656 bytes)

fermat 1963272347809
(8123471,241679)
(7.37 secs, 4,749,137,600 bytes)

 -}
  {- Exercise 14:
fermat2 8616460799
(96079,89681)
(0.01 secs, 1,595,944 bytes)

fermat2  1963272347809
(8123471,241679)
(5.37 secs, 3,920,265,464 bytes)
  
 -}
searchNegative2 :: Integer -> Integer -> Integer -> Integer -> (Integer,Integer)
searchNegative2 p q n r | r==0 = (p,q)
		        | q==0 = search2 (p+1) q n (r+2*p+1)
		        | r<0 = searchNegative2 p (q-1) n (r+2*q-1)
		        | r>0 = search2 (p+1) q n (r+2*p+1)

searchPositive2 :: Integer -> Integer -> Integer -> Integer -> (Integer,Integer)
searchPositive2  p q n r | r==0 = (p,q)
		         | r>0 = searchPositive2 p (q+1) n (r-2*q-1)
		         | r<0 = search2 (p+1) q n (r+2*p+1)

search2:: Integer -> Integer -> Integer -> Integer -> (Integer,Integer)
search2 p q n r | r==0 = (p,q)
	        | r< 0 = searchNegative2 p q n r
	        | r> 0 = searchPositive2 p q n r
	        
fermat2:: Integer -> (Integer,Integer)
fermat2 n = ((u+v),(u-v))
 where (u,v) =search2 (isqrt(n)+1) 0 n ((isqrt(n)+1)*(isqrt(n)+1)-n)
 
   {- Exercise 15:
sqrtlineer 8616460799
92824
(0.08 secs, 48,330,856 bytes)

sqrtlineer 1963272347809
1401168
(1.11 secs, 728,671,208 bytes)

 -}
sqrtlineerhelper :: Integer -> Integer -> Integer
sqrtlineerhelper n q | ( q*q<=n && (q+1)*(q+1) >n ) = q
                     | otherwise = sqrtlineerhelper n (q+1)
sqrtlineer :: Integer -> Integer
sqrtlineer n = sqrtlineerhelper n 0

   {- Exercise 16:
split 8616460799 (90000,100000)
(90000,95000)
(0.00 secs, 68,704 bytes)

split 1963272347809 (1000000,2000000)
(1000000,1500000)
(0.01 secs, 71,648 bytes)
 -}
 
split:: Integer -> (Integer,Integer) -> (Integer,Integer )
split n (l,r)| (l+r)*(l+r) > 4*n = (l, ((l+r)`div` 2 ))
           | otherwise = (((l+r)`div` 2 ),r)
 
sqrtBinsearchHelper :: Integer -> (Integer,Integer ) -> Integer 
sqrtBinsearchHelper n (l,k) | k-l<=1 =l
 --                           | (l+k)*(l+k) > 4*n = sqrtBinsearchHelper n (split n (l,k))
                            | otherwise = sqrtBinsearchHelper n (split n (l,k))
                            
sqrtBinsearch :: Integer -> Integer               
sqrtBinsearch n = sqrtBinsearchHelper n (1,n)

   {- Exercise 17:
sqrtBinsearch 8616460799
92824
(0.01 secs, 99,376 bytes)

sqrtBinsearch 1963272347809
1401168
(0.01 secs, 110,560 bytes)

sqrtBinsearch 123456789^123456
------ (too long number)
(0.74 secs, 378,570,760 bytes)

It takes approximately log n steps. Because in each step interval become half size and the initial size is n. And when size of interval less or equal to 1 it stops.
 -}
 
sqrtOpenBinsearchBoundHelper n b | b*b>n = b  
                                 | otherwise = sqrtOpenBinsearchBoundHelper n (2*b)
                                                            
sqrtOpenBinsearchBound n = sqrtOpenBinsearchBoundHelper n 1

sqrtOpenBinsearch n = sqrtBinsearchHelper n (1,(sqrtOpenBinsearchBound n))

   {- Exercise 18:
sqrtOpenBinsearch 8616460799
92824
(0.01 secs, 87,224 bytes)

sqrtOpenBinsearch 1963272347809
1401168
(0.01 secs, 94,048 bytes)

sqrtOpenBinsearch 123456789^123456
------ (too long number)
0.69 secs, 378,561,968 bytes)



It takes approximately log b steps to search, and approximately log b steps to find upper bound where b is upper bound. And 2*sqrt(n) => b > sqrt(n). 

We have 2*log b= log(b*b)>log(n) steps. Therefore, it does not worth the extra effort.  

 -}

