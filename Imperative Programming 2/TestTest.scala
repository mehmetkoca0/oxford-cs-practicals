/**  With Scala 2.13 on Lab machines:

 * In normal circumstances the CLASSPATH is already set for you:

fsc TestTest.scala
scala org.scalatest.run TestTest

 * If you use jar files in your own space:

   With Scala 2.12 and ScalaTest 3.2.2:
fsc -cp ./scalatest-app_2.12-3.2.2.jar TestTest.scala
scala -cp ./scalatest-app_2.12-3.2.2.jar org.scalatest.run TestTest
 * (Once this is working you can set your CLASSPATH in .bashrc) 

*/
import org.scalatest.funsuite.AnyFunSuite
// or import org.scalatest.FunSuite with
// ScalaTest 3.0 or earlier
import IntSet._

class TestTest extends AnyFunSuite{ // FunSuite in ScalaTest 3.0
  var set1 = IntSet()
  var set2 = IntSet(1)
  var set3 = IntSet(1,2,3)
  var set4 = IntSet(1,2,4)
  var set5 = IntSet(3,2,1)
  var set6 = IntSet(1,2)
  var set7 = IntSet()
  var set8 = IntSet(1,2,3,4)
  var set9 = IntSet(1,1,1)
  var set10 = IntSet(1,4,9)

  // Tests toString (indeed also  add ) 
  test("{} to string "){ assert(set1.toString==="{}") }
  test("{1} to string "){ assert(set2.toString==="{1}") }
  test("{1,2,3} to string "){ assert(set3.toString==="{1, 2, 3}") }
  test("{1,1,1} to string "){ assert(set9.toString==="{1}") }
  // Tests contains
  test("{} contains 1 "){ assert(set1.contains(1)===false) }
  test("{1} contains 1 "){ assert(set2.contains(1)===true) }
  test("{1} contains 2 "){ assert(set2.contains(2)===false) }
  test("{1,2,3} contains 2 "){ assert(set3.contains(2)===true) }
  test("{1,2,3} contains 5 "){ assert(set3.contains(5)===false) }
  test("{1,2,3} contains 1 "){ assert(set3.contains(1)===true) }
  test("{1,2,3} contains -1 "){ assert(set3.contains(-1)===false) }
  test("{1,2,4} contains 3 "){ assert(set4.contains(3)===false) }
  test("{1,2,3} contains 3 "){ assert(set3.contains(3)===true) }

  // Tests size 
  test("{} has size 0 "){ assert(set1.size===0) }
  test("{1} has size 1 "){ assert(set2.size===1) }
  test("{1,2,3} has size 3 "){ assert(set3.size===3) }

  // Tests equals
    test("{} == {} "){ assert(set1==set1) }
    test("{} != {1} "){ assert(set1!=set2) }
    test("{1,2,3} != {1,2,4} "){ assert(set3!=set4) }
    test("{1,2,3} == {3,2,1} "){ assert(set3==set5) }
    test("{1,2,4} == {1,2,4} "){ assert(set4==set4) }

  // Tests remove
  
  test("{}.remove(1) == {} "){ set1.remove(1); assert(set1==set7) }
  test("{1,2,3}.remove(3) == {1,2} "){set3.remove(3); assert(set3==set6); set3.add(3) }
  test("{1,2,4}.remove(4) == {1,2} "){set4.remove(4); assert(set4==set6); set4.add(4) }
  test("{3,2,1}.remove(3) == {1,2} "){ set5.remove(3); assert(set5==set6); set5.add(3) }

  // Test subsetOf
  test("is {} subset of {1} "){ assert(set1.subsetOf(set2)) }
  test("is {1} subset of {} "){ assert( ! set2.subsetOf(set1)) }
  test("is {1,2} subset of {1,2,3} "){ assert(set6.subsetOf(set3)) }
  test("is {1,2,3} subset of {1,2} "){ assert(! set3.subsetOf(set6)) }
  test("is {1,2} subset of {1,2,4} "){ assert(set6.subsetOf(set4)) }
  test("is {1,2,3} subset of {1,2,4} "){  assert(!set3.subsetOf(set4)) }
  test("is {1,2,4} subset of {1,2,3} "){ assert(!set4.subsetOf(set3)) }
  test("is {1,2,3} subset of {3,2,1} "){ assert(set3.subsetOf(set5)) }

  // Test Union
    test("is {} union {1,2,3} equal to {1,2,3} "){ assert(set1.union(set3)==set3) }
    test("is {1,2} union {1,2,3} equal to {1,2,3} "){ assert(set6.union(set3)==set3) }
    test("is {1,2,4} union {1,2,3} equal to {1,2,3,4} "){ assert(set4.union(set3)==set8) }

  // Test intersect
  test("is {} intersect {1,2,3} equal to {} "){ assert(set1.intersect(set3)==set1) }
  test("is {1,2,4} intersect {1,2,3} equal to {1,2} "){ assert(set4.intersect(set3)==set6) }
  test("is {1,2,3} intersect {3,2,1} equal to {1,2,3} "){ assert(set3.intersect(set5)==set3) }

  // Test Map
  def f (x:Int)= {
    x*x
  }
  test("is map f {} equal to {} "){  assert(set1.map(f)==set1) }
  test("is map f {1,2,3} equal to {1,4,9} "){  assert(set3.map(f)==set10) }


  // Test Filter 
  def p (x:Int):Boolean={
    !(x%2==0)
  }

  test("is p {} equal to {} "){  assert(set1.filter(p)==set1) }
  test("is p {1,2,3} equal to {1,3} "){  assert(set3.filter(p)==IntSet(1,3)) }











  








}


/*
//  Corrected:
class TestTest extends AnyFunSuite{
  var x = 0
  test("x=0"){ 
    x=0
    assert(x===0) 
  }
  test("x=1"){ 
    x=1
    assert(x===1)
  }
}
*/  
