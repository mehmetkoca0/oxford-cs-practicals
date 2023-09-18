// A class of objects to represent a set

class IntSet{
  // State: S : P(Int) (where "P" represents power set)

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  private type Node = IntSet.Node
  // Constructor
  private def Node(datum: Int, next: Node) = new IntSet.Node(datum, next)
  // I will use an ordered linked list with dummy header and without repetitions because dummy header is needed for removing first element
  // and oredering linked list makes easier to compare two sets and avoid repetitions
  // Data type invariant: for each node n , n.datum< n.next.datum
  // Abstraction function: set={n.datum | n <- L(theSet.next)} 
  // Init: S = {}
  private var theSet : Node = Node(-100,null) // or however empty set is represented
  private var length : Int=0
  /** Convert the set to a string.
    * (The given implementation is not sufficient.) */
  override def toString : String = {
    var res : String="{"
    var n :Node= theSet.next
    while(n!=null){
        res+=n.datum
        if (n.next!=null) res+=", "
        n=n.next
    }
    res+="}"
    res
  }
    // if such n exist return n s.t  n.datum<e<n.next.datum (suppose null.datum infinity and dummy node has value negative infinity)
    // Post: S=S_0 and returns such n
  def find(e:Int):Node={
    var n : Node= theSet
    while(n.next!=null && n.next.datum<e) n=n.next
    n

    
  }

  /** Add element e to the set
    * Post: S = S_0 U {e} */
  def add(e: Int) : Unit ={
    var n:Node =this.find(e)
    if(n.next!=null && e==n.next.datum) {}
    else {
        n.next=Node(e,n.next)
        length+=1
    }

  }

  /** Length of the list
    * Post: S = S_0 && returns #S */
  def size : Int ={
    length
  }

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S) */
  def contains(e: Int) : Boolean = {
    var n : Node= (this.find(e)).next
    if(n==null) false
    else (e==n.datum)


  }

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
  def any : Int = {
    var n :Node = theSet.next
    require(n!=null)
    n.datum

  }

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  override def equals(that: Any) : Boolean = that match {
    case s: IntSet => {
        var flag: Boolean=true
        var n1: Node= theSet.next
        var n2 : Node= s.theSet.next
        if (s.size!=this.size) flag=false

        while(flag && n1 !=null && n2 != null ){
            if (n1.datum != n2.datum) flag=false

            n1=n1.next
            n2=n2.next
        }
        flag
    }
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  def remove(e: Int) : Boolean = {
    var n : Node= this.find(e)
    if (n.next== null || n.next.datum!=e) false
    else {
        n.next=n.next.next
        length-=1
        true
    }

  }
    
  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
  def subsetOf(that: IntSet) : Boolean = {
    var n : Node= this.theSet.next
    var flag: Boolean=true
    while(n!=null && flag){
        if (that.contains(n.datum)) n=n.next
        else flag=false
    }
    flag
  }

  // ----- optional parts below here -----

  /** return union of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  def union(that: IntSet) : IntSet = {
    var unionSet :IntSet= new IntSet()

    var n1: Node = this.theSet.next
    while(n1 != null ){
        unionSet.add(n1.datum)
        n1=n1.next
    }

    var n2: Node = that.theSet.next
    while(n2 != null ){
        unionSet.add(n2.datum)
        n2=n2.next
    }

    unionSet

  }

  /** return intersection of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  def intersect(that: IntSet) : IntSet = {
    var n: Node = this.theSet.next
    var intersectionSet :IntSet= new IntSet()
    while(n != null ){
        if (that.contains(n.datum)){
            intersectionSet.add(n.datum)
        }
        n=n.next
    }
    intersectionSet

  }

  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
  def map(f: Int => Int) : IntSet = {
    var n: Node = this.theSet.next
    var mapSet :IntSet= new IntSet()
    while(n != null ){
        mapSet.add(f(n.datum))
        n=n.next
    }
    mapSet

  }

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  def filter(p : Int => Boolean) : IntSet ={
    var n: Node = this.theSet.next
    var filteredSet :IntSet= new IntSet()
    while(n != null ){
        if (p(n.datum)){
            filteredSet.add(n.datum)
        }
        n=n.next
    }
    filteredSet


  }
}


// The companion object
object IntSet{
  /** The type of nodes defined in the linked list */
  private class Node(var datum: Int, var next: Node)

  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined 
    * the main constructor and the add operation. 
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply(xs: Int*) : IntSet = {
    val s = new IntSet; for(x <- xs) s.add(x); s
  }
}