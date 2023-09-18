/** Import is for readLine so that we can write input directly to the program */
import scala.io.StdIn

object Brack{
	//Maximum length of word so we can define our arrays in dynamic programming
	val MAXWORD = 1000

	//Operation to take 'A', 'B' and 'C' to corresponding Ints
  def LetterToInt(a: Char) : Int = {
		if(a == 'A' || a == 'B' || a == 'C'){
			return (a.toInt - 'A'.toInt);
		} else{
			println("Please only Letters from A,B,C.")
			sys.exit
		}
	}
	
  //Defining the op array for everything to use
  val op = Array.ofDim[Int](3,3)  
  	op(0)(0) = 1; op(0)(1) = 1; op(0)(2) = 0
	op(1)(0) = 2; op(1)(1) = 1; op(1)(2) = 0
	op(2)(0) = 0; op(2)(1) = 2; op(2)(2) = 2

  /** Read file into array (discarding the EOF character) */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray.init

 
  /* Functions below here need to be implemented */


	//TASK 1
	//PossibleRec checks whether bracketing to something is possible recursively
	//Checks whether w(i,j) can be bracketed to z
	
	def PossibleRec(w: Array[Int], i: Int, j: Int, z:Int): Boolean = {
	val word: Array[Int]= w.slice(i,j)
	if (word.length==0) {false}
	if ( word.length ==1) {z==word(0)}
	else{
	var k =1
	var found=false
	while (!found && k< word.length){
	if (z==0){
	found= (PossibleRec(word,0,k,2) && PossibleRec(word,k,word.length,0)) || (PossibleRec(word,0,k,0) && PossibleRec(word,k,word.length,2)) || (PossibleRec(word,0,k,1) && PossibleRec(word,k,word.length,2)) 
	}
	else if (z==1){
	found = (PossibleRec(word,0,k,0) && PossibleRec(word,k,word.length,0)) || (PossibleRec(word,0,k,1) && PossibleRec(word,k,word.length,1)) || (PossibleRec(word,0,k,0) && PossibleRec(word,k,word.length,1)) 
	}
	else {
	found= (PossibleRec(word,0,k,1) && PossibleRec(word,k,word.length,0)) || (PossibleRec(word,0,k,2) && PossibleRec(word,k,word.length,1)) || (PossibleRec(word,0,k,2) && PossibleRec(word,k,word.length,2)) 
	}
	k+=1
	}
	found
	
	}
	
	//TODO
	} 

	
	//TASK 2
	//NumberRec which checks the ways you get a result recursively
	//Computes number of ways w(i,j) can be bracketed to get z
	
	def NumberRec(w: Array[Int], i: Int, j: Int, z:Int): Int = {
	val word: Array[Int]= w.slice(i,j)
	if (word.length==0) {0}
	if ( word.length ==1) {
	if (z==word(0)) 1
	else 0
	}
	else{
	var k =1
	var result=0
	while (k< word.length){
	if (z==0){
	result+=NumberRec(word,0,k,2)* NumberRec(word,k,word.length,0) + NumberRec(word,0,k,0)* NumberRec(word,k,word.length,2)+ NumberRec(word,0,k,1)* NumberRec(word,k,word.length,2)
	}
	else if (z==1){
	result+=NumberRec(word,0,k,0)* NumberRec(word,k,word.length,0) + NumberRec(word,0,k,1)* NumberRec(word,k,word.length,1)+ NumberRec(word,0,k,0)* NumberRec(word,k,word.length,1)
	}
	else {
	result+=NumberRec(word,0,k,1)* NumberRec(word,k,word.length,0) + NumberRec(word,0,k,2)* NumberRec(word,k,word.length,1)+ NumberRec(word,0,k,2)* NumberRec(word,k,word.length,2)
	}
	k+=1
	}
	result
	
	}
	//TODO
	} 

	
	//TASK 3
	//TODO Runtime analysis of recursive solution along with tests
	// In the worst case PossibleRec and NumberRec are same 
	// Let say T(n) is the running time of NumberRec where n is the length of input . T(n)=3*( sum over k from 1 to n-1 ( T(k)+T(n-k)))=6* ( sum over k from 1 to n-1 ( T(k))) 
	// T(1)=1 . T(2)= 6 T(3)=42 .. so T(n)= 6*7^(n-2) for n>1. So they are exponantial.
	// for ABBA 0.592s
	// For ABBACAB 0.607S
	// For ABBACABABAC  7.857s
	// For ABBACABABACA 51.710ss

	
	
	
	//You may find the following class useful for Task 7
	// Binary tree class
	abstract class BinaryTree
	case class Node (left : BinaryTree, right : BinaryTree) extends BinaryTree
	case class Leaf (value : Char) extends BinaryTree

	//Printing for a binary tree
	def print_tree(t : BinaryTree) {
		t match{ 

		case node:Node => {	
		print("(")
		print_tree(node.left)
		print(")")

		print("(")
		print_tree(node.right)
		print(")")
		}
		case leaf:Leaf => {	
			print(leaf.value)
		}
		case _ => {	
		}

		}

	//TODO(optional)
	}

	//These arrays should hold the relevant data for dynamic programming
	var poss = Array.ofDim[Boolean](MAXWORD, MAXWORD, 3)
	var ways = Array.ofDim[Int](MAXWORD, MAXWORD, 3)
	var exp = Array.ofDim[BinaryTree](MAXWORD, MAXWORD, 3)


	//Task 4, 5, and 7(optional)
	//TODO Fill out arrays with dynamic programming solution
	
	
	def Tabulate(w: Array[Int], n: Int): Unit = {
	for (z <- 0 to 2 ){
		for (i <-0 to (n-1)){
			poss(i)(i+1)(z)= (w(i)==z)
			}
	}
	for (z <- 0 to 2 ){
		for (i <-0 to (n-1)){
			if (w(i)==z) {
				ways(i)(i+1)(z) +=1
				if (z==0){
					exp(i)(i+1)(z)=Leaf('A')
				}
				else if (z==1){
					exp(i)(i+1)(z)=Leaf('B')
				}
				else if (z==2){
					exp(i)(i+1)(z)=Leaf('C')
				}

			}
			}
	}
	for (m <- 2 to n ) {
		for (i <- 0 to (n-m) ) {
			var found0=false 
			var found1=false 
			var found2=false 
			for (k <- (i+1) to (i+m-1)){
				found0= found0 || ( poss(i)(k)(2) && poss(k)(i+m)(0) ) || ( poss(i)(k)(0) && poss(k)(i+m)(2) ) || ( poss(i)(k)(1) && poss(k)(i+m)(2) )
				ways(i)(i+m)(0) += ( ways(i)(k)(2) * ways(k)(i+m)(0) ) + ( ways(i)(k)(0) * ways(k)(i+m)(2) ) + (ways(i)(k)(1) * ways(k)(i+m)(2))
				found1= found1 || ( poss(i)(k)(0) && poss(k)(i+m)(0) ) || ( poss(i)(k)(0) && poss(k)(i+m)(1) ) || ( poss(i)(k)(1) && poss(k)(i+m)(1) )
				ways(i)(i+m)(1) += ( ways(i)(k)(0) * ways(k)(i+m)(0) ) + ( ways(i)(k)(0) * ways(k)(i+m)(1) ) + (ways(i)(k)(1) * ways(k)(i+m)(1))
				found2= found2 || ( poss(i)(k)(1) && poss(k)(i+m)(0) ) || ( poss(i)(k)(2) && poss(k)(i+m)(1) ) || ( poss(i)(k)(2) && poss(k)(i+m)(2) )
				ways(i)(i+m)(2) += ( ways(i)(k)(1) * ways(k)(i+m)(0) ) + ( ways(i)(k)(2) * ways(k)(i+m)(1) ) + (ways(i)(k)(2) * ways(k)(i+m)(2))

				if ( poss(i)(k)(2) && poss(k)(i+m)(0) ) exp(i)(i+m)(0)= Node(exp(i)(k)(2),exp(k)(i+m)(0))
				if ( poss(i)(k)(0) && poss(k)(i+m)(2) ) exp(i)(i+m)(0)= Node(exp(i)(k)(0),exp(k)(i+m)(2))
				if ( poss(i)(k)(1) && poss(k)(i+m)(2) ) exp(i)(i+m)(0)= Node(exp(i)(k)(1),exp(k)(i+m)(2))
				if ( poss(i)(k)(0) && poss(k)(i+m)(0) )	exp(i)(i+m)(1)= Node(exp(i)(k)(0),exp(k)(i+m)(0))
				if ( poss(i)(k)(0) && poss(k)(i+m)(1) )	exp(i)(i+m)(1)= Node(exp(i)(k)(0),exp(k)(i+m)(1))
				if ( poss(i)(k)(1) && poss(k)(i+m)(1) )	exp(i)(i+m)(1)= Node(exp(i)(k)(1),exp(k)(i+m)(1))
				if ( poss(i)(k)(1) && poss(k)(i+m)(0) )	exp(i)(i+m)(2)= Node(exp(i)(k)(1),exp(k)(i+m)(0))
				if ( poss(i)(k)(2) && poss(k)(i+m)(1) )	exp(i)(i+m)(2)= Node(exp(i)(k)(2),exp(k)(i+m)(1))
				if ( poss(i)(k)(2) && poss(k)(i+m)(2) )	exp(i)(i+m)(2)= Node(exp(i)(k)(2),exp(k)(i+m)(2))

				






			}

			poss(i)(i+m)(0)=found0
			poss(i)(i+m)(1)=found1
			poss(i)(i+m)(2)=found2
		}
	}

	//TODO
	}

	//Task 6
	//TODO Runtime analysis of dynamic programming version with tests
	// The maximum word length that program works is MAXWORD=30 because our array store at most 30 indexs in its first two argument
	// Tabulate is O(n^3) because there is 3 nested loops with at most n iteration each.
	// For ABBACABABACA: 1.146 s
	// For ABBACABABACAABBACABABACA : 1.150 s
	// For a word lenght 385 : 2.227 s
	// For a word length 770 : 6.625 s
	// For a word length 995 : 13.385  s

  

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {

    // string to print if error occurs
    val errString = 
      "Usage: scala Brack -PossibleRec [file]\n"+
      "     | scala Brack -NumberRec [file]\n"+
      "     | scala Brack -Tabulate [file]\n"
		
		if (args.length > 2){
			println(errString)
			sys.exit
		}

    //Get the plaintext, either from the file whose name appears in position
    //pos, or from standard input
    def getPlain(pos: Int) = 
      if(args.length==pos+1) readFile(args(pos)) else StdIn.readLine.toArray

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
		val plain = getPlain(1)
    val command = args(0)

		//Making sure the letters are of the right type
		val len = plain.length
		var plainInt = new Array[Int](len)
		if (len > MAXWORD){
			println("Word Too Long! Change MAXWORD")
			sys.exit;
		} else {
    	for (i <- 0 until len){
				plainInt(i) = LetterToInt(plain(i))
			}
		}
		
		//Executing appropriate command
    if(command=="-PossibleRec"){
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			if(PossibleRec(plainInt, 0, len, i)){
				println(('A'.toInt + i).toChar + " is Possible");
			}
			else{
				println(('A'.toInt + i).toChar + " is not Possible");
			}
		}
    }
    else if(command=="-NumberRec"){
		var z: Int = 0
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			z = NumberRec(plainInt, 0, len, i)
			if(z == 1){
				printf(('A'.toInt + i).toChar+ " can be achieved in %d way\n", z)
			}
			else{
				printf(('A'.toInt + i).toChar+ " can be achieved in %d ways\n", z)
			}
		}
    }

    else if(command=="-Tabulate"){
		Tabulate(plainInt,len)
		println("Bracketing values for "+ plain.mkString(""))
		for(v<-0 to 2){
		var z: Int = ways(0)(len)(v)
			if(z==0){
			println(('A'.toInt + v).toChar+ " cannot be achieved")
			}
			else if(z==1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d way\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
			else if (z > 1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d ways\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
		}
    }      
    else println(errString)
  }
}


