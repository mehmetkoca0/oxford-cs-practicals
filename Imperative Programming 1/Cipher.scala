object Cipher{
  /** Bit-wise exclusive-or of two characters */
  def xor(a: Char, b: Char) : Char = (a.toInt ^ b.toInt).toChar

  /** Print ciphertext in octal */
  def showCipher(cipher: Array[Char]) =
    for(c <- cipher){ print(c/64); print(c%64/8); print(c%8); print(" ") }

  /** Read file into array */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray

  /** Read from stdin in a similar manner */
  def readStdin() = scala.io.Source.stdin.toArray

  /* ----- Functions below here need to be implemented ----- */

  /** Encrypt plain using key; can also be used for decryption */
  def encrypt(key: Array[Char], plain: Array[Char]) : Array[Char] ={
      val lenghtText= plain.size
      val lenghtKey= key.size
      var cipher : Array[Char] = new Array[Char](plain.size)
      var i=0
      while (i<lenghtText){
      // I: 0 <= i <= lenghtText and for all 0 <= x < i : cipher(x)=xor(plain(x),k(x%lenghtKey)) 
      // Variant is (lenghtText - i) as lenghtText constant and i decreases 1 in each iteration
        cipher(i)=xor(plain(i),key(i % lenghtKey))
        i+=1
      }
      cipher



  }

  /** Try to decrypt ciphertext, using crib as a crib */
  def tryCrib(crib: Array[Char], ciphertext: Array[Char]) : Unit ={
    val lenghtCrib : Int= crib.size
    val lenghtCiphertext : Int= ciphertext.size
    var start : Int = 0
    var found : Boolean= true
    var lenghtKey:Int= -1
    var startingIndex:Int = -1
    var keyChars : Array[Char]= new Array[Char](lenghtCrib)
    while (start<lenghtCiphertext-lenghtCrib && found){
      // I: 
      keyChars=new Array[Char](lenghtCrib)
      var i : Int = 0
      while (i<lenghtCrib) {
        // I : for every 0 <= x <i keychars(i)=xor(ciphertext(start+i),crib(i)) and 0<= i <= lenghtCrib
        keyChars(i)=xor(ciphertext(start+i),crib(i))
        i+=1
      }
      var j:Int=1
      while(j<lenghtCrib-1 && found ) {
          if (tryIndex(keyChars,j)){
            lenghtKey=j
            found=false
            startingIndex = start
          }
          j+=1

      }
      start+=1
    }
    var actualKey : Array[Char]= new Array[Char](lenghtKey)
    var k=0
    while(k<lenghtKey){
      actualKey(k)=keyChars( (lenghtKey-(startingIndex % lenghtKey) +k)%lenghtKey )
      k+=1
    }
    println(actualKey.mkString(""))
    println(encrypt(actualKey,ciphertext).mkString(""))
  }

  def tryIndex(keyChars: Array[Char],j: Int): Boolean = {
    val LenghtkeyChars : Int= keyChars.size
    require(LenghtkeyChars>=j)
    var i: Int=0
    var flag: Boolean= true
    while(i<LenghtkeyChars-j && flag){
      // if for all 0<=x <i : keyChars(i)== keyChars(i+j) then flag=true otherwise false and 0 <= i <= LenghtkeyChars-j
        flag = flag && (keyChars(i)== keyChars(i+j))
        i+=1
    }
    flag
  }

  /** The first optional statistical test, to guess the length of the key */
 
  def crackKeyLen(ciphertext: Array[Char]) : Unit = {
  	var shift: Int=1
  	while(shift<31){
	  	var count:Int =0
	  	var i:Int=0
	  	
	  	while(i+shift<ciphertext.size){
	  		if (ciphertext(i) == ciphertext(shift+i)) {count+=1}
	  		i+=1
	  	}
	  	println(shift+": "+count)
	  	shift+=1
  	}
  
  
  
  }


  /** The second optional statistical test, to guess characters of the key. */
  def crackKey(klen: Int, ciphertext: Array[Char]) : Unit = {
  	var shift :Int =klen
  	while(shift<ciphertext.size){
  		var i: Int=0
  		while (i+shift<ciphertext.size){
  			if (ciphertext(i) == ciphertext(shift+i)){
  			if (31<xor(' ', ciphertext(i)).toInt && xor(' ', ciphertext(i)).toInt<127 )
  			println( (i%klen) +" " +xor(' ', ciphertext(i)))
  			
  			}
  		i+=1
  		}
  	shift+=klen
  	
  	}
  
  
  
  }

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {
    // string to print if error occurs
    val errString = 
      "Usage: scala Cipher (-encrypt|-decrypt) key [file]\n"+
      "     | scala Cipher -crib crib [file]\n"+
      "     | scala Cipher -crackKeyLen [file]\n"+
      "     | scala Cipher -crackKey len [file]"

    // Get the plaintext, either from the file whose name appears in position
    // pos, or from standard input
    def getPlain(pos: Int) = 
      if(args.length==pos+1) readFile(args(pos)) else readStdin()

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
    val command = args(0)
    if(command=="-encrypt" || command=="-decrypt"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      print(new String (encrypt(key,plain)))
    }
    else if(command=="-crib"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      tryCrib(key, plain)
    }
    else if(command=="-crackKeyLen"){
      checkNumArgs(1); val plain = getPlain(1)
      crackKeyLen(plain)
    }      
    else if(command=="-crackKey"){
      checkNumArgs(2); val klen = args(1).toInt; val plain = getPlain(2)
      crackKey(klen, plain)
    }
    else println(errString)
  }
}
